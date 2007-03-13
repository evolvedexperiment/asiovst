unit AnalyserForm;

{$IFDEF FPC}
{$MODE Delphi}
{$ENDIF}

interface

uses {$IFDEF FPC} LCLIntf, LResources, TAGraph,
     {$ELSE} Windows, TeeProcs, TeEngine, Series, {$ENDIF}
     SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, DDspBase,
     DFilter, Chart, Spin, Buttons, DASIOHost;

const
  cNumFrequencies = 32;
  cThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);

type

  { TFmAnalyser }

  TFmAnalyser = class(TForm)
    Bt_CP: TButton;
    Bt_Analyse: TButton;
    DriverCombo: TComboBox;
    ChannelBox: TComboBox;
    ASIOHost: TASIOHost;
    Lb_Drivername: TLabel;
    Lb_Channels: TLabel;
    AnalyserChart: TChart;
    BarSeries: TBarSeries;
    LbSpeed: TLabel;
    RB_Fast: TRadioButton;
    RB_Medium: TRadioButton;
    RB_Slow: TRadioButton;
    LbFullscale: TLabel;
    SEFullscaleGain: TSpinEdit;
    Lb_dB: TLabel;
    procedure AnalyserChartClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure Bt_AnalyseClick(Sender: TObject);
    procedure ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleDynArray);
    procedure RB_MediumClick(Sender: TObject);
    procedure RB_FastClick(Sender: TObject);
    procedure RB_SlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
  private
    fFilterArray : Array [0..cNumFrequencies-1] of TSimpleBandpass;
    fFilterRMS   : Array [0..cNumFrequencies-1] of Single;
    fChannelNr   : Integer;
    fSpeedConst  : Array [0..1] of Single;
    fFSGain      : Single;
    procedure UpdateBarGraph;
  public
  published
  end;

var
  FmAnalyser    : TFmAnalyser;

implementation

{$IFDEF FPC}
{$R *.dfm}
{$ENDIF}

uses Inifiles, Registry;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var i : Integer;
begin
 fChannelNr:=0;
 fSpeedConst[0]:=0.999; fSpeedConst[1]:=1-fSpeedConst[0];
 fFSGain:=SEFullscaleGain.Value;
 DriverCombo.Items := ASIOHost.DriverList;
 if DriverCombo.Items.Count = 0 then
  try
   raise Exception.Create('No ASIO Driver present! Application Terminated!');
  except
   Application.Terminate;
  end;

 // and make sure all controls are enabled or disabled
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   Left := ReadInteger('Layout', 'Audio Left', Left);
   Top := ReadInteger('Layout', 'Audio Top', Top);
   DriverCombo.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
   if DriverCombo.ItemIndex >= 0 then DriverComboChange(DriverCombo);
   ChannelBox.ItemIndex := ReadInteger('Audio', 'Channels', 0);
  finally
   Free;
  end;

 for i:=0 to cNumFrequencies-1 do
  begin
   fFilterArray[i]:=TSimpleBandpass.Create;
   with fFilterArray[i] do
    begin
     SampleRate:=44100;
     Bandwidth:=1;
     Frequency:=cThirdOctaveFrequencies[i];
     if Frequency<1000
      then BarSeries.Add(0,FloatToStr(Frequency)+' Hz')
      else BarSeries.Add(0,FloatToStr(0.001*Frequency)+' kHz');
    end;
  end;
end;

procedure TFmAnalyser.AnalyserChartClick(Sender: TObject);
begin

end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
var i : Integer;
begin
 ASIOHost.Active:=False;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
  finally
   Free;
  end;
 for i := 0 to cNumFrequencies-1 do fFilterArray[i].Free;
end;

procedure TFmAnalyser.RB_FastClick(Sender: TObject);
begin
 fSpeedConst[0]:=0.99; fSpeedConst[1]:=1-fSpeedConst[0];
end;

procedure TFmAnalyser.RB_MediumClick(Sender: TObject);
begin
 fSpeedConst[0]:=0.999; fSpeedConst[1]:=1-fSpeedConst[0];
end;

procedure TFmAnalyser.RB_SlowClick(Sender: TObject);
begin
 fSpeedConst[0]:=0.9999; fSpeedConst[1]:=1-fSpeedConst[0];
end;

procedure TFmAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 fFSGain:=SEFullscaleGain.Value;
 AnalyserChart.LeftAxis.Maximum:=fFSGain+20;
end;

procedure TFmAnalyser.DriverComboChange(Sender: TObject);
var i        : Integer;
begin
 Bt_CP.Enabled := False;
 Bt_Analyse.Enabled := False;
 DriverCombo.ItemIndex := DriverCombo.Items.IndexOf(DriverCombo.Text);
 if DriverCombo.ItemIndex >= 0 then
  begin
   ASIOHost.DriverIndex := DriverCombo.ItemIndex;
   ChannelBox.Clear;
   for i := 0 to ASIOHost.InputChannelCount - 1
    do ChannelBox.Items.Add(ASIOHost.InputChannelInfos[i].name);
   with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
     WriteInteger('Audio', 'Asio Driver', DriverCombo.ItemIndex);
    finally
     Free;
    end;
   Bt_CP.Enabled := True;
   Bt_Analyse.Enabled := True;
   ChannelBox.ItemIndex := 0;
  end;
end;

procedure TFmAnalyser.Bt_CPClick(Sender: TObject);
begin
 ASIOHost.ControlPanel;
end;

procedure TFmAnalyser.Bt_AnalyseClick(Sender: TObject);
begin
 if Bt_Analyse.Caption = 'Analyse' then
  begin
   ASIOHost.Active := True; // Start Audio
   Bt_Analyse.Caption := 'Stop';
  end
 else
  begin
   ASIOHost.Active := False; // Stop Audio
   Bt_Analyse.Caption := 'Analyse';
  end;
end;

procedure TFmAnalyser.UpdateBarGraph;
var j : Integer;
begin
 for j := 0 to cNumFrequencies-1
  do BarSeries.YValue[j]:=fFilterRMS[j]+ fFSGain;
 AnalyserChart.Invalidate;
end;

procedure TFmAnalyser.ASIOHostBufferSwitch(Sender: TObject; InBuffer, OutBuffer: TArrayOfSingleDynArray);
var i,j : Integer;
    s   : Single;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   s:=InBuffer[fChannelNr,i];
   for j := 0 to cNumFrequencies-1
    do fFilterRMS[j]:=fSpeedConst[0]*fFilterRMS[j]+fSpeedConst[1]*Amp_to_dB(f_abs(fFilterArray[j].Process(s+1E-24)));
  end;
 UpdateBarGraph;
end;

{$IFDEF FPC}
initialization
  {$i AnalyserForm.lrs}
  {$i AnalyserForm.lrs}
{$ENDIF}

end.
