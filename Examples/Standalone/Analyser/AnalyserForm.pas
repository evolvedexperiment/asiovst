unit AnalyserForm;

{$I DAV_Compiler.INC}

interface

uses
  {$IFDEF FPC} LCLIntf, LResources, TAGraph,
  {$ELSE} Windows, TeeProcs, TeEngine, Series, {$ENDIF}
  SysUtils, Classes, Controls, Forms, StdCtrls, ComCtrls, DAV_Common, Chart,
  Spin, Buttons, ExtCtrls, DAV_DspFilter, DAV_DspFilterBasics, DAV_ASIOHost;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies : Array [0..CNumFrequencies - 1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);

type

  { TFmAnalyser }

  TFmAnalyser = class(TForm)
    AnalyserChart: TChart;
    ASIOHost: TASIOHost;
    BarSeries: TBarSeries;
    Bt_Analyse: TButton;
    Bt_CP: TButton;
    ChannelBox: TComboBox;
    DriverCombo: TComboBox;
    Lb_Channels: TLabel;
    Lb_dB: TLabel;
    Lb_Drivername: TLabel;
    LbFullscale: TLabel;
    LbSpeed: TLabel;
    RB_Fast: TRadioButton;
    RB_Medium: TRadioButton;
    RB_Slow: TRadioButton;
    SEFullscaleGain: TSpinEdit;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ASIOHostBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure Bt_AnalyseClick(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure RB_FastClick(Sender: TObject);
    procedure RB_MediumClick(Sender: TObject);
    procedure RB_SlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
  private
    FFilterArray : Array [0..CNumFrequencies - 1] of TBasicBandpassFilter;
    FFilterRMS   : Array [0..CNumFrequencies - 1] of Single;
    FChannelNr   : Integer;
    FSpeedConst  : TDAV2SingleArray;
    FFSGain      : Single;
    procedure UpdateBarGraph;
  public
  published
  end;

var
  FmAnalyser    : TFmAnalyser;

implementation

{$IFNDEF FPC}
{$R *.dfm}
{$ENDIF}

uses
  Inifiles;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var
  i : Integer;
begin
 FChannelNr := 0;
 FSpeedConst[0] := 0.999;
 FSpeedConst[1] := 1 - FSpeedConst[0];
 FFSGain        := SEFullscaleGain.Value;
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

 for i := 0 to cNumFrequencies - 1 do
  begin
   FFilterArray[i] := TBasicBandpassFilter.Create;
   with FFilterArray[i] do
    begin
     SampleRate := 44100;
     Gain := 0;
     Bandwidth := 1;
     Frequency := cThirdOctaveFrequencies[i];
     {$IFNDEF FPC}
     if Frequency < 1000
      then BarSeries.Add(0,FloatToStr(Frequency) + ' Hz')
      else BarSeries.Add(0,FloatToStr(0.001 * Frequency) + ' kHz');
     {$ELSE}
     if Frequency < 1000
      then AnalyserChart.AddBar(FloatToStr(Frequency) + ' Hz', 0, $000000FF)
      else AnalyserChart.AddBar(FloatToStr(0.001 * Frequency) + ' kHz', 0, $000000FF);
     {$ENDIF}
    end;
  end;
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 ASIOHost.Active := False;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
  finally
   Free;
  end;
 for i := 0 to cNumFrequencies - 1 do FreeAndNil(FFilterArray[i]);
end;

procedure TFmAnalyser.RB_FastClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.99; FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TFmAnalyser.RB_MediumClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.999; FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TFmAnalyser.RB_SlowClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.9999; FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TFmAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 FFSGain := SEFullscaleGain.Value;
 {$IFNDEF FPC}
 AnalyserChart.LeftAxis.Maximum := FFSGain+20;
 {$ENDIF}
end;

procedure TFmAnalyser.DriverComboChange(Sender: TObject);
var
  i : Integer;
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
var
  j : Integer;
begin
 {$IFNDEF FPC}
 for j := 0 to cNumFrequencies - 1
  do BarSeries.YValue[j] := FFilterRMS[j] + FFSGain;
 AnalyserChart.Invalidate;
 {$ELSE}
 for j := 0 to cNumFrequencies - 1
  do TBar(AnalyserChart.Bars.Items[j]).Value := round(FFilterRMS[j] + FFSGain);
 AnalyserChart.Invalidate;
 {$ENDIF}
end;

procedure TFmAnalyser.ASIOHostBufferSwitch(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
var
  i, j : Integer;
  s    : Single;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   s := InBuffer[FChannelNr,i];
   for j := 0 to cNumFrequencies - 1
    do FFilterRMS[j] := FSpeedConst[0] * FFilterRMS[j] + FSpeedConst[1] * Amp_to_dB(abs(FFilterArray[j].ProcessSample(s + 1E-24)));
  end;
 UpdateBarGraph;
end;

{$IFDEF FPC}
initialization
  {$i AnalyserForm.lrs}
{$ENDIF}

end.
