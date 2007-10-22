unit AnalyserForm;

interface

uses {$IFDEF FPC} LCLIntf, LResources, Buttons, {$ELSE} Windows, Messages, {$ENDIF}
     SysUtils, Classes, Graphics, Controls, Forms,
     Math, StdCtrls, ComCtrls, DASIOHost, ExtCtrls, DAVDCommon,
     Spin, DChebyshevFilter, DButterworthFilter, DBarChart;

const
  cNumFrequencies = 32;
  cThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);

type
  TComplexDouble = record
                    Re, Im : Double;
                   end;
  TFmAnalyser = class(TForm)
    Bt_CP: TButton;
    Bt_Analyse: TButton;
    DriverCombo: TComboBox;
    ChannelBox: TComboBox;
    ASIOHost: TASIOHost;
    Lb_Drivername: TLabel;
    Lb_Channels: TLabel;
    LbSpeed: TLabel;
    RB_Fast: TRadioButton;
    RB_Medium: TRadioButton;
    RB_Slow: TRadioButton;
    LbFullscale: TLabel;
    SEFullscaleGain: TSpinEdit;
    Lb_dB: TLabel;
    Timer: TTimer;
    BarChart: TFrequencyBarChart;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure Bt_AnalyseClick(Sender: TObject);
    procedure BSDownSampled(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray);
    procedure RB_MediumClick(Sender: TObject);
    procedure RB_FastClick(Sender: TObject);
    procedure RB_SlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
    procedure AnalyserChartDblClick(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    fLPFilterArray : Array [0..cNumFrequencies-1] of TChebyshev1LP;
    fHPFilterArray : Array [0..cNumFrequencies-1] of TChebyshev1HP;
    fFilterRMS     : Array [0..cNumFrequencies-1] of Single;
    fChannelNr     : Integer;
    fSpeedConst    : Array [0..1] of Single;
    fFSGain        : Single;
    fDownSampler   : Integer;
    fDownSampleMax : Integer;
  protected
  public
  published
  end;

var
  FmAnalyser    : TFmAnalyser;

implementation

{$IFNDEF FPC}
{$R *.DFM}
{$ENDIF}

uses inifiles, DASIOConvert;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var i   : Integer;
const HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
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

 fDownSampler:=0;

 for i:=0 to cNumFrequencies-1 do
  begin
   fLPFilterArray[i]:=TChebyshev1LP.Create;
   with fLPFilterArray[i] do
    begin
     SampleRate:=44100;
     SetFilterValues(0.87*(cThirdOctaveFrequencies[cNumFrequencies-i-1]*HalfThirdMulFak),0,10);
     if fDownSampler=-1 then DownsampleAmount:=0 else
      while Power(2,DownsampleAmount)*Frequency<0.2*SampleRate do DownsampleAmount:=DownsampleAmount+1;
     CalculateCoefficients;
    end;

   fHPFilterArray[i]:=TChebyshev1HP.Create;
   with fHPFilterArray[i] do
    begin
     SampleRate:=44100;
     SetFilterValues(1.149*(cThirdOctaveFrequencies[cNumFrequencies-i-1]/HalfThirdMulFak),0,10);
     DownsampleAmount:=fLPFilterArray[i].DownsampleAmount;
     CalculateCoefficients;
    end;
  end;

 fDownSampleMax:=fLPFilterArray[cNumFrequencies-1].DownsampleFaktor;
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
 for i := 0 to cNumFrequencies-1 do
  begin
   fLPFilterArray[i].Free;
   fHPFilterArray[i].Free;
  end;
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
 Timer.Enabled:=ASIOHost.Active;
end;

procedure TFmAnalyser.BSDownSampled(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray);
var i,j   : Integer;
    d,z,s : Double;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   d:=InBuffer[fChannelNr,i];
   for j:=0 to cNumFrequencies-1 do
    begin
     if (fDownSampler mod fLPFilterArray[j].DownsampleFaktor)<>0
      then Break;

     d:=fLPFilterArray[j].ProcessSample(d+1E-32);
     z:=fHPFilterArray[j].ProcessSample(d+1E-32);

     s:=IntPower(fSpeedConst[0],8*fLPFilterArray[j].DownsampleAmount+1);
     fFilterRMS[j]:=s*fFilterRMS[j]+(1-s)*Amp_to_dB(abs(z));
    end;
   inc(fDownSampler);
   if fDownSampler>=fDownSampleMax then fDownSampler:=0;
  end;
end;

procedure TFmAnalyser.AnalyserChartDblClick(Sender: TObject);
begin
 if BarChart.Align<>alClient
  then BarChart.Align:=alClient
  else
   begin
    BarChart.Align:=alBottom;
    BarChart.Top:=88;
    BarChart.Height:=ClientHeight-88;
   end;
end;

procedure TFmAnalyser.TimerTimer(Sender: TObject);
var j : Integer;
begin
 for j:=0 to cNumFrequencies-1
  do BarChart.MagnitudedB[j]:=fFilterRMS[cNumFrequencies-j-1]+fFSGain;
 BarChart.Invalidate;
end;


initialization
Set8087CW(Default8087CW or $3F);
{$IFDEF FPC}
{$i AnalyserForm.lrs}
{$ENDIF}

end.
