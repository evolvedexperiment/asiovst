unit AnalyserForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, ExtCtrls, Spin, Math, TeeProcs, TeEngine, Chart, Series,
  DAV_Common, DAV_Complex, DAV_ASIOHost;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
      630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
      10000, 12500, 16000, 20000);

type
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
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure AnalyserChartDblClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure Bt_AnalyseClick(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure RB_FastClick(Sender: TObject);
    procedure RB_MediumClick(Sender: TObject);
    procedure RB_SlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FMagnitudes     : Array [0..cNumFrequencies - 1] of Double;
    FThirdOctaveExp : Array [0..cNumFrequencies - 1] of TComplexSingle;
    FSpeedConst     : Array [0..1] of Single;
    FChannelNr      : Integer;
    FSampleRateReci : Double;
    FFSGain         : Single;

    FBuffer         : PDAVSingleFixedArray;  // the Buffer
    FBufferSize     : Integer;               // Buffer size
    FBufferPosition : Integer;               // position within the Buffer
    FBufferOverlap  : Integer;               // overlap in samples
    procedure DoGoertzelMagic;
    procedure CalculateWeight;
    procedure CalculateComplexAngulars;
  end;

var
  FmAnalyser: TFmAnalyser;

implementation

{$R *.DFM}

uses
  Inifiles, Registry, DAV_ASIOConvert, DAV_DspDft;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var
  Band : Integer;
begin
 FChannelNr := 0;
 FSpeedConst[0] := 0.99;
 CalculateWeight;

 FBufferPosition := 0;
 FBufferOverlap  := 15 * 1024;
 FBufferSize     := 16 * 1024;
 ReallocMem(FBuffer, FBufferSize * SizeOf(Single));

 FFSGain := SEFullscaleGain.Value;
 FSampleRateReci := 1 / ASIOHost.SampleRate;
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
   SEFullscaleGain.Value := ReadInteger('Audio', 'Fullscale Gain', 0);
  finally
   Free;
  end;

 CalculateComplexAngulars;
 for Band := 0 to CNumFrequencies - 1 do
  begin
   {$IFNDEF FPC}
   if CThirdOctaveFrequencies[Band] < 1000
    then BarSeries.Add(0,FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz')
    else BarSeries.Add(0,FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz');
   {$ELSE}
   if Frequency < 1000
    then AnalyserChart.AddBar(FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz', 0, $000000FF)
    else AnalyserChart.AddBar(FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz', 0, $000000FF);
   {$ENDIF}
  end;

 ASIOHostSampleRateChanged(Sender);
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
begin
 ASIOHost.Active := False;
 with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
   WriteInteger('Layout', 'Audio Left', Left);
   WriteInteger('Layout', 'Audio Top', Top);
   WriteInteger('Audio', 'ASIO Driver', DriverCombo.ItemIndex);
   WriteInteger('Audio', 'Channels', ChannelBox.ItemIndex);
   WriteInteger('Audio', 'Fullscale Gain', SEFullscaleGain.Value);
  finally
   Free;
  end;
 Dispose(FBuffer);
end;

procedure TFmAnalyser.CalculateComplexAngulars;
var
  Band : Integer;
begin
 for Band := 0 to CNumFrequencies - 1
  do GetSinCos(CThirdOctaveFrequencies[Band] * FSampleRateReci, FThirdOctaveExp[Band].Im, FThirdOctaveExp[Band].Re);
end;

procedure TFmAnalyser.RB_FastClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.9;
 CalculateWeight;
end;

procedure TFmAnalyser.RB_MediumClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.99;
 CalculateWeight;
end;

procedure TFmAnalyser.RB_SlowClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.999;
 CalculateWeight;
end;

procedure TFmAnalyser.CalculateWeight;
begin
 FSpeedConst[1] := 0.5 * (1 - FSpeedConst[0]);
end;

procedure TFmAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 FFSGain := SEFullscaleGain.Value;
// AnalyserChart.LeftAxis.Maximum := FFSGain + 20;
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
 Timer.Enabled := ASIOHost.Active;
end;

procedure TFmAnalyser.ASIOHostSampleRateChanged(Sender: TObject);
begin
 FSampleRateReci := 1 / ASIOHost.SampleRate;
 CalculateComplexAngulars;
end;

procedure TFmAnalyser.BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
var
  CurrentPosition : Integer;
begin
 CurrentPosition := 0;
 repeat
  if FBufferPosition + (ASIOHost.BufferSize - CurrentPosition) < FBufferSize then
   begin
    Move(InBuffer[0, CurrentPosition],
         FBuffer[FBufferPosition],
         (ASIOHost.BufferSize - CurrentPosition) * Sizeof(Single));
    FBufferPosition  := FBufferPosition + (ASIOHost.BufferSize - CurrentPosition);
    CurrentPosition := ASIOHost.BufferSize;
   end
  else
   begin
    Move(InBuffer[0, CurrentPosition],
         FBuffer[FBufferPosition],
         (FBufferSize - FBufferPosition) * Sizeof(Single));

    DoGoertzelMagic; // do Processing here!

    Move(FBuffer[FBufferSize - FBufferOverlap],
         FBuffer[0], FBufferOverlap * Sizeof(Single));

    CurrentPosition := CurrentPosition + (FBufferSize - FBufferPosition);
    FBufferPosition := FBufferOverlap;
   end;
 until CurrentPosition >= ASIOHost.BufferSize;
end;

procedure TFmAnalyser.DoGoertzelMagic;
var
  i  : Integer;
  bs : Integer;
begin
 for i := 0 to CNumFrequencies - 1 do
  begin
   bs := round(sqr(sqr(1 - CThirdOctaveFrequencies[i] * FSampleRateReci)) * FBufferSize);
   with Goertzel(PDAVSingleFixedArray(@FBuffer^[(FBufferSize - bs) div 2]), bs,
                 FThirdOctaveExp[i])
    do FMagnitudes[i] := FSpeedConst[0] * FMagnitudes[i]+
                         FSpeedConst[1] * Amp_to_dB(sqr(Re) + sqr(Im));
  end;
end;

procedure TFmAnalyser.AnalyserChartDblClick(Sender: TObject);
begin
 with AnalyserChart do
  if Align <> alClient
   then Align := alClient
   else
    begin
     Align := alBottom;
     Top := 88;
     Height := Self.ClientHeight - 88;
    end;
end;

procedure TFmAnalyser.TimerTimer(Sender: TObject);
var
  Band : Integer;
begin
 {$IFNDEF FPC}
 for Band := 0 to cNumFrequencies - 1
  do BarSeries.YValue[Band] := FMagnitudes[Band] + FFSGain;
 AnalyserChart.Invalidate;
 {$ELSE}
 for j := 0 to cNumFrequencies - 1
  do TBar(AnalyserChart.Bars.Items[Band]).Value := round(FMagnitudes[Band] + FFSGain);
 AnalyserChart.Invalidate;
 {$ENDIF}
end;

initialization
  Set8087CW(Default8087CW or $3F);

end.