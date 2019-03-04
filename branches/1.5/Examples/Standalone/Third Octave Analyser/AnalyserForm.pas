unit AnalyserForm;

interface

uses
  {$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Messages, SysUtils, Classes, 
  Graphics, Controls, Forms, StdCtrls, ComCtrls, ExtCtrls, Spin, Math, TeEngine, 
  Series, TeeProcs, Chart, DAV_ASIOHost, DAV_Types, DAV_DspFilterChebyshevType1,
  VclTee.TeeGDIPlus;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies: array [0..cNumFrequencies-1] of Single = (
    16, 20, 25, 31, 40, 50, 63, 80, 100, 125, 160, 200, 250, 315, 400, 500,
    630, 800, 1000, 1250, 1600, 2000, 2500, 3150, 4000, 5000, 6300, 8000,
    10000, 12500, 16000, 20000);
  CDS = 8;
  CBW = 0.4;

type
  TDownsampleFilterRecord = record
    Lowpass: TChebyshev1LowpassFilter;
    Highpass: TChebyshev1HighpassFilter;
    Downsampling: Integer;
    RMS: Double;
  end;

  TFmAnalyser = class(TForm)
    ASIOHost: TASIOHost;
    BarSeries: TBarSeries;
    ButtonAnalyse: TButton;
    ButtonControlPanel: TButton;
    ChartAnalyser: TChart;
    ComboBoxChannel: TComboBox;
    ComboBoxDriver: TComboBox;
    LabelChannels: TLabel;
    LabelDriverName: TLabel;
    LabelFullScale: TLabel;
    LabelFullScaleUnit: TLabel;
    LabelSpeed: TLabel;
    RadioButtonFast: TRadioButton;
    RadioButtonMedium: TRadioButton;
    RadioButtonSlow: TRadioButton;
    SpinEditFullscaleGain: TSpinEdit;
    Timer: TTimer;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AnalyserChartDblClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BSDownSampled(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
    procedure ButtonAnalyseClick(Sender: TObject);
    procedure ButtonControlPanelClick(Sender: TObject);
    procedure ComboBoxDriverChange(Sender: TObject);
    procedure LabelDriverNameClick(Sender: TObject);
    procedure RadioButtonFastClick(Sender: TObject);
    procedure RadioButtonMediumClick(Sender: TObject);
    procedure RadioButtonSlowClick(Sender: TObject);
    procedure SpinEditFullscaleGainChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FUseDownsampling: Boolean;
    function GetBandReserve: Single;
    procedure SetBandReserve(const Value: Single);
    procedure SetUseDownsampling(const Value: Boolean);
    function GetSamplerate: Single;
    procedure CalculateSmoothingFactor;
  protected
    FMaxDSStages: Integer;
    FDownSampleCount: Integer;
    FDownSampleMax: Integer;
    FBandReserve: Double;

    FFilterArray: array [0..cNumFrequencies - 1] of TDownsampleFilterRecord;
    FChannelNr: Integer;
    FSpeedConst: array [0..1] of Single;
    FFSGain: Single;
    procedure UpdateBarGraph; virtual;
    procedure UpdateFilters; virtual;
    procedure DownsamplingChanged; virtual;
  public
    property BandReserve: Single read GetBandReserve write SetBandReserve;
    property UseDownsampling: Boolean read FUseDownsampling write SetUseDownsampling default True;
    property SampleRate: Single read GetSamplerate;
  end;

var
  FmAnalyser: TFmAnalyser;

implementation

{$R *.DFM}

uses
  Inifiles, Registry, DAV_Common;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var
  Band: Integer;
begin
  FChannelNr := 0;
  FSpeedConst[0] := 0.999; FSpeedConst[1] := 1 - FSpeedConst[0];
  FFSGain := SpinEditFullscaleGain.Value;
  ComboBoxDriver.Items := ASIOHost.DriverList;

  // make sure any ASIO driver is present
  if ComboBoxDriver.Items.Count = 0 then
    try
      raise Exception.Create('No ASIO Driver present! Application Terminated!');
    except
      Application.Terminate;
    end;

  FBandReserve := 0.25;
  UpdateFilters;

  // add bands
  for Band := 0 to CNumFrequencies - 1 do
  begin
    {$IFNDEF FPC}
    if CThirdOctaveFrequencies[Band] < 1000 then
      BarSeries.Add(0,FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz')
    else
      BarSeries.Add(0,FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz');
   {$ELSE}
   if Frequency < 1000 then
     ChartAnalyser.AddBar(FloatToStr(CThirdOctaveFrequencies[Band]) + ' Hz', 0, $000000FF)
   else
     ChartAnalyser.AddBar(FloatToStr(0.001 * CThirdOctaveFrequencies[Band]) + ' kHz', 0, $000000FF);
   {$ENDIF}
  end;

  UseDownsampling := True;
  DownsamplingChanged;

  if FDownSampleCount = -1 then
    ASIOHost.OnBufferSwitch32 := BSNormal
  else
    ASIOHost.OnBufferSwitch32 := BSDownSampled;
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
var
  FreqIndex: Integer;
begin
  ASIOHost.Active := False;

  // free filters
  for FreqIndex := 0 to CNumFrequencies - 1 do
  begin
    FreeAndNil(FFilterArray[FreqIndex].Lowpass);
    FreeAndNil(FFilterArray[FreqIndex].Highpass);
  end;
end;

procedure TFmAnalyser.FormShow(Sender: TObject);
begin
  // and make sure all controls are enabled or disabled
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
    Left := ReadInteger('Layout', 'Audio Left', Left);
    Top := ReadInteger('Layout', 'Audio Top', Top);
    ComboBoxDriver.ItemIndex := ReadInteger('Audio', 'Asio Driver', -1);
    if ComboBoxDriver.ItemIndex >= 0 then
      ComboBoxDriverChange(ComboBoxDriver);
    ComboBoxChannel.ItemIndex := ReadInteger('Audio', 'Channels', 0);
    SpinEditFullscaleGain.Value := ReadInteger('Audio', 'Fullscale Gain', 0);
  finally
    Free;
  end;
end;

procedure TFmAnalyser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  // save settings
  with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
  try
    WriteInteger('Layout', 'Audio Left', Left);
    WriteInteger('Layout', 'Audio Top', Top);
    WriteInteger('Audio', 'ASIO Driver', ComboBoxDriver.ItemIndex);
    WriteInteger('Audio', 'Channels', ComboBoxChannel.ItemIndex);
    WriteInteger('Audio', 'Fullscale Gain', SpinEditFullscaleGain.Value);
  finally
    Free;
  end;
end;

procedure TFmAnalyser.UpdateFilters;
var
  Band: Integer;
  Downsampling: Integer;
  DesiredFreq: Double;
const
  HalfThirdMulFak: Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
  Downsampling := 0;

  for Band := 0 to Length(FFilterArray) - 1 do
  begin
    // Lowpass
    DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] * HalfThirdMulFak;
    if DesiredFreq > 0.499 * SampleRate then
      DesiredFreq := 0.499 * SampleRate;

    if UseDownsampling then
      while ((2 * DesiredFreq / Self.SampleRate) * (1 shl Downsampling)) < FBandReserve do
        Inc(Downsampling);

    // eventually create filter
    if not Assigned(FFilterArray[Band].Lowpass) then
      FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(10);

    with FFilterArray[Band].Lowpass do
    begin
      SampleRate := Self.SampleRate / (1 shl Downsampling);
      Frequency := DesiredFreq;
    end;
    FFilterArray[Band].Downsampling := (1 shl Downsampling);

    // Highpass
    DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] / HalfThirdMulFak;

    // eventually create filter
    if not Assigned(FFilterArray[Band].Highpass) then
      FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(12);
    
    with FFilterArray[Band].Highpass do
    begin
      SampleRate := Self.SampleRate / (1 shl Downsampling);
      Frequency := DesiredFreq;
    end;
  end;
  FDownSampleMax := 1 shl Downsampling;
end;

function TFmAnalyser.GetBandReserve: Single;
begin
  Result := 100 * FBandReserve;
end;

function TFmAnalyser.GetSamplerate: Single;
begin
  Result := ASIOHost.SampleRate;
end;

procedure TFmAnalyser.RadioButtonFastClick(Sender: TObject);
begin
  FSpeedConst[0] := 0.99;
  CalculateSmoothingFactor;
end;

procedure TFmAnalyser.RadioButtonMediumClick(Sender: TObject);
begin
  FSpeedConst[0] := 0.999;
  CalculateSmoothingFactor;
end;

procedure TFmAnalyser.RadioButtonSlowClick(Sender: TObject);
begin
  FSpeedConst[0] := 0.9999;
  CalculateSmoothingFactor;
end;

procedure TFmAnalyser.CalculateSmoothingFactor;
begin
  FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TFmAnalyser.SpinEditFullscaleGainChange(Sender: TObject);
begin
  FFSGain := SpinEditFullscaleGain.Value;
end;

procedure TFmAnalyser.SetBandReserve(const Value: Single);
begin
  FBandReserve := 0.01 * Value;
end;

procedure TFmAnalyser.SetUseDownsampling(const Value: Boolean);
begin
  if FUseDownsampling <> Value then
  begin
    FUseDownsampling := Value;
    DownsamplingChanged;
  end;
end;

procedure TFmAnalyser.DownsamplingChanged;
begin
  if FUseDownsampling then
    FDownSampleCount := 0
  else
    FDownSampleCount := -1
end;

procedure TFmAnalyser.ComboBoxDriverChange(Sender: TObject);
var
  i: Integer;
begin
  ButtonControlPanel.Enabled := False;
  ButtonAnalyse.Enabled := False;
  ComboBoxDriver.ItemIndex := ComboBoxDriver.Items.IndexOf(ComboBoxDriver.Text);
  if ComboBoxDriver.ItemIndex >= 0 then
  begin
    ASIOHost.DriverIndex := ComboBoxDriver.ItemIndex;
    ComboBoxChannel.Clear;
    for i := 0 to ASIOHost.InputChannelCount - 1 do
      ComboBoxChannel.Items.Add(string(ASIOHost.InputChannelInfos[i].name));

    with TIniFile.Create(ExtractFilePath(ParamStr(0)) + 'ASIODemo.INI') do
    try
      WriteInteger('Audio', 'Asio Driver', ComboBoxDriver.ItemIndex);
    finally
      Free;
    end;
    ButtonControlPanel.Enabled := True;
    ButtonAnalyse.Enabled := True;
    ComboBoxChannel.ItemIndex := 0;
  end;
end;

procedure TFmAnalyser.ButtonControlPanelClick(Sender: TObject);
begin
  ASIOHost.ControlPanel;
end;

procedure TFmAnalyser.ButtonAnalyseClick(Sender: TObject);
begin
  if ButtonAnalyse.Caption = 'Analyse' then
  begin
    ASIOHost.Active := True; // Start Audio
    ButtonAnalyse.Caption := 'Stop';
  end
  else
  begin
    ASIOHost.Active := False; // Stop Audio
    ButtonAnalyse.Caption := 'Analyse';
  end;
  Timer.Enabled := ASIOHost.Active;
end;

procedure TFmAnalyser.UpdateBarGraph;
var
  Band: Integer;
begin
  {$IFNDEF FPC}
  for Band := 0 to cNumFrequencies - 1 do
    BarSeries.YValue[Band] := FFilterArray[cNumFrequencies - Band - 1].RMS + FFSGain;
  ChartAnalyser.Invalidate;
  {$ELSE}
  for j := 0 to cNumFrequencies - 1 do
    TBar(ChartAnalyser.Bars.Items[Band]).Value := round(FFilterArray[cNumFrequencies - Band - 1].RMS + FFSGain);
  ChartAnalyser.Invalidate;
  {$ENDIF}
end;

procedure TFmAnalyser.AnalyserChartDblClick(Sender: TObject);
begin
  if ChartAnalyser.Align <> alClient then
    ChartAnalyser.Align := alClient
  else
  begin
    ChartAnalyser.Align := alBottom;
    ChartAnalyser.Top := 88;
    ChartAnalyser.Height := ClientHeight - 88;
  end;
end;

procedure TFmAnalyser.TimerTimer(Sender: TObject);
begin
  UpdateBarGraph;
end;

procedure TFmAnalyser.LabelDriverNameClick(Sender: TObject);
begin
  if FDownSampleCount > 0 then
    FDownSampleCount := -1
  else
    FDownSampleCount := 0;

  UpdateFilters;
end;

procedure TFmAnalyser.BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  i,j: Integer;
  d,z: Double;
const
  cDenorm = 1E-32;
begin
  for i := 0 to ASIOHost.BufferSize - 1 do
  begin
    d := InBuffer[FChannelNr,i];
    for j := 0 to CNumFrequencies - 1 do
    begin
      d := FFilterArray[j].Lowpass.ProcessSample64(d + cDenorm);
      z := FFilterArray[j].Highpass.ProcessSample64(d + cDenorm);
      FFilterArray[j].RMS := FSpeedConst[0] * FFilterArray[j].RMS + FSpeedConst[1] * Amp_to_dB(abs(z));
    end;
  end;
  UpdateBarGraph;
end;

procedure TFmAnalyser.ASIOHostSampleRateChanged(Sender: TObject);
begin
  UpdateFilters;
end;

procedure TFmAnalyser.BSDownSampled(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleFixedArray);
var
  i, j: Integer;
  d, z, s: Double;
const
  cDenorm = 1E-32;
begin
  for i := 0 to ASIOHost.BufferSize - 1 do
  begin
    d := InBuffer[FChannelNr, i];
    for j := 0 to CNumFrequencies - 1 do
    begin
      if (FDownSampleCount mod FFilterArray[j].Downsampling) <> 0 then
        Break;

      d := FFilterArray[j].Lowpass.ProcessSample64(d + cDenorm);
      z := FFilterArray[j].Highpass.ProcessSample64(d + cDenorm);

      s := IntPower(FSpeedConst[0], 8 * FFilterArray[j].Downsampling + 1);
      FFilterArray[j].RMS := s * FFilterArray[j].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
    Inc(FDownSampleCount);
    if FDownSampleCount >= FDownSampleMax then
      FDownSampleCount := 0;
  end;

  UpdateBarGraph;
end;

initialization
  Set8087CW(Default8087CW or $3F);

end.
