unit AnalyserForm;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, StdCtrls,
  ComCtrls, ExtCtrls, Spin, Math, TeEngine, Series, TeeProcs, Chart,
  DAV_ASIOHost, DAV_Common, DAV_DspFilterChebyshevType1, GLScene, GLObjects,
  GLMisc, GLWin32Viewer;

const
  CNumFrequencies = 32;
  CThirdOctaveFrequencies : Array [0..cNumFrequencies-1] of Single =
      (16,20,25,31,40,50,63,80,100,125,160,200,250,315,400,500,630,800,1000,
       1250,1600,2000,2500,3150,4000,5000,6300,8000,10000,12500,16000,20000);
  CDS = 8;
  CBW = 0.4;

type
  TDownsampleFilterRecord = record
    Lowpass      : TChebyshev1LowpassFilter;
    Highpass     : TChebyshev1HighpassFilter;
    Downsampling : Integer;
    RMS          : Double;
  end;

  TFmAnalyser = class(TForm)
    ASIOHost: TASIOHost;
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
    BarGraphScene: TGLScene;
    GLSceneViewer: TGLSceneViewer;
    DefaultCamera: TGLCamera;
    LightSource: TGLLightSource;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure AnalyserChartDblClick(Sender: TObject);
    procedure ASIOHostSampleRateChanged(Sender: TObject);
    procedure BSDownSampled(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
    procedure Bt_AnalyseClick(Sender: TObject);
    procedure Bt_CPClick(Sender: TObject);
    procedure DriverComboChange(Sender: TObject);
    procedure Lb_DrivernameClick(Sender: TObject);
    procedure RB_FastClick(Sender: TObject);
    procedure RB_MediumClick(Sender: TObject);
    procedure RB_SlowClick(Sender: TObject);
    procedure SEFullscaleGainChange(Sender: TObject);
    procedure TimerTimer(Sender: TObject);
  private
    FUseDownsampling: Boolean;
    function GetBandReserve: Single;
    procedure SetBandReserve(const Value: Single);
    procedure SetUseDownsampling(const Value: Boolean);
    function GetSamplerate: Single;
    procedure CalculateSmoothingFactor;
  protected
    FMaxDSStages     : Integer;
    FDownSampleCount : Integer;
    FDownSampleMax   : Integer;
    FBandReserve     : Double;

    FFilterArray     : Array [0..cNumFrequencies - 1] of TDownsampleFilterRecord;
    FCubeArray       : Array [0..cNumFrequencies - 1] of TGLCube;

    FChannelNr       : Integer;
    FSpeedConst      : Array [0..1] of Single;
    FFSGain          : Single;
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
  Inifiles, Registry;

procedure TFmAnalyser.FormCreate(Sender: TObject);
var
  Band : Integer;
begin
 FChannelNr := 0;
 FSpeedConst[0] := 0.999; FSpeedConst[1] := 1 - FSpeedConst[0];
 FFSGain := SEFullscaleGain.Value;
 DriverCombo.Items := ASIOHost.DriverList;

 // make sure any ASIO driver is present
 if DriverCombo.Items.Count = 0 then
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
   FCubeArray[Band] := TGLCube.Create(BarGraphScene);
   with FCubeArray[Band] do
    begin
     CubeWidth := 0.06;
     CubeHeight := 1;
     CubeDepth := 0.1;
     Position.X := 0.08 * (Band - 16);
     BarGraphScene.Objects.AddChild(FCubeArray[Band]);
    end;
  end;

 DefaultCamera.TargetObject := FCubeArray[16];

 UseDownsampling := True;
 DownsamplingChanged;

 if FDownSampleCount = -1
  then ASIOHost.OnBufferSwitch32 := BSNormal
  else ASIOHost.OnBufferSwitch32 := BSDownSampled;
end;

procedure TFmAnalyser.FormDestroy(Sender: TObject);
var
  i : Integer;
begin
 ASIOHost.Active := False;

 // free filters
 for i := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FFilterArray[i].Lowpass);
   FreeAndNil(FFilterArray[i].Highpass);
  end;
end;

procedure TFmAnalyser.FormShow(Sender: TObject);
begin
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
end;

procedure TFmAnalyser.FormClose(Sender: TObject; var Action: TCloseAction);
begin
 // save settings
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
end;

procedure TFmAnalyser.UpdateFilters;
var
  Band         : Integer;
  Downsampling : Integer;
  DesiredFreq  : Double;
const
  HalfThirdMulFak : Double = 1.1224620483093729814335330496792; // = Power(2,1/6)
begin
 Downsampling := 0;

 for Band := 0 to Length(FFilterArray) - 1 do
  begin
   // Lowpass
   DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] * HalfThirdMulFak;
   if DesiredFreq > 0.499 * SampleRate then DesiredFreq := 0.499 * SampleRate;   

   if UseDownsampling then
    while ((2 * DesiredFreq / Self.SampleRate) * (1 shl Downsampling)) < FBandReserve
     do Inc(Downsampling);

   // eventually create filter
   if not assigned(FFilterArray[Band].Lowpass)
    then FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(10);

   with FFilterArray[Band].Lowpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
   FFilterArray[Band].Downsampling := (1 shl Downsampling);

   // Highpass
   DesiredFreq := CThirdOctaveFrequencies[CNumFrequencies - Band - 1] / HalfThirdMulFak;

   // eventually create filter
   if not assigned(FFilterArray[Band].Highpass)
    then FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(12);
    
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
 result := 100 * FBandReserve;
end;

function TFmAnalyser.GetSamplerate: Single;
begin
 result := ASIOHost.SampleRate;
end;

procedure TFmAnalyser.RB_FastClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.99;
 CalculateSmoothingFactor;
end;

procedure TFmAnalyser.RB_MediumClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.999;
 CalculateSmoothingFactor;
end;

procedure TFmAnalyser.RB_SlowClick(Sender: TObject);
begin
 FSpeedConst[0] := 0.9999;
 CalculateSmoothingFactor;
end;

procedure TFmAnalyser.CalculateSmoothingFactor;
begin
 FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TFmAnalyser.SEFullscaleGainChange(Sender: TObject);
begin
 FFSGain := SEFullscaleGain.Value;
// AnalyserChart.LeftAxis.Maximum := FFSGain+20;
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
 if FUseDownsampling
  then FDownSampleCount := 0
  else FDownSampleCount := -1
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

procedure TFmAnalyser.UpdateBarGraph;
var
  Band : Integer;
begin
 for Band := 0 to cNumFrequencies - 1 do
  begin
   FCubeArray[Band].CubeHeight := (FFilterArray[cNumFrequencies - Band - 1].RMS + FFSGain) / FFSGain;
   FCubeArray[Band].StructureChanged;
  end;
end;

procedure TFmAnalyser.AnalyserChartDblClick(Sender: TObject);
begin
 if GLSceneViewer.Align <> alClient
  then GLSceneViewer.Align := alClient
  else
   begin
    GLSceneViewer.Align := alBottom;
    GLSceneViewer.Top := 88;
    GLSceneViewer.Height := ClientHeight - 88;
   end;
end;

procedure TFmAnalyser.TimerTimer(Sender: TObject);
begin
 UpdateBarGraph;
end;

procedure TFmAnalyser.Lb_DrivernameClick(Sender: TObject);
var
  i : Integer;
begin
 if FDownSampleCount > 0
  then FDownSampleCount := -1
  else FDownSampleCount := 0;

 UpdateFilters;
end;

procedure TFmAnalyser.BSNormal(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray);
var
  i,j : Integer;
  d,z : Double;
const
  cDenorm = 1E-32;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   d := InBuffer[FChannelNr,i];
   for j := 0 to CNumFrequencies - 1 do
    begin
     d := FFilterArray[j].Lowpass.ProcessSample(d + cDenorm);
     z := FFilterArray[j].Highpass.ProcessSample(d + cDenorm);
     FFilterArray[j].RMS := FSpeedConst[0] * FFilterArray[j].RMS + FSpeedConst[1] * Amp_to_dB(abs(z));
    end;
  end;
end;

procedure TFmAnalyser.ASIOHostSampleRateChanged(Sender: TObject);
begin
 UpdateFilters;
end;

procedure TFmAnalyser.BSDownSampled(Sender: TObject; const InBuffer,
  OutBuffer: TDAVArrayOfSingleDynArray);
var
  i, j, r : Integer;
  d, z, s : Double;
const
  cDenorm = 1E-32;
begin
 for i := 0 to ASIOHost.BufferSize - 1 do
  begin
   d := InBuffer[FChannelNr, i];
   for j := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[j].Downsampling) <> 0
      then Break;

     d := FFilterArray[j].Lowpass.ProcessSample(d + cDenorm);
     z := FFilterArray[j].Highpass.ProcessSample(d + cDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[j].Downsampling + 1);
     FFilterArray[j].RMS := s * FFilterArray[j].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
  end;
end;

initialization
  Set8087CW(Default8087CW or $3F);

end.