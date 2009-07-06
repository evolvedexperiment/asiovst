unit AudioAmeliorationDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspExciter, DAV_DspAmbience, DAV_DspCrosstalkSimulator,
  DAV_DspLightweightDynamics, DAV_DspPsychoacousticBassEnhancer,
  DAV_DspFilterChebyshevType1;

const
  CNumFrequencies = 10;
  CFrequencyArray : array [0..CNumFrequencies - 1] of Single = (31.25, 62.5,
    125, 250, 500, 1000, 2000, 4000, 8000, 16000);

type
  TDownsampleFilterRecord = record
    Lowpass      : TChebyshev1LowpassFilter;
    Highpass     : TChebyshev1HighpassFilter;
    Downsampling : Integer;
    RMS          : Double;
  end;

  TAudioAmeliorationModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterSpeakerDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleProcessSpeaker(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessHeadphones(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure Parameter3DSoundChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAmbienceActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAmbienceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterCompressorChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExciterActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExciterChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExtraBassActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExtraBassChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSpeakerChangedChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FAmbience           : TAmbience;
    FCompressor         : TLightweightSoftKneeCompressor;
    FLimiter            : TLightweightSoftKneeLimiter;
    FCrosstalkSimulator : TIIRCrosstalkSimulator;
    FAmbienceActive     : Boolean;
    FExciter            : array [0..1] of TExciter;
    FBassEnhancer       : array [0..1] of THarmonicBass;
    FIsSpeaker          : Boolean;
    FExciterActive      : Boolean;
    FSourroundActive    : Boolean;
    FCompressorActive   : Boolean;
    FExtraBassActive    : Boolean;
    FPowerActive        : Boolean;

    FMaxDSStages        : Integer;
    FDownSampleCount    : Integer;
    FDownSampleMax      : Integer;
    FBandReserve        : Double;
    FUseDownsampling    : Boolean;

    FFilterArray        : Array [0..CNumFrequencies - 1] of TDownsampleFilterRecord;
    FFSGain             : Single;
    FSpeedConst         : array [0..1] of Single;

    procedure ChooseProcess;
    function GetBandReserve: Single;
    function GetBandRMS(Index: Integer): Single;
    procedure SetBandReserve(const Value: Single);
    procedure SetUseDownsampling(const Value: Boolean);
    procedure CalculateSmoothingFactor;
    procedure DownsamplingChanged;
    procedure UpdateFilters;
  public
    property IsSpeaker: Boolean read FIsSpeaker;
    property PowerActive: Boolean read FPowerActive;
    property ExciterActive: Boolean read FExciterActive;
    property AmbienceActive: Boolean read FAmbienceActive;
    property SourroundActive: Boolean read FSourroundActive;
    property CompressorActive: Boolean read FCompressorActive;
    property ExtraBassActive: Boolean read FExtraBassActive;

    // Analyser
    property BandReserve: Single read GetBandReserve write SetBandReserve;
    property UseDownsampling: Boolean read FUseDownsampling write SetUseDownsampling default True;
    property BandRMS[Index: Integer]: Single read GetBandRMS;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_DspDynamics, DAV_Approximations, AudioAmeliorationGUI;

procedure TAudioAmeliorationModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 // create and setup bass enhancer
 for Channel := 0 to Length(FExciter) - 1 do
  begin
   FExciter[Channel] := TExciter.Create;
   FExciter[Channel].SampleRate := SampleRate;
  end;

 // create and setup ambience
 FAmbience := TAmbience.Create;
 FAmbience.SampleRate := SampleRate;

 // create and setup crosstalk simulator
 FCrosstalkSimulator := TIIRCrosstalkSimulator.Create;
 FCrosstalkSimulator.Model := csmIRCAM;
 FCrosstalkSimulator.SampleRate := SampleRate;

 // create and setup compressor
 FCompressor := TLightweightSoftKneeCompressor.Create;
 FCompressor.SampleRate := SampleRate;

 // create and setup bass enhancer
 for Channel := 0 to Length(FBassEnhancer) - 1 do
  begin
   FBassEnhancer[Channel] := THarmonicBass.Create;
   FBassEnhancer[Channel].SampleRate := SampleRate;
  end;

 // create and setup limiter
 FLimiter := TLightweightSoftKneeLimiter.Create;
 with FLimiter do
  begin
   Threshold_dB  := -3.02;
   Knee_dB       := 0.5;
   MakeUpGain_dB := 3;
   Attack        := 0.2;
   Release       := 800;
   SampleRate    := Self.SampleRate;
  end;

 // set default speaker type
 FIsSpeaker := True;

 // setup default speaker type analyser
 FSpeedConst[0] := 0.999;
 CalculateSmoothingFactor;
 FFSGain := 0;
 FBandReserve := 0.25;
 UpdateFilters;

 UseDownsampling := True;
 DownsamplingChanged;

end;

procedure TAudioAmeliorationModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
  Band    : Integer;
begin
 FreeAndNil(FAmbience);
 FreeAndNil(FCompressor);
 FreeAndNil(FLimiter);
 FreeAndNil(FCrosstalkSimulator);

 // free bass enhancer
 for Channel := 0 to Length(FBassEnhancer) - 1
  do FreeAndNil(FBassEnhancer[Channel]);

 // free exciter
 for Channel := 0 to Length(FExciter) - 1
  do FreeAndNil(FExciter[Channel]);

 // free filters
 for Band := 0 to CNumFrequencies - 1 do
  begin
   FreeAndNil(FFilterArray[Band].Lowpass);
   FreeAndNil(FFilterArray[Band].Highpass);
  end;
end;

procedure TAudioAmeliorationModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmAudioAmelioration.Create(Self);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TAudioAmeliorationModule.ParameterSpeakerDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] > 0.5
  then PreDefined := 'Speaker'
  else PreDefined := 'Headphones';
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.ParameterPowerChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FPowerActive := Value > 0.5;
 ChooseProcess;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdatePower;
end;

procedure TAudioAmeliorationModule.ParameterExciterActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FExciterActive := Value > 0.5;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExciterActive;
end;

procedure TAudioAmeliorationModule.ParameterAmbienceActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FAmbienceActive := Value > 0.5;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateAmbienceActive;
end;

function TAudioAmeliorationModule.GetBandReserve: Single;
begin
 result := 100 * FBandReserve;
end;

function TAudioAmeliorationModule.GetBandRMS(Index: Integer): Single;
begin
 if Index in [0..CNumFrequencies - 1]
  then result := FFilterArray[Index].RMS
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TAudioAmeliorationModule.Parameter3DSoundChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FSourroundActive := Value > 0.5;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).Update3DSurroundActive;
end;

procedure TAudioAmeliorationModule.ParameterCompressorActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressorActive := Value > 0.5;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateCompressorActive;
end;

procedure TAudioAmeliorationModule.ParameterExtraBassActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FExtraBassActive := Value > 0.5;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExtraBassActive;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.ParameterExciterChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FBassEnhancer) - 1 do
  with FExciter[Channel] do
   begin
    Frequency := 8000 - 6500 * FastSqrtBab1(0.01 * Value);
    HighFrequencyLevel := 1 + 0.001 * Value;
    LowFrequencyLevel := 1;
    HarmonicsLevel := 0.01 + 0.01 * Value;
   end;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExtraBass;
end;

procedure TAudioAmeliorationModule.ParameterAmbienceChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FAmbience.Mix := 0.009 * Value;
 FAmbience.Damping := 0.3 + 0.6 * (1 - 0.01 * Value);

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateAmbience;
end;

procedure TAudioAmeliorationModule.ParameterCompressorChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 with FCompressor do
  begin
   Threshold_dB := -0.06 * Value;
   Knee_dB := 5 * (1 - 0.01 * Value);
   MakeUpGain_dB := -Threshold_dB + (1 - 0.01 * Value) * Knee_dB;
   Ratio := FastPower2MinError4(0.05 * Value);
  end;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateCompressor;
end;

procedure TAudioAmeliorationModule.ParameterExtraBassChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FBassEnhancer) - 1 do
  with FBassEnhancer[Channel] do
   begin
    HighpassSelect := hp1stOrder;
    Frequency := 80 + 1.5 * Value;
    HarmonicBassLevel := dB_To_Amp(-18 * (1 - 0.01 * Value));
    OriginalBassLevel := dB_To_Amp(-0.1 * Value);
    Decay := dB_To_Amp(-(9 + 0.1 * Value));
    InputLevel := 1;
    HighFrequencyLevel := 1;
   end;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateExtraBass;
end;

procedure TAudioAmeliorationModule.ParameterSpeakerChangedChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FIsSpeaker := Value > 0.5;
 ChooseProcess;

 if EditorForm is TFmAudioAmelioration
  then TFmAudioAmelioration(EditorForm).UpdateSpeaker;
end;

procedure TAudioAmeliorationModule.ChooseProcess;
begin
 if FPowerActive
  then
   if FIsSpeaker
    then OnProcess := VSTModuleProcessSpeaker
    else OnProcess := VSTModuleProcessHeadphones
  else OnProcess := VSTModuleProcessBypass;
 OnProcessReplacing := OnProcess;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.SetBandReserve(const Value: Single);
begin
 FBandReserve := 0.01 * Value;
end;

procedure TAudioAmeliorationModule.SetUseDownsampling(const Value: Boolean);
begin
 if FUseDownsampling <> Value then
  begin
   FUseDownsampling := Value;
   DownsamplingChanged;
  end;
end;

procedure TAudioAmeliorationModule.CalculateSmoothingFactor;
begin
 FSpeedConst[1] := 1 - FSpeedConst[0];
end;

procedure TAudioAmeliorationModule.UpdateFilters;
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
   DesiredFreq := CFrequencyArray[CNumFrequencies - Band - 1] * HalfThirdMulFak;
   if DesiredFreq > 0.499 * SampleRate then DesiredFreq := 0.499 * SampleRate;   

   if UseDownsampling then
    while ((2 * DesiredFreq / Self.SampleRate) * (1 shl Downsampling)) < FBandReserve
     do Inc(Downsampling);

   // eventually create filter
   if not assigned(FFilterArray[Band].Lowpass)
    then FFilterArray[Band].Lowpass := TChebyshev1LowpassFilter.Create(6);

   with FFilterArray[Band].Lowpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
   FFilterArray[Band].Downsampling := (1 shl Downsampling);

   // Highpass
   DesiredFreq := CFrequencyArray[CNumFrequencies - Band - 1] / HalfThirdMulFak;

   // eventually create filter
   if not assigned(FFilterArray[Band].Highpass)
    then FFilterArray[Band].Highpass := TChebyshev1HighpassFilter.Create(8);
    
   with FFilterArray[Band].Highpass do
    begin
     SampleRate := Self.SampleRate / (1 shl Downsampling);
     Frequency := DesiredFreq;
    end;
  end;
 FDownSampleMax := 1 shl Downsampling;
end;

procedure TAudioAmeliorationModule.DownsamplingChanged;
begin
 if FUseDownsampling
  then FDownSampleCount := 0
  else FDownSampleCount := -1;

(*
 if FDownSampleCount = -1
  then OnProcess := VSTModuleProcessNormal
  else OnProcess := VSTModuleProcessDownSampled;

 OnProcessReplacing := OnProcess;
*)
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAudioAmeliorationModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 FAmbience.SampleRate := SampleRate;
 FCompressor.SampleRate := SampleRate;
 FLimiter.SampleRate := SampleRate;
 FCrosstalkSimulator.SampleRate := SampleRate;

 for Channel := 0 to Length(FBassEnhancer) - 1
  do FBassEnhancer[Channel].SampleRate := SampleRate;

 for Channel := 0 to Length(FExciter) - 1
  do FExciter[Channel].SampleRate := SampleRate;
end;

procedure TAudioAmeliorationModule.VSTModuleProcessSpeaker(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
  Band    : Integer;
  d, z, s : Double;
const
  cDenorm = 1E-32;
begin
 for Channel := 0 to min(numInputs, numOutputs) - 1
  do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));

 // apply exciter
 if ExciterActive then
  for Channel := 0 to Length(FExciter) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FExciter[Channel].Process(Outputs[Channel, Sample]);

 // apply ambience
 if AmbienceActive then
  for Sample := 0 to SampleFrames - 1
   do FAmbience.Process(Outputs[0, Sample], Outputs[1, Sample]);

 // apply compression
 if CompressorActive then
  for Sample := 0 to SampleFrames - 1 do
   begin
    FCompressor.InputSample(0.5 * (Outputs[0, Sample] + Outputs[1, Sample]));
    Outputs[0, Sample] := FCompressor.GainSample(Outputs[0, Sample]);
    Outputs[1, Sample] := FCompressor.GainSample(Outputs[1, Sample]);
   end;

 // apply extra bass
 if ExtraBassActive then
  for Channel := 0 to Length(FBassEnhancer) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FBassEnhancer[Channel].Process(Outputs[Channel, Sample]);

 // apply limiter
 for Sample := 0 to SampleFrames - 1 do
  begin
   FLimiter.InputSample(0.5 * (Outputs[0, Sample] + Outputs[1, Sample]));
   Outputs[0, Sample] := Limit(1.3 * FastTanhMinError4(FLimiter.GainSample(Outputs[0, Sample])));
   Outputs[1, Sample] := Limit(1.3 * FastTanhMinError4(FLimiter.GainSample(Outputs[1, Sample])));
  end;

 for Sample := 0 to SampleFrames - 1 do
  begin
   d := 0.5 * (Outputs[0, Sample] + Outputs[1, Sample]);
   for Band := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[Band].Downsampling) <> 0
      then Break;

     d := FFilterArray[Band].Lowpass.ProcessSample(d + cDenorm);
     z := FFilterArray[Band].Highpass.ProcessSample(d + cDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[Band].Downsampling + 1);
     FFilterArray[Band].RMS := s * FFilterArray[Band].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
  end;
end;

procedure TAudioAmeliorationModule.VSTModuleProcessHeadphones(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
  Band    : Integer;
  d, z, s : Double;
const
  cDenorm = 1E-32;
begin
 for Channel := 0 to min(numInputs, numOutputs) - 1
  do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));

 // apply exciter
 if ExciterActive then
  for Channel := 0 to Length(FExciter) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FExciter[Channel].Process(Outputs[Channel, Sample]);

 // apply ambience
 if AmbienceActive then
  for Sample := 0 to SampleFrames - 1
   do FAmbience.Process(Outputs[0, Sample], Outputs[1, Sample]);

 // apply compression
 if CompressorActive then
  for Sample := 0 to SampleFrames - 1 do
   begin
    FCompressor.InputSample(0.5 * (Outputs[0, Sample] + Outputs[1, Sample]));
    Outputs[0, Sample] := FCompressor.GainSample(Outputs[0, Sample]);
    Outputs[1, Sample] := FCompressor.GainSample(Outputs[1, Sample]);
   end;

 // apply extra bass
 if ExtraBassActive then
  for Channel := 0 to Length(FBassEnhancer) - 1 do
   for Sample := 0 to SampleFrames - 1
    do Outputs[Channel, Sample] := FBassEnhancer[Channel].Process(Outputs[Channel, Sample]);

 for Sample := 0 to SampleFrames - 1
  do FCrosstalkSimulator.Process(Outputs[0, Sample], Outputs[1, Sample]);

 // apply limiter
 for Sample := 0 to SampleFrames - 1 do
  begin
   FLimiter.InputSample(0.5 * (Outputs[0, Sample] + Outputs[1, Sample]));
   Outputs[0, Sample] := Limit(1.3 * FastTanhMinError4(FLimiter.GainSample(Outputs[0, Sample])));
   Outputs[1, Sample] := Limit(1.3 * FastTanhMinError4(FLimiter.GainSample(Outputs[1, Sample])));
  end;

 for Sample := 0 to SampleFrames - 1 do
  begin
   d := 0.5 * (Outputs[0, Sample] + Outputs[1, Sample]);
   for Band := 0 to CNumFrequencies - 1 do
    begin
     if (FDownSampleCount mod FFilterArray[Band].Downsampling) <> 0
      then Break;

     d := FFilterArray[Band].Lowpass.ProcessSample(d + cDenorm);
     z := FFilterArray[Band].Highpass.ProcessSample(d + cDenorm);

     s := IntPower(FSpeedConst[0], 8 * FFilterArray[Band].Downsampling + 1);
     FFilterArray[Band].RMS := s * FFilterArray[Band].RMS + (1 - s) * Amp_to_dB(abs(z));
    end;
   inc(FDownSampleCount);
   if FDownSampleCount >= FDownSampleMax then FDownSampleCount := 0;
  end;
end;

procedure TAudioAmeliorationModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
begin
 for Channel := 0 to min(numInputs, numOutputs) - 1
  do Move(Inputs[Channel, 0], Outputs[Channel, 0], SampleFrames * SizeOf(Single));
end;

end.
