unit DAV_DspPsychoacousticBassEnhancer;

interface

{$I ..\DAV_Compiler.inc}

uses 
  DAV_Common, DAV_DspCommon, DAV_DspFilter, DAV_DspButterworthFilter,
  DAV_DspFilterLinkwitzRiley, DAV_DspDynamics, DAV_DspLightweightDynamics;

type
  THighpassSelect = (hpDC, hp1stOrder, hp2ndOrder);

  TCustomPsychoAcousticBassEnhancer = class(TDspObject)
  private
    FFrequency      : Single;
    FSampleRate     : Single;

    procedure SetSampleRate(const Value: Single);
    procedure SetFrequency(const Value: Single);
  protected
    procedure FrequencyChanged; virtual; abstract;
    procedure SampleRateChanged; virtual; abstract;
  public
    constructor Create; virtual;
    function Process(Input: Single): Single; virtual; abstract;

    property Frequency: Single read FFrequency write SetFrequency;
    property SampleRate: Single read FSampleRate write SetSampleRate;
  end;

  TCustomHarmonicBass = class(TCustomPsychoAcousticBassEnhancer)
  private
    FDecay          : Single;
    FGains          : array [0..3] of Single;
    FRatio          : Single;
    FResponse       : Single;
    procedure SetHighpassSelect(const Value: THighpassSelect);
    procedure SetRatio(const Value: Single);
    procedure SetDecay(const Value: Single);
    procedure SetResponse(const Value: Single);
  protected
    FCrossover      : TButterworthSplitBandFilter;
    FDrive          : Single;
    FHighpass       : TButterworthHighpassFilter;
    FHighpassSelect : THighpassSelect;
    FLimiter        : TLightweightSoftKneeLimiter;
    FUpwardComp     : TLightweightSoftKneeUpwardCompressor;

    procedure DecayChanged; virtual;
    procedure HighpassSelectChanged; virtual;
    procedure RatioChanged; virtual;
    procedure ResponseChanged; virtual;
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Process(Input: Single): Single; override;

    property Ratio: Single read FRatio write SetRatio;
    property Response: Single read FResponse write SetResponse;
    property InputLevel: Single read FGains[0] write FGains[0];
    property HighFrequencyLevel: Single read FGains[1] write FGains[1];
    property OriginalBassLevel: Single read FGains[2] write FGains[2];
    property HarmonicBassLevel: Single read FGains[3] write FGains[3];
    property Decay: Single read FDecay write SetDecay;
    property HighpassSelect: THighpassSelect read FHighpassSelect write SetHighpassSelect;
  end;

  TCustomDownsampledHarmonicBass = class(TCustomHarmonicBass)
  private
    FResamplingRatio : Single;
    FDownsamplePos     : Single;
    FLastInputSamples  : TDAV4SingleArray;
    FLastOutputSamples : TDAV4SingleArray;
    procedure CalculateResamplingRatio;
  protected
    procedure SampleRateChanged; override;
    procedure FrequencyChanged; override;
  public
    constructor Create; override;
    function Process(Input: Single): Single; override;
  end;

  TCustomLinkwitzBass = class(TCustomPsychoAcousticBassEnhancer)
  private
    FDecay    : Single;
    FGains    : array [0..3] of Single;
    FDrive    : Single;
    FResponse : Single;
    procedure SetHighpassSelect(const Value: THighpassSelect);
    procedure SetDecay(const Value: Single);
    procedure SetResponse(const Value: Single);
    procedure SetDrive(const Value: Single);
  protected
    FCrossover      : TLinkwitzRiley;
    FHighpass       : TButterworthHighpassFilter;
    FHighpassSelect : THighpassSelect;
    FLimiter        : TLightweightSoftKneeLimiter;

    procedure ResponseChanged; virtual;
    procedure DecayChanged; virtual;
    procedure HighpassSelectChanged; virtual;
    procedure DriveChanged; virtual;
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Process(Input: Single): Single; override;

    property Drive: Single read FDrive write SetDrive;
    property Response: Single read FResponse write SetResponse;
    property InputLevel: Single read FGains[0] write FGains[0];
    property HighFrequencyLevel: Single read FGains[1] write FGains[1];
    property OriginalBassLevel: Single read FGains[2] write FGains[2];
    property HarmonyBassLevel: Single read FGains[3] write FGains[3];
    property Decay: Single read FDecay write SetDecay;
    property HighpassSelect: THighpassSelect read FHighpassSelect write SetHighpassSelect;
  end;

  TCustomResurrectionBass = class(TCustomPsychoAcousticBassEnhancer)
  private
    FAddOriginalBass : Boolean;
    FGain            : Single;
    FIntensity       : Single;
    procedure SetAddOriginalBass(const Value: Boolean);
    procedure SetGain(const Value: Single);
    procedure SetIntensity(const Value: Single);
  protected
    FCrossover   : TButterworthSplitBandFilter;
    FHighpass    : TButterworthHighpassFilter;
    FLimiter     : TLightweightSoftKneeLimiter;
    FGains       : array [0..3] of Single;

    procedure AddOriginalBassChanged; virtual;
    procedure GainChanged; virtual;
    procedure IntensityChanged; virtual;
    procedure FrequencyChanged; override;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    function Process(Input: Single): Single; override;

    property Intensity: Single read FIntensity write SetIntensity;
    property Gain: Single read FGain write SetGain;
    property AddOriginalBass: Boolean read FAddOriginalBass write SetAddOriginalBass;
  end;

  THarmonicBass = class(TCustomHarmonicBass)
  published
    property Decay;
    property Frequency;
    property HarmonicBassLevel;
    property HighFrequencyLevel;
    property HighpassSelect;
    property InputLevel;
    property OriginalBassLevel;
    property Ratio;
    property Response;
    property SampleRate;
  end;

  TDownsampledHarmonicBass = class(TCustomDownsampledHarmonicBass)
  published
    property Decay;
    property Frequency;
    property HarmonicBassLevel;
    property HighFrequencyLevel;
    property HighpassSelect;
    property InputLevel;
    property OriginalBassLevel;
    property Ratio;
    property Response;
    property SampleRate;
  end;

  TLinkwitzBass = class(TCustomLinkwitzBass)
  published
    property Decay;
    property Drive;
    property Frequency;
    property HarmonyBassLevel;
    property HighFrequencyLevel;
    property HighpassSelect;
    property InputLevel;
    property OriginalBassLevel;
    property Response;
    property SampleRate;
  end;

  TResurrectionBass = class(TCustomResurrectionBass)
  published
    property AddOriginalBass;
    property Frequency;
    property Gain;
    property Intensity;
    property SampleRate;
  end;

implementation

uses
  SysUtils, DAV_DspInterpolation;

{ TCustomPsychoAcousticBassEnhancer }

constructor TCustomPsychoAcousticBassEnhancer.Create;
begin
 FSampleRate := 44100;
 FFrequency := 80;
end;

procedure TCustomPsychoAcousticBassEnhancer.SetFrequency(const Value: Single);
begin
 if FFrequency <> Value then
  begin
   FFrequency := Value;
   FrequencyChanged;
  end;
end;

procedure TCustomPsychoAcousticBassEnhancer.SetSampleRate(const Value: Single);
begin
 if FSampleRate <> Value then
  begin
   FSampleRate := Value;
   SampleRateChanged;
  end;
end;

{ TCustomHarmonicBass }

constructor TCustomHarmonicBass.Create;
begin
 inherited;
 FUpwardComp := TLightweightSoftKneeUpwardCompressor.Create;
 FUpwardComp.SampleRate := SampleRate;
 FUpwardComp.Threshold_dB := -6;
 FUpwardComp.Knee_dB := 6;

 // create & setup limiter
 FLimiter := TLightweightSoftKneeLimiter.Create;
 FLimiter.Knee_dB := 0;
 FLimiter.Release := 20;
 FLimiter.SampleRate := SampleRate;

 FCrossover := TButterworthSplitBandFilter.Create(3);
 FCrossover.SampleRate := SampleRate;

 // create & setup highpass filter
 FHighpass := TButterworthHighpassFilter.Create(2);
 FHighpass.SampleRate := SampleRate;
 FHighpass.Frequency  := 16;

 FGains[0] := 1;
 FGains[1] := 1;
 FGains[2] := 1;
 FGains[3] := 0;

 FrequencyChanged;
end;

destructor TCustomHarmonicBass.Destroy;
begin
 FreeAndNil(FUpwardComp);
 FreeAndNil(FLimiter);

 FreeAndNil(FCrossover);

 FreeAndNil(FHighpass);

 inherited;
end;

procedure TCustomHarmonicBass.SetDecay(const Value: Single);
begin
 if FDecay <> Value then
  begin
   FDecay := Value;
   DecayChanged;
  end;
end;

procedure TCustomHarmonicBass.SetHighpassSelect(
  const Value: THighpassSelect);
begin
 if FHighpassSelect <> Value then
  begin
   FHighpassSelect := Value;
   HighpassSelectChanged;
  end;
end;

procedure TCustomHarmonicBass.SetRatio(const Value: Single);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;

procedure TCustomHarmonicBass.SetResponse(const Value: Single);
begin
 if FResponse <> Value then
  begin
   FResponse := Value;
   ResponseChanged;
  end;
end;

procedure TCustomHarmonicBass.DecayChanged;
begin

end;

procedure TCustomHarmonicBass.ResponseChanged;
begin
 FLimiter.Attack := FResponse;
 FLimiter.Release := FResponse;
 FUpwardComp.Attack := FResponse;
 FUpwardComp.Release := FResponse;
end;

procedure TCustomHarmonicBass.RatioChanged;
begin
 FDrive := FRatio;
 FUpwardComp.Ratio := FRatio;
end;

procedure TCustomHarmonicBass.FrequencyChanged;
begin
 if FHighpassSelect in [hp1stOrder, hp2ndOrder]
  then FHighpass.Frequency := 0.5 * FFrequency;

 FCrossover.Frequency := FFrequency;
end;

procedure TCustomHarmonicBass.HighpassSelectChanged;
begin
 case FHighpassSelect of
  hpDC :
   begin
    FHighpass.Order := 2;
    FHighpass.Frequency := 16;
   end;
  hp1stOrder :
   begin
    FHighpass.Order := 1;
    FHighpass.Frequency := FFrequency;
   end;
  hp2ndOrder :
   begin
    FHighpass.Order := 2;
    FHighpass.Frequency := FFrequency;
   end;
 end;
end;

function TCustomHarmonicBass.Process(Input: Single): Single;
var
  Low, High, Harmonic : Single;
begin
(*
 result := FUpwardComp.ProcessSample(Input);
 continue;
*)

(*
 result := FDecay + Input * (1 + Input * -2 * FDecay);
 continue;
*)

(*
 result := FLimiter.ProcessSample(2 * Input);
 result := Limit(0.5 * result);
 continue;
*)

 FCrossover.ProcessSample(FGains[0] * Input, Low, High);

 Harmonic := 0.5 * FUpwardComp.ProcessSample(
         Limit(0.5 * FLimiter.ProcessSample(4 *
         FHighpass.ProcessSample(
         FDecay + Low * (1 + Low * -2 * FDecay)))));

 result := FGains[2] * Low + FGains[3] * Harmonic + FGains[1] * High;
end;

procedure TCustomHarmonicBass.SampleRateChanged;
begin
 FUpwardComp.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate;
 FCrossover.SampleRate := SampleRate;

 FLimiter.SampleRate := SampleRate;
end;

{ TCustomDownsampledHarmonicBass }

constructor TCustomDownsampledHarmonicBass.Create;
begin
 inherited;
 CalculateResamplingRatio;
end;

procedure TCustomDownsampledHarmonicBass.FrequencyChanged;
begin
 inherited;
 CalculateResamplingRatio;
end;

procedure TCustomDownsampledHarmonicBass.SampleRateChanged;
begin
 inherited;
 CalculateResamplingRatio;
end;

procedure TCustomDownsampledHarmonicBass.CalculateResamplingRatio;
begin
 FHighpass.SampleRate := (1 shl 7) * FFrequency;
 FResamplingRatio := FHighpass.SampleRate / SampleRate;
end;

function TCustomDownsampledHarmonicBass.Process(Input: Single): Single;
var
  Low, High, Harmonic : Single;
begin
 FCrossover.ProcessSample(FGains[0] * Input, Low, High);

 Move(FLastInputSamples[1], FLastInputSamples[0], 3 * SizeOf(Single));
 FLastInputSamples[3] := Low;

 FDownsamplePos := FDownsamplePos + FResamplingRatio;
 while FDownsamplePos >= 1 do
  begin
   FDownsamplePos := FDownsamplePos - 1;

   Harmonic := Hermite32_asm(FDownsamplePos, @FLastInputSamples[0]);

   Harmonic := //0.5 * FUpwardComp.ProcessSample(
            Limit(0.5 * FDrive* FLimiter.ProcessSample(4 *
            FHighpass.ProcessSample(
            FDecay + Harmonic * (1 + Harmonic * -2 * FDecay))));

   Move(FLastOutputSamples[1], FLastOutputSamples[0], 3 * SizeOf(Single));
   FLastOutputSamples[3] := Harmonic;
  end;
 Harmonic := Hermite32_asm(FDownsamplePos, @FLastOutputSamples[0]);

 result := FGains[2] * Low + FGains[3] * Harmonic + FGains[1] * High;
end;


{ TCustomLinkwitzBass }

constructor TCustomLinkwitzBass.Create;
begin
 inherited;

 // create & setup limiter
 FLimiter := TLightweightSoftKneeLimiter.Create;
 FLimiter.Knee_dB := 6;
 FLimiter.Release := 20;
 FLimiter.SampleRate := SampleRate;

 // create & setup crossover
 FCrossover := TLinkwitzRiley.Create(3);
 FCrossover.SampleRate := SampleRate;

 // create & setup highpass filter
 FHighpass := TButterworthHighpassFilter.Create(2);
 FHighpass.SampleRate := SampleRate;
 FHighpass.Frequency  := 16;

 FGains[0] := 1;
 FGains[1] := 1;
 FGains[2] := 1;
 FGains[3] := 0;

 FrequencyChanged;
end;

destructor TCustomLinkwitzBass.Destroy;
begin
 FreeAndNil(FLimiter);
 FreeAndNil(FCrossover);
 FreeAndNil(FHighpass);
 inherited;
end;

procedure TCustomLinkwitzBass.SetDecay(const Value: Single);
begin
 if FDecay <> Value then
  begin
   FDecay := Value;
   DecayChanged;
  end;
end;

procedure TCustomLinkwitzBass.SetHighpassSelect(
  const Value: THighpassSelect);
begin
 if FHighpassSelect <> Value then
  begin
   FHighpassSelect := Value;
   HighpassSelectChanged;
  end;
end;

procedure TCustomLinkwitzBass.SetDrive(const Value: Single);
begin
 if FDrive <> Value then
  begin
   FDrive := Value;
   DriveChanged;
  end;
end;

procedure TCustomLinkwitzBass.SetResponse(const Value: Single);
begin
 if FResponse <> Value then
  begin
   FResponse := Value;
   ResponseChanged;
  end;
end;

procedure TCustomLinkwitzBass.DecayChanged;
begin
 // yet empty
end;

procedure TCustomLinkwitzBass.DriveChanged;
begin
 // yet empty
end;

procedure TCustomLinkwitzBass.ResponseChanged;
begin
 FLimiter.Attack := FResponse;
 FLimiter.Release := FResponse;
end;

procedure TCustomLinkwitzBass.FrequencyChanged;
begin
 if FHighpassSelect in [hp1stOrder, hp2ndOrder]
  then FHighpass.Frequency := 0.5 * FFrequency;

 FCrossover.Frequency := FFrequency;
end;

procedure TCustomLinkwitzBass.HighpassSelectChanged;
begin
 case FHighpassSelect of
  hpDC :
   begin
    FHighpass.Order := 2;
    FHighpass.Frequency := 16;
   end;
  hp1stOrder :
   begin
    FHighpass.Order := 1;
    FHighpass.Frequency := FFrequency;
   end;
  hp2ndOrder :
   begin
    FHighpass.Order := 2;
    FHighpass.Frequency := FFrequency;
   end;
 end;
end;

function TCustomLinkwitzBass.Process(Input: Single): Single;
var
  Low, High, Harmonic : Single;
begin
 FCrossover.ProcessSample(FGains[0] * Input, Low, High);

 Harmonic := Limit(0.5 * FDrive* FLimiter.ProcessSample(4 *
         FHighpass.ProcessSample(
         FDecay + Low * (1 + Low * -2 * FDecay))));

 result := FGains[2] * Low + FGains[3] * Harmonic + FGains[1] * High;
end;

procedure TCustomLinkwitzBass.SampleRateChanged;
begin
 FCrossover.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate;
 FLimiter.SampleRate := SampleRate;
end;

{ TCustomResurrectionBass }

constructor TCustomResurrectionBass.Create;
begin
 inherited;

 // create & setup crossover
 FCrossover := TButterworthSplitBandFilter.Create(3);
 FCrossover.SampleRate := SampleRate;

 // create & setup highpass filter
 FHighpass := TButterworthHighpassFilter.Create(2);
 FHighpass.SampleRate := SampleRate;
 FHighpass.Frequency  := 16;

 FLimiter := TLightweightSoftKneeLimiter.Create;
 FLimiter.Threshold_dB := 0;
 FLimiter.Knee_dB := 0;
 FLimiter.Attack := 25;
 FLimiter.Release := 25;

 FrequencyChanged;
end;

destructor TCustomResurrectionBass.Destroy;
begin
 FreeAndNil(FLimiter);
 FreeAndNil(FHighpass);
 FreeAndNil(FCrossover);
 inherited;
end;

procedure TCustomResurrectionBass.FrequencyChanged;
begin
 FCrossover.Frequency := FFrequency;
end;

procedure TCustomResurrectionBass.SampleRateChanged;
begin
 FCrossover.SampleRate := SampleRate;
 FLimiter.SampleRate := SampleRate;
 FHighpass.SampleRate := SampleRate;
end;

procedure TCustomResurrectionBass.SetAddOriginalBass(const Value: Boolean);
begin
 if FAddOriginalBass <> Value then
  begin
   FAddOriginalBass := Value;
   AddOriginalBassChanged;
  end;
end;

procedure TCustomResurrectionBass.SetGain(const Value: Single);
begin
 if FGain <> Value then
  begin
   FGain := Value;
   GainChanged;
  end;
end;

procedure TCustomResurrectionBass.SetIntensity(const Value: Single);
begin
 if FIntensity <> Value then
  begin
   FIntensity := Value;
   IntensityChanged;
  end;
end;

procedure TCustomResurrectionBass.IntensityChanged;
begin
 FGains[0] := dB_to_Amp(FIntensity);
end;

procedure TCustomResurrectionBass.AddOriginalBassChanged;
begin
 FGains[1] := Integer(FAddOriginalBass);
end;

procedure TCustomResurrectionBass.GainChanged;
begin
 FGains[2] := FGain;
end;

function TCustomResurrectionBass.Process(Input: Single): Single;
var
  Low, High, Harmonic : Single;
begin
 FCrossover.ProcessSample(Input, Low, High);

 Harmonic := FGains[0] * Low;
 Harmonic := FHighpass.ProcessSample(0.2 + Harmonic * (1 - 0.4 * Harmonic));
 Harmonic := 0.5 * FLimiter.ProcessSample(Harmonic);
 Harmonic := Limit(Harmonic);

 result := FGains[2] * (FGains[1] * Low + Harmonic + High);
// result := Harmonic;
end;

end.
