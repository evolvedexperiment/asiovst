unit DAV_DSPLevelingAmplifier;

interface

uses
  DAV_DSPDynamics;

type
  TCustomLevelingAmplifier = class
    function GetGainReductiondB: Double;
    function GetInput_dB: Double;
    function GetOutput_dB: Double;
    procedure SetKnee(const Value: Double);
    procedure SetAttack_ms(const Value: Double);
    procedure SetInput_dB(const Value: Double);
    procedure SetOutput_dB(const Value: Double);
    procedure SetRelease_ms(const Value: Double);
  private
    procedure SetThreshold(const Value: Double);
  protected
    fOldInput        : Double;
    FInputLevel      : Double;
    FOutputLevel     : Double;
    FPeak            : Double;
    FKnee            : Double;
    FGain            : Double;
    FLevel           : Double;
    FRatio           : Double;
    fRatioReciprocal : Double;
    FSampleRate      : Double;
    FSampleRateRez   : Double;
    FRelease_ms      : Double;
    FAttack_ms       : Double;
    FReleaseFactor   : Double;
    FAttackFactor    : Double;
    FThreshold       : Double;
    FThresholdReci   : Double;
    FMakeUpGain      : array [0..2] of Double;
//    fInternal        : array [0..3] of Double;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetRatio(const Value: Double); virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
    procedure SampleRateChanged; virtual;
  public
    constructor Create; virtual;
    function TranslatePeakToGain(PeakLevel: Double): Double; virtual;
    function CharacteristicCurve(InputLevel: Double): Double; virtual;
    function CharacteristicCurve_dB(InputLevel_dB: Double): Double; virtual;
    function ProcessSample(const Input : Double): Double; virtual;
    procedure Sidechain(const Input : Double); virtual;

    property GainReductionFactor : Double read FGain;            // in dB
    property GainReduction_dB : Double read GetGainReductiondB;  // in dB
    property Input_dB: Double read GetInput_dB write SetInput_dB;
    property Output_dB: Double read GetOutput_dB write SetOutput_dB;
    property Attack_ms: Double read FAttack_ms write SetAttack_ms;
    property Release_ms: Double read FRelease_ms write SetRelease_ms;
    property Ratio: Double read FRatio write SetRatio;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property Threshold: Double read FThreshold write SetThreshold;
    property Knee: Double read FKnee write SetKnee;
  end;

  TLevelingAmplifier = class(TCustomLevelingAmplifier)
  published
    property Input_dB;
    property Output_dB;
    property Attack_ms;
    property Release_ms;
    property Ratio;
    property SampleRate;
    property Knee;
  end;

  TLevelingAmplifierProgramDependentRelease = class(TCustomLevelingAmplifier)
  private
    FMinReleaseFactor : Double;
    FMaxReleaseFactor : Double;
    fProgramDependency: Double;
    procedure SetProgramDependency(const Value: Double);
  protected
    procedure CalculateReleaseFactor; override;
  public
    constructor Create; override;
    procedure Sidechain(const Input : Double); override;
  published
    property Input_dB;
    property Output_dB;
    property Attack_ms;
    property Release_ms;
    property ProgramDependency: Double read fProgramDependency write SetProgramDependency;
    property Ratio;
    property SampleRate;
    property Knee;
  end;

implementation

uses
  Math, DAV_Common;

const 
  Harms : array [0..3] of Single = (1.4092750123e-07, -7.5166615806e-07,
                                    1.1523939535e-06, -6.3117489523e-07);

{ TCustomLevelingAmplifier }

constructor TCustomLevelingAmplifier.Create;
begin
  FSampleRate      := 44100;
  FSampleRateRez   := 1 / fSampleRate;
  FRatio           := 1;
  FAttack_ms       := 5;
  FRelease_ms      := 5;
  FLevel           := 0;
  FKnee            := 0.5;
  FRatio           := 1;
  FRatioReciprocal := 1;
  FThreshold       := 1;
  FThresholdReci   := 1;
  CalculateAttackFactor;
  CalculateReleaseFactor;
end;

procedure TCustomLevelingAmplifier.CalculateAttackFactor;
begin
 if FAttack_ms = 0
  then FAttackFactor := 0
  else FAttackFactor := exp( -ln2 / (FAttack_ms * 0.001 * SampleRate));
end;

procedure TCustomLevelingAmplifier.CalculateReleaseFactor;
begin
 if FRelease_ms = 0
  then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease_ms * 0.001 * SampleRate));
end;

function TCustomLevelingAmplifier.CharacteristicCurve(InputLevel: Double): Double;
begin
 result := TranslatePeakToGain(InputLevel) * InputLevel;
end;

function TCustomLevelingAmplifier.CharacteristicCurve_dB(InputLevel_dB: Double): Double;
begin
 result := Amp_to_dB(1E-30 + abs(CharacteristicCurve(dB_to_Amp(InputLevel_dB))));
end;

function TCustomLevelingAmplifier.GetGainReductiondB: Double;
begin
 result := Amp_to_dB(FGain);
end;

function TCustomLevelingAmplifier.GetInput_dB: Double;
begin
 result := Amp_to_dB(FInputLevel);
end;

function TCustomLevelingAmplifier.GetOutput_dB: Double;
begin
 result := Amp_to_dB(FOutputLevel);
end;

procedure TCustomLevelingAmplifier.SetAttack_ms(const Value: Double);
begin
 if FAttack_ms <> abs(Value) then
  begin
   FAttack_ms := abs(Value);
   CalculateAttackFactor;
  end;
end;

procedure TCustomLevelingAmplifier.SetInput_dB(const Value: Double);
begin
 FInputLevel := dB_to_Amp(Value);
end;

procedure TCustomLevelingAmplifier.SetKnee(const Value: Double);
begin
 if fKnee <> Value then
  begin
   fKnee := Value;
  end;
end;

procedure TCustomLevelingAmplifier.SetOutput_dB(const Value: Double);
begin
 FOutputLevel := dB_to_Amp(Value);
end;

procedure TCustomLevelingAmplifier.SetRatio(const Value: Double);
begin
 if fRatio <> Value then
  begin
   fRatio := Value;
   fRatioReciprocal := 1 / fRatio;
  end;
end;

procedure TCustomLevelingAmplifier.SetRelease_ms(const Value: Double);
begin
 if FRelease_ms <> abs(Value) then
  begin
   FRelease_ms := abs(Value);
   CalculateReleaseFactor;
  end;
end;

procedure TCustomLevelingAmplifier.SetSampleRate(const Value: Double);
begin
 if fSampleRate <> Value then
  begin
   fSampleRate := Value;
   fSampleRateRez := 1 / fSampleRate;
   SampleRateChanged;
  end;
end;

procedure TCustomLevelingAmplifier.SampleRateChanged;
begin
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

procedure TCustomLevelingAmplifier.SetThreshold(const Value: Double);
begin
 if FThreshold <> Value then
  begin
   FThreshold := Value;
   FThresholdReci := 1 / Value;
  end;
end;

function Diode(x: Single): Single;
var
  a, b : Double;
begin
 x := x * 10;
 x := abs(x - 0.4) + x - 0.4;
 a := abs(x);
 b := 10 * (1 + sqr(a));
 Result := 0.005 * (x + (b * x) / (b * a + 1));
end;

function SimpleDiode(x: Single): Single;
begin
 Result := 0.5 * (abs(x) + x);
end;

procedure TCustomLevelingAmplifier.Sidechain(const Input: Double);
var
  Value : Double;
begin
 // apply feedback gain and input level gain
 Value := Input * FInputLevel;

 // smooth input by attack
 fOldInput := abs(Value) + (fOldInput - abs(Value)) * fAttackFactor;

 // add fall off (released) caused by electroluminicence effect of an LED
 fPeak := fPeak * fReleaseFactor;

 // calculate difference to current peak level
 Value := SimpleDiode(fOldInput - fPeak);

 // apply release phase
 fPeak := fPeak + Value;

 fGain := TranslatePeakToGain(fPeak);
end;

function TCustomLevelingAmplifier.TranslatePeakToGain(PeakLevel: Double): Double;
var
  d : Double;
begin
  begin
   d := SimpleDiode(PeakLevel * FThresholdReci - sqr(1 - Knee));
   d := (d - sqr(Knee) * FastTanhOpt3(d));
   result := Power(1 + d, fRatio * (1 - fRatioReciprocal));
  end;
end;

function TCustomLevelingAmplifier.ProcessSample(const Input: Double): Double;
begin
 result := FInputLevel * (Harms[0] + Input * (1 + Input *
           (Harms[1] + sqr(Input) * (Harms[2] + sqr(Input) * Harms[3]))));
 result := FOutputLevel * fGain * result;
end;

{ TLevelingAmplifierProgramDependentRelease }

procedure TLevelingAmplifierProgramDependentRelease.CalculateReleaseFactor;
begin
 inherited;
 FMinReleaseFactor := Power(FReleaseFactor,     fProgramDependency);
 FMaxReleaseFactor := Power(FReleaseFactor, 1 / fProgramDependency);
end;

constructor TLevelingAmplifierProgramDependentRelease.Create;
begin
 inherited;
 fProgramDependency := 2;
end;

procedure TLevelingAmplifierProgramDependentRelease.SetProgramDependency(
  const Value: Double);
begin
 if fProgramDependency <> Value then
  begin
   fProgramDependency := Value;
   CalculateReleaseFactor;
  end;
end;

procedure TLevelingAmplifierProgramDependentRelease.Sidechain(
  const Input: Double);
var
  Value : Double;
begin
 // apply feedback gain and input level gain
 Value := Input * FInputLevel;

 // smooth input by attack
 fOldInput := abs(Value) + (fOldInput - abs(Value)) * fAttackFactor;

 // add fall off (released) caused by electroluminicence effect of an LED
 fPeak := fPeak * fReleaseFactor;

 // calculate difference to current peak level
 Value := SimpleDiode(fOldInput - fPeak);

 // apply release phase
 fPeak := fPeak + Value;

 fGain := TranslatePeakToGain(fPeak);
end;

end.
