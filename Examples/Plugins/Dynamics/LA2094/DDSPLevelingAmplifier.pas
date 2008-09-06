unit DDSPLevelingAmplifier;

interface

uses
  DDSPDynamics;

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
    procedure CalculateInternals;
  protected
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
    fInternal        : array [0..3] of Double;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetRatio(const Value: Double); virtual;
    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
  public
    function ProcessSample(Input : Double): Double; virtual;
    procedure Sidechain(Input : Double); virtual;
    constructor Create; virtual;

    property GainReductionFactor : Double read FGain;            // in dB
    property GainReduction_dB : Double read GetGainReductiondB;  // in dB
    property Input_dB: Double read GetInput_dB write SetInput_dB;
    property Output_dB: Double read GetOutput_dB write SetOutput_dB;
    property Attack_ms: Double read FAttack_ms write SetAttack_ms;
    property Release_ms: Double read FRelease_ms write SetRelease_ms;
    property Ratio: Double read FRatio write SetRatio;
    property SampleRate: Double read FSampleRate write SetSampleRate;
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

implementation

uses
  Math, DAVDCommon;

const 
  Harms : array [0..3] of Single = (1.4092750123e-07, -7.5166615806e-07,
                                    1.1523939535e-06, -6.3117489523e-07);

{ TCustomLevelingAmplifier }

constructor TCustomLevelingAmplifier.Create;
begin
  fSampleRate      := 44100;
  fSampleRateRez   := 1 / fSampleRate;
  fRatio           := 1;
  fAttack_ms       := 5;
  fRelease_ms      := 5;
  fLevel           := 0;
  fRatio           := 1;
  fRatioReciprocal := 1;
  CalculateAttackFactor;
  CalculateReleaseFactor;
end;

procedure TCustomLevelingAmplifier.CalculateAttackFactor;
begin
 if FAttack_ms = 0
  then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack_ms * 0.001 * SampleRate));
end;

procedure TCustomLevelingAmplifier.CalculateReleaseFactor;
begin
 if FRelease_ms = 0
  then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease_ms * 0.001 * SampleRate));
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
   CalculateInternals;
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
   CalculateInternals;
  end;
end;

procedure TCustomLevelingAmplifier.CalculateInternals;
begin
 fInternal[0] := sqrt(fRatio);
 fInternal[1] := 1 / fInternal[0];
 fInternal[2] := 0.95 + 0.0001 * 10 * fKnee * fRatio;
 fInternal[3] := 3000 / Power(2, 10 * fKnee);
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
   CalculateAttackFactor;
   CalculateReleaseFactor;
  end;
end;

procedure TCustomLevelingAmplifier.Sidechain(Input: Double);
var
  a : Double;
begin
 Input := Input * FInputLevel * fGain;
 if abs(Input) > fPeak
  then fPeak := abs(Input) + (fPeak - abs(Input)) * fAttackFactor
  else fPeak := abs(Input) + (fPeak - abs(Input)) * fReleaseFactor;

 a := Power(fPeak, fRatioReciprocal);
 fGain := (1 - fInternal[2] * a / (a + fInternal[3])) * fInternal[1];
end;

function TCustomLevelingAmplifier.ProcessSample(Input: Double): Double;
begin
 Input  := FInputLevel * Input;
 result := Harms[0] + Input * (1 + Input * (Harms[1] + sqr(Input) *
           (Harms[2] + sqr(Input) * Harms[3])));
 result := FOutputLevel * fGain * fInternal[0] * Input;
end;

end.
