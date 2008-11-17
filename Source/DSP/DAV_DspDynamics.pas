unit DAV_DspDynamics;

interface

{$I ..\ASIOVST.INC}

uses
  DAV_Common, DAV_DspCommon, DAV_DspButterworthFilter;

type
  TCustomDynamics = class(TDspObject)
  private
    procedure SetAttack(const Value: Double);
    procedure SetDecay(const Value: Double);
    procedure SetSampleRate(const Value: Double);
    procedure SetThreshold(const Value: Double);
    procedure SetRatio(const Value: Double);
    function GetGainReductiondB: Double;
  protected
    FPeak          : Double;
    FGain          : Double;
    FLevel         : Double;
    FRatio         : Double;
    FSampleRate    : Double;
    FSampleRateRez : Double;
    FThreshold     : Double;
    FThresholddB   : Double;
    FDecay         : Double;
    FAttack        : Double;
    FDecayFactor   : Double;
    FAttackFactor  : Double;
    procedure RatioChanged; virtual; abstract;
    procedure SampleRateChanged; virtual;
    procedure ThresholdChanged; virtual;
    procedure AttackChanged; virtual;
    procedure DecayChanged; virtual;
    procedure CalculateAttackFactor; virtual; abstract;
    procedure CalculateDecayFactor; virtual; abstract;
    procedure CalculateThreshold; virtual; abstract;
  public
    function ProcessSample(Input : Double):Double; virtual; abstract;
    constructor Create; virtual;
    function TranslatePeakToGain(const PeakLevel: Double): Double; virtual; abstract;
    function CharacteristicCurve(const InputLevel: Double): Double; virtual;
    function CharacteristicCurve_dB(const InputLevel_dB: Double): Double; virtual;

    property Threshold: Double read FThresholddB write SetThreshold;  // in dB
    property Ratio: Double read FRatio write SetRatio;
    property Attack: Double read FAttack write SetAttack;             // in ms
    property Decay: Double read FDecay write SetDecay;                // in ms
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property GainReductionFactor: Double read FGain;                  // in dB
    property GainReductiondB: Double read GetGainReductiondB;         // in dB
  end;

  TDynamics = class(TCustomDynamics)
  protected
    procedure CalculateAttackFactor; override;
    procedure CalculateDecayFactor; override;
    procedure CalculateThreshold; override;
  published
    property Threshold;
    property Ratio;
    property Attack;
    property Decay;
    property SampleRate;
    property GainReductionFactor;
    property GainReductiondB;
  end;

  TSimpleGate = class(TDynamics)
  private
  public
    constructor Create; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample(Input : Double):Double; override;
  end;

  TGate = class(TDynamics)
  private
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure SetHold(const Value: Double);
    procedure SetRange(const Value: Double);
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
    procedure SetKnee(const Value: Double);
  protected
    FHold        : Double;
    FKnee        : Double;
    FRange       : Double;
    FRangeFactor : Double;
    FHoldSmplCnt : Integer;
    FHoldSamples : Double;
    FSideChain   : Double;
    FDuck        : Boolean;
    FLowCut      : TButterworthHP;
    FHighCut     : TButterworthLP;
    procedure CalculateHoldSamples;
    procedure CalculateRangeFactor;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    function ProcessSample(Input : Double):Double; override;
    procedure InputSideChain(Input : Double); virtual;
  published
    property Hold : Double read FHold write SetHold;      // in s
    property Range : Double read FRange write SetRange;   // in dB
    property Knee : Double read FKnee write SetKnee;      // in dB
    property Duck : Boolean read FDuck write FDuck;       // not implemented yet
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;

  TSimpleCompressor = class(TDynamics)
  private
    procedure SetAutoMakeUp(const Value: Boolean);
    procedure SetMakeUpGaindB(const Value: Double);
  protected
    FSideChain    : Double;
    FAutoMakeUp   : Boolean;
    FMakeUpGaindB : Double;
    FMakeUpGain   : TDAV2DoubleArray;
    procedure RatioChanged; override;
    procedure ThresholdChanged; override;
    procedure CalculateMakeUpGain; virtual;
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    procedure InputSideChain(Input : Double); virtual;
    property AutoMakeUp : Boolean read FAutoMakeUp write SetAutoMakeUp;
    property MakeUpGaindB : Double read FMakeUpGaindB write SetMakeUpGaindB; //in dB
  end;

  TSimpleFeedbackCompressor = class(TSimpleCompressor)
  protected
    procedure CalculateMakeUpGain; override;
    procedure CalculateAttackFactor; override;
    procedure CalculateDecayFactor; override;
    procedure RatioChanged; override;
  public
    function ProcessSample(Input : Double): Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TSoftKneeFeedbackCompressor = class(TSimpleFeedbackCompressor)
  protected
    FRatioReciprocal     : Double;
    FThresholdReciprocal : Double;
    procedure RatioChanged; override;
    procedure ThresholdChanged; override;
  public
    function ProcessSample(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TSimpleRMSCompressor = class(TSimpleCompressor)
  private
    FRMSTime    : Double;
    procedure SetRMSTime(const Value: Double);
    procedure UpdateRMSBuffer;
  protected
    FRMSSize    : Integer;
    FRMSPos     : Integer;
    FRMSFactor  : Double;
    FRMSBuffer  : TDAVDoubleDynArray;
    FCurrentRMS : Double;
    procedure SampleRateChanged; override;
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
  published
    property RMSTime : Double read FRMSTime write SetRMSTime;  // in ms
  end;

  TCompressor = class(TSimpleRMSCompressor)
  private
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
  protected
    FLowCut      : TButterworthHP;
    FHighCut     : TButterworthLP;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure InputSideChain(Input : Double); override;
    function ProcessSample(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;

  TSimpleLimiter = class(TDynamics)
  private
    FThresholdRatioFactor : Double;
    procedure CalculateThresholdRatioFactor;
  protected
    procedure RatioChanged; override;
    procedure ThresholdChanged; override;
  public
    function ProcessSample(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  end;

  TSoftKneeLimiter = class(TSimpleLimiter)
  private
    FSoftKnee: Double;
    procedure SetSoftKnee(const Value: Double);
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
  published
    property SoftKnee : Double read FSoftKnee write SetSoftKnee;
  end;

  TSoftKneeFeedbackLimiter = class(TSoftKneeLimiter)
  protected
    FOversample : Integer;
    FFilter     : TButterworthLowCut;
    FAttackFac2 : Double;
    FDecayFac2  : Double;
    FPeak2      : Double;
    procedure CalculateAttackFactor; override;
    procedure CalculateDecayFactor; override;
    procedure SampleRateChanged; override;
  public
    function ProcessSample(Input : Double):Double; override;
    function TranslatePeakToGain(const PeakLevel: Double): Double; override;
    constructor Create; override;
  end;

implementation

uses
  SysUtils, Dialogs, Math;

{ TCustomDynamics }

constructor TCustomDynamics.Create;
begin
  FSampleRate := 44100;
  FSampleRateRez := 1 / FSampleRate;
  FThresholddB := -40;
  FRatio := 1;
  FAttack := 5;
  FDecay := 5;
  FLevel := 0;
  FRatio := 1;
  CalculateThreshold;
  CalculateAttackFactor;
  CalculateDecayFactor;
end;

procedure TCustomDynamics.SetSampleRate(const Value: Double);
begin
  if FSampleRate <> Value then
  begin
    FSampleRate := Value;
    FSampleRateRez := 1 / FSampleRate;
    SampleRateChanged;
  end;
end;

function TCustomDynamics.GetGainReductiondB: Double;
begin
 result := Amp_to_dB(FGain);
end;

procedure TCustomDynamics.DecayChanged;
begin
 CalculateDecayFactor;
end;

procedure TCustomDynamics.AttackChanged;
begin
 CalculateAttackFactor;
end;

function TCustomDynamics.CharacteristicCurve(const InputLevel: Double): Double;
begin
 result := TranslatePeakToGain(InputLevel) * InputLevel;
end;

function TCustomDynamics.CharacteristicCurve_dB(const InputLevel_dB: Double): Double;
begin
 result := Amp_to_dB(CharacteristicCurve(dB_to_Amp(InputLevel_dB)));
end;

procedure TCustomDynamics.SetRatio(const Value: Double);
begin
 if FRatio <> Value then
  begin
   FRatio := Value;
   RatioChanged;
  end;
end;

procedure TCustomDynamics.SampleRateChanged;
begin
 CalculateAttackFactor;
 CalculateDecayFactor;
end;

procedure TCustomDynamics.SetAttack(const Value: Double);
begin
  if FAttack <> Value then
  begin
    FAttack := abs(Value);
    AttackChanged;
  end;
end;

procedure TCustomDynamics.SetDecay(const Value: Double);
begin
  if FDecay <> Value then
  begin
    FDecay := abs(Value);
    DecayChanged;
  end;
end;

procedure TCustomDynamics.SetThreshold(const Value: Double);
begin
  if FThresholddB <> Value then
  begin
    FThresholddB := Value;
    ThresholdChanged;
  end;
end;

procedure TCustomDynamics.ThresholdChanged;
begin
 CalculateThreshold;
end;

{TDynamics}

procedure TDynamics.CalculateThreshold;
begin
  FThreshold := dB_to_Amp(FThresholddB);
end;

procedure TDynamics.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate));
end;

procedure TDynamics.CalculateDecayFactor;
begin
  if FDecay = 0 then FDecayFactor := 0
  else FDecayFactor := exp( -ln2 / (FDecay * 0.001 * SampleRate));
end;

{ TSimpleGate }

constructor TSimpleGate.Create;
begin
  inherited;
end;

function TSimpleGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
  if abs(PeakLevel) < FThreshold
   then Result := 0
   else Result := PeakLevel;
end;

function TSimpleGate.ProcessSample(Input: Double): Double;
begin
  if abs(Input) < FThreshold
   then Result := 0
   else Result := Input;
end;

{ TGate }

constructor TGate.Create;
begin
  inherited;
  FGain              := 1;
  FHold              := 0.01;
  FHoldSmplCnt       := 0;
  FLowCut            := TButterworthHP.Create;
  FHighCut           := TButterworthLP.Create;
  FLowCut.Frequency  := 20;
  FHighCut.Frequency := 20000;
  CalculateHoldSamples;
end;

destructor TGate.Destroy;
begin
  FreeAndNil(FLowCut);
  FreeAndNil(FHighCut);
  inherited;
end;

function TGate.GetHighCut: Double;
begin
 result := FHighCut.Frequency;
end;

function TGate.GetLowCut: Double;
begin
 result := FLowCut.Frequency;
end;

procedure TGate.InputSideChain(Input: Double);
begin
 FSideChain := FHighCut.ProcessSample(FLowCut.ProcessSample(Input));
end;

function TGate.ProcessSample(Input: Double): Double;
begin
 // old algorithm:
 /////////////////

 if abs(FSideChain) > FPeak
  then FPeak := FPeak + (abs(FSideChain) - FPeak) * FAttackFactor
  else FPeak := abs(FSideChain) + (FPeak - abs(FSideChain)) * FDecayFactor;

 FGain := CharacteristicCurve(FPeak);

 result := Input * FGain;
end;

procedure TGate.CalculateHoldSamples;
begin
  FHoldSamples := 0; //FHold * FSampleRate;
end;

procedure TGate.SampleRateChanged;
begin
 inherited;
 CalculateHoldSamples;
 FLowCut.SampleRate := FSampleRate;
 FHighCut.SampleRate := FSampleRate;
end;

procedure TGate.SetHighCut(const Value: Double);
begin
 FHighCut.Frequency := Value;
end;

procedure TGate.SetHold(const Value: Double);
begin
  if FHold <> Value then
  begin
    FHold := Value;
    CalculateHoldSamples;
  end;
end;

procedure TGate.SetKnee(const Value: Double);
begin
 if FKnee <> Value then
  begin
   FKnee := Value;
  end;
end;

procedure TGate.SetLowCut(const Value: Double);
begin
 FLowCut.Frequency := Value;
end;

procedure TGate.CalculateRangeFactor;
begin
 FRangeFactor := dB_to_Amp(FRange);
end;

function TGate.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if FPeak < FThreshold then
  begin
   if FHoldSmplCnt > FHoldSamples
    then result := Power(FThreshold, 1 - FRatio) * Power(PeakLevel, FRatio - 1) * (1 - FRangeFactor) + FRangeFactor
    else
     begin
      result := FGain;
      inc(FHoldSmplCnt);
     end
  end
 else
  begin
   // start hold phase
   FHoldSmplCnt := 0;
   result := FRangeFactor + (1 - FRangeFactor);
  end;
end;

procedure TGate.SetRange(const Value: Double);
begin
 if FRange <> Value then
  begin
   FRange := Value;
   CalculateRangeFactor;
  end;
end;

{ TSimpleCompressor }

constructor TSimpleCompressor.Create;
begin
  inherited;
  FPeak := 0;
  FRatio := 1;
  FMakeUpGaindB := 0;
  FAutoMakeUp := False;
  CalculateMakeUpGain;
end;

function TSimpleCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := FMakeUpGain[0]
  else result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);
end;

procedure TSimpleCompressor.ThresholdChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure TSimpleCompressor.RatioChanged;
begin
 inherited;
 CalculateMakeUpGain;
end;

procedure  TSimpleCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 dbl := Power(FThreshold, 1 - FRatio);
 if FAutoMakeUp
  then FMakeUpGain[0] := 1 / dbl
  else FMakeUpGain[0] := dB_to_Amp(FMakeUpGaindB);
 FMakeUpGain[1] := FMakeUpGain[0] * dbl;
end;

procedure TSimpleCompressor.InputSideChain(Input: Double);
begin
 FSideChain := Input;
end;

function TSimpleCompressor.ProcessSample(Input: Double): Double;
begin
 if abs(FSideChain) > FPeak
  then FPeak := FPeak + (abs(FSideChain) - FPeak) * FAttackFactor
  else FPeak := abs(FSideChain) + (FPeak - abs(FSideChain)) * FDecayFactor;

 FGain := TranslatePeakToGain(FPeak);

 FSideChain := Input;
 result := FGain * Input;
end;

procedure TSimpleCompressor.SetAutoMakeUp(const Value: Boolean);
begin
 FAutoMakeUp := Value;
 if FAutoMakeUp <> Value then
  begin
   FAutoMakeUp := Value;
   CalculateMakeUpGain;
  end;
end;

procedure TSimpleCompressor.SetMakeUpGaindB(const Value: Double);
begin
 if FMakeUpGaindB <> Value then
  begin
   FMakeUpGaindB := Value;
   if not FAutoMakeUp
    then CalculateMakeUpGain;
  end;
end;

{ TSimpleFeedbackCompressor }

procedure TSimpleFeedbackCompressor.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack / FRatio * 0.001 * SampleRate));
end;

procedure TSimpleFeedbackCompressor.CalculateDecayFactor;
begin
  if FDecay = 0 then FDecayFactor := 0
  else FDecayFactor := exp( -ln2 / (FDecay / FRatio * 0.001 * SampleRate));
end;

function TSimpleFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := 1
  else result := FMakeUpGain[1] * Power(PeakLevel, 1 - (1 / FRatio));
end;

function TSimpleFeedbackCompressor.ProcessSample(Input: Double): Double;
begin
 if abs(FSideChain) > FPeak
  then FPeak := FPeak + (abs(FSideChain) - FPeak) * FAttackFactor
  else FPeak := abs(FSideChain) + (FPeak - abs(FSideChain)) * FDecayFactor;

 FGain := TranslatePeakToGain(FPeak);

 FSideChain := FGain * Input;
 result := FMakeUpGain[0] * FSideChain; // * FMakeUpGain[0];
end;

procedure TSimpleFeedbackCompressor.RatioChanged;
begin
 inherited;
 CalculateAttackFactor;
 CalculateDecayFactor;
end;

procedure TSimpleFeedbackCompressor.CalculateMakeUpGain;
var
  dbl : Double;
begin
 dbl := Power(FThreshold, 1 - FRatio);
 if FAutoMakeUp
  then FMakeUpGain[0] := 1 / dbl
  else FMakeUpGain[0] := dB_to_Amp(FMakeUpGaindB);
 FMakeUpGain[1] := Power(FThreshold, (1 - FRatio) / FRatio);
end;

{ TSoftKneeFeedbackCompressor }

procedure TSoftKneeFeedbackCompressor.RatioChanged;
begin
 inherited;
 FRatioReciprocal := 1 / FRatio;
end;

procedure TSoftKneeFeedbackCompressor.ThresholdChanged;
begin
 inherited;
 FThresholdReciprocal := 1 / FThreshold;
end;

function TSoftKneeFeedbackCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  a : Double;
begin
 a := Power(FPeak, FRatioReciprocal);
 result := (1 - a / (a + 3)) * FThresholdReciprocal;
end;

function TSoftKneeFeedbackCompressor.ProcessSample(Input: Double): Double;
var
  a : Double;
begin
 result := FGain * Input;
 if abs(result) > FPeak
  then FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor
  else FPeak := abs(result) + (FPeak - abs(result)) * FDecayFactor;

 a := Power(FPeak, FRatioReciprocal);
 FGain := (1 - a / (a + 3)) * FThresholdReciprocal;
 result := FThreshold * result;
end;

{ TSimpleRMSCompressor }

constructor TSimpleRMSCompressor.Create;
begin
 inherited;
 FCurrentRMS := 0;
 FRMSTime := 10;
 UpdateRMSBuffer;
end;

function TSimpleRMSCompressor.ProcessSample(Input: Double): Double;
begin
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;
 FSideChain := Sqrt(FCurrentRMS * FRMSFactor);
 Result := inherited ProcessSample(Input);
end;

procedure TSimpleRMSCompressor.UpdateRMSBuffer;
var i : Integer;
begin
 i := Round(FSampleRate * 0.001 * FRMSTime);
 SetLength(FRMSBuffer, i);
 if i > FRMSSize then FillChar(FRMSBuffer[FRMSSize], (i - FRMSSize) * SizeOf(Double), 0);
 FRMSSize := i; FRMSFactor := 1 / FRMSSize;
 if FRMSPos > FRMSSize then FRMSPos := 0;
end;

procedure TSimpleRMSCompressor.SampleRateChanged;
begin
 inherited;
 UpdateRMSBuffer;
end;

procedure TSimpleRMSCompressor.SetRMSTime(const Value: Double);
begin
 if FRMSTime <> Value then
  begin
   FRMSTime := Value;
   UpdateRMSBuffer;
  end;
end;

{ TCompressor }

constructor TCompressor.Create;
begin
 inherited;
 FLowCut  := TButterworthHP.Create;
 FHighCut := TButterworthLP.Create;
 FLowCut.Frequency := 20;
 FHighCut.Frequency := 20000;
end;

destructor TCompressor.Destroy;
begin
 FreeAndNil(FLowCut);
 FreeAndNil(FHighCut);
 inherited;
end;

function TCompressor.GetHighCut: Double;
begin
 result := FHighCut.Frequency;
end;

function TCompressor.GetLowCut: Double;
begin
 result := FLowCut.Frequency;
end;

function TCompressor.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := FMakeUpGain[0]
  else result := FMakeUpGain[1] * Power(PeakLevel, FRatio - 1);
end;

procedure TCompressor.InputSideChain(Input: Double);
begin
 Input := FHighCut.ProcessSample(FLowCut.ProcessSample(Input));
// FSideChain := Input;
 FCurrentRMS := FCurrentRMS - FRMSBuffer[FRMSPos] + sqr(Input);
 if FCurrentRMS < 0 then FCurrentRMS := 0;
 FRMSBuffer[FRMSPos] := sqr(Input);
 if FRMSPos < FRMSSize then inc(FRMSPos) else FRMSPos := 0;
 FSideChain := Sqrt(FCurrentRMS * FRMSFactor);
end;

function TCompressor.ProcessSample(Input: Double): Double;
begin
 if abs(FSideChain) > FPeak
  then FPeak := FPeak + (abs(FSideChain) - FPeak) * FAttackFactor
  else FPeak := abs(FSideChain) + (FPeak - abs(FSideChain)) * FDecayFactor;

 FGain := CharacteristicCurve(FPeak);

 result := FGain * Input;
 FSideChain := abs(result);
end;

procedure TCompressor.SetHighCut(const Value: Double);
begin
 if FHighCut.Frequency <> Value
  then FHighCut.Frequency := Value;
end;

procedure TCompressor.SetLowCut(const Value: Double);
begin
 if FLowCut.Frequency <> Value
  then FLowCut.Frequency := Value;
end;

{ TSimpleLimiter }

function TSimpleLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 if PeakLevel < FThreshold
  then result := 1
  else result := FThresholdRatioFactor * Power(PeakLevel, FRatio - 1);
end;

function TSimpleLimiter.ProcessSample(Input: Double): Double;
{$IFNDEF PUREPASCAL}
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FDecayFactor;

 FGain := CharacteristicCurve(FPeak);

 result := FGain * Input;
end;
{$ELSE}
asm
 fld Input                        // Input
 fabs                             // abs(Input)
 fld [self.FPeak].Double          // FPeak, abs(Input)
 mov edx, eax                     // edx = self
 fcom st(1)                       // FPeak, abs(Input)
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 jbe @Attack                      // goto Attack
@Decay:
 fsub st(0), st(1)                // FPeak - abs(Input), abs(Input)
 fmul [edx.FDecayFactor].Double   // (FPeak - abs(Input)) * FDecayFactor, abs(Input)
 faddp                            // (FPeak - abs(Input)) * FDecayFactor + abs(Input)
 fst [self.FPeak].Double          // FPeak := (FPeak - abs(Input)) * FDecayFactor + abs(Input)
 jmp @EndAttack
@Attack:
 fxch                             // abs(Input), FPeak
 fsub st(0), st(1)                // abs(Input) - FPeak, FPeak
 fmul [edx.FAttackFactor].Double  // (abs(Input) - FPeak) * FAttackFactor, FPeak
 faddp                            // (abs(Input) - FPeak) * FAttackFactor + FPeak
 fst  [self.FPeak].Double         // FPeak := (abs(Input) - FPeak) * FAttackFactor + FPeak
@EndAttack:

 fld [edx.FThreshold].Double      // FThreshold, FPeak
 fcom st(1)                       // FThreshold, FPeak
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 fstp st(0)                       // FPeak
 jbe @Limit                       // goto Limit
 fstp st(0)                       // --
 fld Input                        // Input
 jmp @Exit
@Limit:

 fld [edx.FRatio].Double          // FRatio, FPeak
 fld1                             // 1, FRatio, FPeak
 fsubp                            // FRatio - 1, FPeak
 fxch
 fldln2                           // {
 fxch                             //
 fyl2x                            //
 fxch                             //
 fmulp   st(1), st                //  P
 fldl2e                           //  O
 fmulp   st(1), st                //  W
 fld     st(0)                    //  E
 frndint                          //  R
 fsub    st(1), st                //
 fxch    st(1)                    //
 f2xm1                            //
 fld1                             //
 faddp   st(1), st                //
 fscale                           // }
 fstp    st(1)

 fmul [edx.FThresholdRatioFactor].Double // FThresholdRatioFactor * Power(FPeak, FRatio - 1)
 fmul Input                              // Input * FThresholdRatioFactor * Power(FPeak, FRatio - 1)


@Exit:
end;
{$ENDIF}

procedure TSimpleLimiter.ThresholdChanged;
begin
 inherited;
 CalculateThresholdRatioFactor;
end;

procedure TSimpleLimiter.RatioChanged;
begin
 inherited;
 CalculateThresholdRatioFactor;
end;

procedure TSimpleLimiter.CalculateThresholdRatioFactor;
begin
 FThresholdRatioFactor := Power(FThreshold, 1 - FRatio);
end;

{ TSoftKneeLimiter }

function TSoftKneeLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
var
  InternalRatio, Knee : Double;
begin
 Knee := 0.5 *(1 + Tanh2c(FSoftKnee * log10(PeakLevel / FThreshold)));
 InternalRatio := 1 + Knee * (FRatio - 1);
 result := Power(FThreshold, 1 - InternalRatio) * Power(PeakLevel, InternalRatio - 1);
end;

constructor TSoftKneeLimiter.Create;
begin
 inherited;
 FSoftKnee := 1;
end;

function TSoftKneeLimiter.ProcessSample(Input: Double): Double;
begin
 if abs(Input) > FPeak
  then FPeak := FPeak + (abs(Input) - FPeak) * FAttackFactor
  else FPeak := abs(Input) + (FPeak - abs(Input)) * FDecayFactor;
 FGain := CharacteristicCurve(FPeak);
 result := FGain * Input;
end;

procedure TSoftKneeLimiter.SetSoftKnee(const Value: Double);
begin
 if FSoftKnee <> Value then
  begin
   FSoftKnee := Value;
  end;
end;

{ TSoftKneeFeedbackLimiter }

constructor TSoftKneeFeedbackLimiter.Create;
begin
 inherited;
 FGain := 1;
 FOversample := Round(1E5 / Samplerate + 0.5);
 FFilter := TButterworthLowCut.Create;
 FFilter.Frequency := 13.8;
 FFilter.Order := 1;
 CalculateAttackFactor;
 CalculateDecayFactor;
end;

procedure TSoftKneeFeedbackLimiter.CalculateAttackFactor;
begin
 if FAttack = 0
  then FAttackFactor := 0
  else FAttackFactor := exp( -ln2 / (FOversample * FAttack * 0.001 * SampleRate));
 FAttackFac2 := exp( -ln2 / (0.48 * SampleRate));
end;

procedure TSoftKneeFeedbackLimiter.CalculateDecayFactor;
begin
 if FDecay = 0 then FDecayFactor := 0
  else FDecayFactor := exp( -ln2 / ( {FOversample *} FDecay * 0.001 * SampleRate));
 FDecayFac2 := exp( -ln2 / (0.98 * SampleRate));
end;

function TSoftKneeFeedbackLimiter.TranslatePeakToGain(const PeakLevel: Double): Double;
begin
 // yet to do
 result := 1;
end;

function TSoftKneeFeedbackLimiter.ProcessSample(Input: Double): Double;
var
  InternalRatio    : Double;
  PeakdB           : Double;
begin
 Input := FFilter.ProcessSample(Input);
(*

// threshold = -13.4 .. - 13

 result := FGain * Input;
 if abs(result)>FPeak2
  then FPeak2 := abs(result) + (FPeak2 - abs(result)) * FAttackFac2
  else FPeak2 := abs(result) + (FPeak2 - abs(result)) * FDecayFac2;

 if abs(result)>FPeak
  then
   begin
    FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor;
    PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
    InternalRatio := - (3 + 3 * (PeakdB - FThresholddB - 0.5) / (abs(PeakdB - FThresholddB) + 1));
    FGain := dB_to_Amp(PeakdB * InternalRatio - FThresholddB * InternalRatio);
    for OversampleCount := 1 to FOversample - 1 do
     begin
      result := FGain * Input;
      FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor;
      PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
      InternalRatio := - (3 + 3 * (PeakdB - FThresholddB - 0.5) / (abs(PeakdB - FThresholddB) + 1));
      FGain := dB_to_Amp(PeakdB * InternalRatio - FThresholddB * InternalRatio);
     end
   end
  else
   begin
    FPeak := abs(result) + (FPeak - abs(result)) * FDecayFactor;
    PeakdB := Amp_to_dB(abs(0.3 * FPeak2 + 0.7 * FPeak + 1E-32));
    InternalRatio := - (3 + 3 * (PeakdB - FThresholddB - 0.5) / (abs(PeakdB - FThresholddB) + 1));
    FGain := dB_to_Amp(PeakdB * InternalRatio - FThresholddB * InternalRatio);
   end;

*)
 result := FGain * Input;
{
 if abs(result)>FPeak
  then FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor
  else FPeak := abs(result) + (FPeak - abs(result)) * FDecayFactor;
}
 FPeak := FDecayFactor * FPeak;
 if abs(result) > FPeak
  then FPeak := abs(result) + (FPeak - abs(result)) * FAttackFactor;

 FPeak2 := FDecayFac2 * FPeak2;
 if abs(result) > FPeak2
  then FPeak2 := abs(result) + (FPeak2 - abs(result)) * FAttackFac2;

 PeakdB := Amp_to_dB(abs(0.3 * FPeak + 0.7 * FPeak2 + 1E-32));
// InternalRatio := - (3 + 3 * (PeakdB - FThresholddB - 0.5) / (abs(PeakdB - FThresholddB) + 1));
 InternalRatio := - (2.6 + 2.6 * (PeakdB - FThresholddB - 1.5) / (abs(PeakdB - FThresholddB) + 2));
 FGain := dB_to_Amp(PeakdB * InternalRatio - FThresholddB * InternalRatio);
end;

procedure TSoftKneeFeedbackLimiter.SampleRateChanged;
begin
 inherited;
 FFilter.SampleRate := FSampleRate;
 FOversample := Round(1E5 / FSampleRate + 1.5);
 CalculateAttackFactor;
 CalculateDecayFactor;
end;

end.
