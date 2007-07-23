unit DDynamics;

interface

uses DButterworthFilter;

type
  TDynamics = class
  private
    procedure SetAttack(const Value: Double);
    procedure SetDecay(const Value: Double);
    procedure CalculateAttackFactor;
    procedure CalculateDecayFactor;
    procedure CalculateThreshold;
  protected
    fPeak          : Double;
    fGain          : Double;
    fLevel         : Double;
    fRatio         : Double;
    fSampleRate    : Double;
    fSampleRateRez : Double;
    fThreshold     : Double;
    fThresholddB   : Double;
    fDecay         : Double;
    fAttack        : Double;
    fDecayFactor   : Double;
    fAttackFactor  : Double;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure SetThreshold(const Value: Double); virtual;
    procedure SetRatio(const Value: Double); virtual;
  public
    function ProcessSample(Input : Double):Double; virtual; abstract;
    constructor Create; virtual;
  published
    property Threshold : Double read fThresholddB write SetThreshold;   // in dB
    property Ratio : Double read fRatio write SetRatio;
    property Attack : Double read fAttack write SetAttack;            // in ms
    property Decay : Double read fDecay write SetDecay;         // in ms
    property SampleRate : Double read fSampleRate write SetSampleRate;
  end;

  TSimpleGate = class(TDynamics)
  private
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
  end;

  TGate = class(TDynamics)
  private
    procedure SetHold(const Value: Double);
    procedure SetRange(const Value: Double);
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
    procedure SetKnee(const Value: Double);
  protected
    fHold        : Double;
    fKnee        : Double;
    fRange       : Double;
    fRangeFactor : Double;
    fHoldSamples : Double;
    fSideChain   : Double;
    fDuck        : Boolean;
    fLowCut      : TButterworthHP;
    fHighCut     : TButterworthLP;
    procedure CalculateHoldSamples;
    procedure CalculateRangeFactor;
    procedure SetSampleRate(const Value: Double); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    function ProcessSample(Input : Double):Double; override;
    procedure InputSideChain(Input : Double); virtual;
  published
    property Hold : Double read fHold write SetHold;      // in s
    property Range : Double read fRange write SetRange;   // in dB
    property Knee : Double read fKnee write SetKnee;      // in dB
    property Duck : Boolean read fDuck write fDuck;       // not implemented yet
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;

  TSimpleCompressor = class(TDynamics)
  private
    fMakeUpGain  : Array [0..1] of Double;
    fSideChain   : Double;
    procedure RatioThresholdChanged;
  protected
    procedure SetThreshold(const Value: Double); override;
    procedure SetRatio(const Value: Double); override;
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
    procedure InputSideChain(Input : Double); virtual;
  published
    property Ratio : Double read fRatio write SetRatio;
  end;

  TSimpleLimiter = class(TDynamics)
  private
    fThresholdRatioFactor : Double;
    procedure RatioThresholdChanged;
  protected
    procedure SetRatio(const Value: Double); override;
    procedure SetThreshold(const Value: Double); override;
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
  end;

  TSoftKneeLimiter = class(TSimpleLimiter)
  private
    fSoftKnee: Double;
    procedure SetSoftKnee(const Value: Double);
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
  published
    property SoftKnee : Double read fSoftKnee write SetSoftKnee;
  end;

implementation

uses Math, DDSPBase;

{ TDynamics }

procedure TDynamics.SetSampleRate(const Value: Double);
begin
  if fSampleRate <> Value then
  begin
    fSampleRate := Value;
    fSampleRateRez := 1 / fSampleRate;
  end;
end;

constructor TDynamics.Create;
begin
  fSampleRate := 44100;
  fSampleRateRez := 1 / fSampleRate;
  fThresholddB := -40;
  fAttack := 5;
  fDecay := 5;
  fLevel := 0;
  CalculateThreshold;
  CalculateAttackFactor;
  CalculateDecayFactor;
end;

procedure TDynamics.CalculateAttackFactor;
begin
  if fAttack = 0 then fAttackFactor := 0
  else fAttackFactor := 1 - exp( -ln2 / (fAttack * 0.001 * SampleRate));
end;

procedure TDynamics.CalculateDecayFactor;
begin
  if fDecay = 0 then fDecayFactor := 0
  else fDecayFactor := exp( -ln2 / (fDecay * 0.001 * SampleRate));
end;

procedure TDynamics.SetRatio(const Value: Double);
begin
 if fRatio <>Value then
  begin
    fRatio := Value;
  end;
end;

procedure TDynamics.SetAttack(const Value: Double);
begin
  if fAttack <> Value then
  begin
    fAttack := abs(Value);
    CalculateAttackFactor;
  end;
end;

procedure TDynamics.SetDecay(const Value: Double);
begin
  if fDecay <> Value then
  begin
    fDecay := abs(Value);
    CalculateDecayFactor;
  end;
end;

procedure TDynamics.CalculateThreshold;
begin
  fThreshold := dB_to_Amp(fThresholddB);
end;

procedure TDynamics.SetThreshold(const Value: Double);
begin
  if fThresholddB <> Value then
  begin
    fThresholddB := Value;
    CalculateThreshold;
  end;
end;

{ TSimpleGate }

constructor TSimpleGate.Create;
begin
  inherited;
end;

function TSimpleGate.ProcessSample(Input: Double): Double;
begin
  if abs(Input)<fThreshold
   then Result := 0
   else Result := Input;
end;

{ TSimpleCompressor }

constructor TSimpleCompressor.Create;
begin
  inherited;
  fRatio := 1;
end;

procedure TSimpleCompressor.InputSideChain(Input: Double);
begin
 fSideChain := Input;
end;

function TSimpleCompressor.ProcessSample(Input: Double): Double;
begin
 if abs(fSideChain)>fPeak
  then fPeak := fPeak + (abs(fSideChain) - fPeak) * fAttackFactor
  else fPeak := abs(fSideChain) + (fPeak - abs(fSideChain)) * fDecayFactor;

 if fPeak < fThreshold
  then fGain := fMakeUpGain[0]
  else fGain := fMakeUpGain[1] * Power(fPeak, fRatio - 1);

 result := fGain * Input;
 fSideChain := Input;
end;

procedure TSimpleCompressor.SetRatio(const Value: Double);
begin
 inherited;
 RatioThresholdChanged;
end;

procedure TSimpleCompressor.SetThreshold(const Value: Double);
begin
 inherited;
 RatioThresholdChanged;
end;

procedure  TSimpleCompressor.RatioThresholdChanged;
var dbl : Double;
begin
 dbl := Power(fThreshold, 1 - fRatio);
 fMakeUpGain[0] := 1 / (dbl * Power(1, fRatio - 1));
 fMakeUpGain[1] := fMakeUpGain[0] * dbl;
end;

{ TGate }

constructor TGate.Create;
begin
  inherited;
  fGain := 1;
  fHold := 0.01;
  fLowCut  := TButterworthHP.Create;
  fHighCut := TButterworthLP.Create;
  fLowCut.Frequency := 20;
  fHighCut.Frequency := 20000;
  CalculateHoldSamples;
end;

destructor TGate.Destroy;
begin
  fLowCut.Free;
  fHighCut.Free;
  inherited;
end;

function TGate.GetHighCut: Double;
begin
 result := fHighCut.Frequency;
end;

function TGate.GetLowCut: Double;
begin
 result := fLowCut.Frequency;
end;

procedure TGate.InputSideChain(Input: Double);
begin
 fSideChain := fHighCut.ProcessSample(fLowCut.ProcessSample(Input));
end;

function TGate.ProcessSample(Input: Double): Double;
begin
 if abs(fSideChain)>fPeak
  then fPeak := fPeak + (abs(fSideChain) - fPeak) * fAttackFactor
  else fPeak := abs(fSideChain) + (fPeak - abs(fSideChain)) * fDecayFactor;

 if fPeak < fThreshold
  then fGain := Power(fThreshold, 1 - fRatio) * Power(fPeak, fRatio - 1) * (1-fRangeFactor) + fRangeFactor
  else fGain := fRangeFactor + (1-fRangeFactor);

 result := Input * fGain;
end;

procedure TGate.CalculateHoldSamples;
begin
  fHoldSamples := fHold * fSampleRate;
end;

procedure TGate.SetHighCut(const Value: Double);
begin
 fHighCut.Frequency := Value;
end;

procedure TGate.SetHold(const Value: Double);
begin
  if fHold <> Value then
  begin
    fHold := Value;
    CalculateHoldSamples;
  end;
end;

procedure TGate.SetKnee(const Value: Double);
begin
 if fKnee <> Value then
  begin
   fKnee := Value;
  end;
end;

procedure TGate.SetLowCut(const Value: Double);
begin
 fLowCut.Frequency := Value;
end;

procedure TGate.CalculateRangeFactor;
begin
 fRangeFactor := dB_to_Amp(fRange);
end;

procedure TGate.SetRange(const Value: Double);
begin
 if fRange <> Value then
  begin
   fRange := Value;
   CalculateRangeFactor;
  end;
end;

procedure TGate.SetSampleRate(const Value: Double);
begin
  inherited;
  CalculateHoldSamples;
  fLowCut.SampleRate := Value;
  fHighCut.SampleRate := Value;
end;

{ TSimpleLimiter }

constructor TSimpleLimiter.Create;
begin
  inherited;

end;

function TSimpleLimiter.ProcessSample(Input: Double): Double;
{$IFNDEF PUREPASCAL}
begin
 if abs(Input)>fPeak
  then fPeak := fPeak + (abs(Input) - fPeak) * fAttackFactor
  else fPeak := abs(Input) + (fPeak - abs(Input)) * fDecayFactor;

 if fPeak < fThreshold
  then fGain := 1
  else fGain := fThresholdRatioFactor * Power(fPeak, fRatio - 1);

 result := fGain * Input;
end;
{$ELSE}
asm
 fld Input                        // Input
 fabs                             // abs(Input)
 fld [self.fPeak].Double          // fPeak, abs(Input)
 mov edx, eax                     // edx = self
 fcom st(1)                       // fPeak, abs(Input)
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 jbe @Attack                      // goto Attack
@Decay:
 fsub st(0), st(1)                // fPeak - abs(Input), abs(Input)
 fmul [edx.fDecayFactor].Double   // (fPeak - abs(Input)) * fDecayFactor, abs(Input)
 faddp                            // (fPeak - abs(Input)) * fDecayFactor + abs(Input)
 fst [self.fPeak].Double          // fPeak := (fPeak - abs(Input)) * fDecayFactor + abs(Input)
 jmp @EndAttack
@Attack:
 fxch                             // abs(Input), fPeak
 fsub st(0), st(1)                // abs(Input) - fPeak, fPeak
 fmul [edx.fAttackFactor].Double  // (abs(Input) - fPeak) * fAttackFactor, fPeak
 faddp                            // (abs(Input) - fPeak) * fAttackFactor + fPeak
 fst  [self.fPeak].Double         // fPeak := (abs(Input) - fPeak) * fAttackFactor + fPeak
@EndAttack:

 fld [edx.fThreshold].Double      // fThreshold, fPeak
 fcom st(1)                       // fThreshold, fPeak
 fstsw ax                         // ax = FPU Status Word
 sahf                             // ax -> EFLAGS register
 fstp st(0)                       // fPeak
 jbe @Limit                       // goto Limit
 fstp st(0)                       // --
 fld Input                        // Input
 jmp @Exit
@Limit:

 fld [edx.fRatio].Double          // fRatio, fPeak
 fld1                             // 1, fRatio, fPeak
 fsubp                            // fRatio - 1, fPeak
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

 fmul [edx.fThresholdRatioFactor].Double // fThresholdRatioFactor * Power(fPeak, fRatio - 1)
 fmul Input                              // Input * fThresholdRatioFactor * Power(fPeak, fRatio - 1)


@Exit:
end;
{$ENDIF}

procedure TSimpleLimiter.SetRatio(const Value: Double);
begin
 inherited;
 RatioThresholdChanged;
end;

procedure TSimpleLimiter.SetThreshold(const Value: Double);
begin
 inherited;
 RatioThresholdChanged;
end;

procedure TSimpleLimiter.RatioThresholdChanged;
begin
 fThresholdRatioFactor := Power(fThreshold, 1 - fRatio);
end;

{ TSoftKneeLimiter }

constructor TSoftKneeLimiter.Create;
begin
 inherited;
 fSoftKnee := 1;
end;

function TSoftKneeLimiter.ProcessSample(Input: Double): Double;
var InternalRatio, Knee : Double;
begin
 if abs(Input)>fPeak
  then fPeak := fPeak + (abs(Input) - fPeak) * fAttackFactor
  else fPeak := abs(Input) + (fPeak - abs(Input)) * fDecayFactor;
 Knee := 0.5 *(1 + Tanh2c(fSoftKnee * log10(fPeak / fThreshold)));
 InternalRatio := 1 + Knee * (fRatio - 1);
 fGain := Power(fThreshold, 1 - InternalRatio) * Power(fPeak, InternalRatio - 1);
 result := fGain * Input;
end;

procedure TSoftKneeLimiter.SetSoftKnee(const Value: Double);
begin
  fSoftKnee := Value;
end;

end.
