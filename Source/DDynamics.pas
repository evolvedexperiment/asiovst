unit DDynamics;

interface

uses DButterworthFilter;

type
  TDynamics = class
  private
    fSampleRate: Double;
    fSampleRateRez: Double;
    fThreshold: Double;
    fDecay: Double;
    fAttack: Double;
    procedure SetThreshold(const Value: Double);
    function GetThreshold: Double;
    procedure SetAttack(const Value: Double);
    procedure SetDecay(const Value: Double);
    procedure CalculateAttackFactor;
    procedure CalculateDecayFactor;
  protected
    fLevel: Double;
    fDecayFactor: Double;
    fAttackFactor: Double;
    procedure SetSampleRate(const Value: Double); virtual;
  public
    function ProcessSample(Input : Double):Double; virtual; abstract;
    constructor Create; virtual;
  published
    property Threshold : Double read GetThreshold write SetThreshold;   // in dB
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
    fGainReductionFactor : Double;
    procedure SetHold(const Value: Double);
    procedure SetRatio(const Value: Double);
    procedure SetRange(const Value: Double);
    function GetHighCut: Double;
    function GetLowCut: Double;
    procedure SetHighCut(const Value: Double);
    procedure SetLowCut(const Value: Double);
    procedure SetKnee(const Value: Double);
  protected
    fGain        : Double;
    fHold        : Double;
    fRatio       : Double;
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
    property Ratio : Double read fRatio write SetRatio;
    property SideChainLowCut : Double read GetLowCut write SetLowCut;     // in Hz
    property SideChainHighCut : Double read GetHighCut write SetHighCut;  // in Hz
  end;

  TSimpleCompressor = class(TDynamics)
  private
    fRatio: Double;
    procedure SetRatio(const Value: Double);
  protected
    fGainFaktor: Double;
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
  published
    property Ratio : Double read fRatio write SetRatio;
  end;

  TSimpleLimiter = class(TDynamics)
  private
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
  published
  end;

implementation

uses DDSPBase;

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
  fThreshold := 1E-4;
  fAttack := 5;
  fDecay := 5;
  fLevel := 0;
  CalculateAttackFactor;
  CalculateDecayFactor;
end;

function TDynamics.GetThreshold: Double;
begin
  Result := Amp_to_dB(fThreshold)
end;

procedure TDynamics.CalculateAttackFactor;
begin
  if fAttack = 0 then fAttackFactor := 0
  else fAttackFactor := exp( -ln2 / (fAttack * 0.001 * SampleRate));
end;

procedure TDynamics.CalculateDecayFactor;
begin
  if fDecay = 0 then fDecayFactor := 0
  else fDecayFactor := exp( -ln2 / (fDecay * 0.001 * SampleRate));
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

procedure TDynamics.SetThreshold(const Value: Double);
begin
  fThreshold := dB_to_Amp(Value);
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

function TSimpleCompressor.ProcessSample(Input: Double): Double;
begin
  if Input > Threshold
    then result := Input * fGainFaktor
    else result := Input;
end;

procedure TSimpleCompressor.SetRatio(const Value: Double);
begin
  fRatio := Value;
end;

{ TSimpleLimiter }

constructor TSimpleLimiter.Create;
begin
  inherited;

end;

function TSimpleLimiter.ProcessSample(Input: Double): Double;
begin
  if Input > Threshold
    then result := Threshold
    else result := Input;
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
 if abs(Input)>fThreshold
  then fGainReductionFactor := 1  + (fGainReductionFactor - 1) * fAttackFactor
  else fGainReductionFactor := fGainReductionFactor * fDecayFactor;

 fGain := fRangeFactor + (1-fRangeFactor) * fGainReductionFactor;
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

procedure TGate.SetRatio(const Value: Double);
begin
 if fRatio <>Value then
  begin
    fRatio := Value;
  end;
end;

procedure TGate.SetSampleRate(const Value: Double);
begin
  inherited;
  CalculateHoldSamples;
  fLowCut.SampleRate := Value;
  fHighCut.SampleRate := Value;
end;

end.
