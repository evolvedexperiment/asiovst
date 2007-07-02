unit DDynamics;

interface

type
  TDynamics = class
  private
    fSampleRate: Double;
    fSampleRateRez: Double;
    fThreshold: Double;
    fRelease: Double;
    fAttack: Double;
    procedure SetThreshold(const Value: Double);
    function GetThreshold: Double;
    procedure SetAttack(const Value: Double);
    procedure SetRelease(const Value: Double);
    procedure SetSampleRate(const Value: Double);
  protected
    fLevel: Double;
  public
    function ProcessSample(Input : Double):Double; virtual; abstract;
    constructor Create; virtual;
  published
    property Threshold : Double read GetThreshold write SetThreshold;   // in dB
    property Attack : Double read fAttack write SetAttack;            // in ms
    property Release : Double read fRelease write SetRelease;         // in ms
    property SampleRate : Double read fSampleRate write SetSampleRate;
  end;

  TSimpleGate = class(TDynamics)
  private
  public
    constructor Create; override;
    function ProcessSample(Input : Double):Double; override;
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
  fRelease := 5;
  fLevel := 0;
end;

function TDynamics.GetThreshold: Double;
begin
  Result := Amp_to_dB(fThreshold)
end;

procedure TDynamics.SetAttack(const Value: Double);
begin
  fAttack := Value;
end;

procedure TDynamics.SetRelease(const Value: Double);
begin
  fRelease := Value;
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

end.
