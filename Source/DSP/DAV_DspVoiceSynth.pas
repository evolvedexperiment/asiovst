unit DAV_DspVoiceSynth;

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex, DAV_DspCommon, DAV_DspTuner;

type
  TCustomVoiceSynth = class(TCustomLinearZeroCrossingTuner)
  private
    procedure SetAttack(const Value: Single);
    procedure SetRelease(const Value: Single);
    procedure SetThreshold(const Value: Single);
  protected
    FCurrentPosition         : TComplexSingle;
    FComplexAngle            : TComplexSingle;
    FSampleRateReciprocal    : Single;
    FAttack, FAttackFactor   : Single;
    FRelease, FReleaseFactor : Single;
    FLevel, FThreshold       : Single;
    FQuantizeToNotes         : Boolean;
    procedure ProcessDownsampled(DownSampled: Single); override;
    procedure SampleRateChanged; override;

    procedure CalculateAttackFactor; virtual;
    procedure CalculateReleaseFactor; virtual;
    procedure AttackChanged; virtual;
    procedure ReleaseChanged; virtual;
  public
    constructor Create; override;
    function Process(Input: Single): Single; reintroduce; virtual;

    property Attack: Single read FAttack write SetAttack;
    property Release: Single read FRelease write SetRelease;
    property Threshold: Single read FThreshold write SetThreshold;
    property QuantizeToNotes: Boolean read FQuantizeToNotes write FQuantizeToNotes;
  end;

  TVoiceSynth = class(TCustomVoiceSynth)
  published
    property SampleRate;
    property DownSampleFilterOrder;
    property MinimumFrequency;
    property MaximumFrequency;
    property Attack;  // in ms
    property Release; // in ms
  end;

implementation

uses
  DAV_Approximations, DAV_DspDynamics;

{ TCustomVoiceSynth }

constructor TCustomVoiceSynth.Create;
begin
 inherited;
 FCurrentPosition.Re := 1;
 FCurrentPosition.Im := 0;
 FComplexAngle.Re := 1;
 FComplexAngle.Im := 0;
 FAttack := 1;
 FRelease := 10;
 FThreshold := 0;
 FQuantizeToNotes := True;
 CalculateAttackFactor;
 CalculateReleaseFactor;
end;

function TCustomVoiceSynth.Process(Input: Single): Single;
begin
 inherited Process(Input);

 if abs(Input) > FLevel
  then FLevel := FLevel + (abs(Input) - FLevel) * FAttackFactor
  else FLevel := abs(Input) + (FLevel - abs(Input)) * FReleaseFactor;

 ComplexMultiplyInplace(FCurrentPosition, FComplexAngle);
 result := FLevel * FCurrentPosition.Re;
end;

procedure TCustomVoiceSynth.ProcessDownsampled(DownSampled: Single);
var
  Offset : Single;
begin
 if (Downsampled < FThreshold * FLevel) = FIsAbove then
  begin
   FIsAbove := not FIsAbove;

   if FOneCrossingOnly and FIsAbove then
    begin
     inc(FSamples);
     exit;
    end;

   Offset := (FLastSample / (FLastSample - Downsampled));

   FAverageSamples := SmoothFactor * FAverageSamples +
     (1 - SmoothFactor) * (FSamples - FLastOffset + Offset);
   FSamples := 1;
   FLastOffset := Offset;

   FCurrentFreq := FFrequencyFactor * SampleRate / (DownSampleFilterOrder * FAverageSamples);
   if FQuantizeToNotes
    then FCurrentFreq := 440 * FastPower2ContinousError3(round(12 *
           FastLog2ContinousError3(FCurrentFreq / 440)) * COneTwelfth32);

   GetSinCos(2 * Pi * FCurrentFreq * FSampleRateReciprocal, FComplexAngle.Im, FComplexAngle.Re);
  end
 else inc(FSamples);
end;

procedure TCustomVoiceSynth.SampleRateChanged;
begin
 inherited;
 FSampleRateReciprocal := 1 / SampleRate;
end;

procedure TCustomVoiceSynth.SetAttack(const Value: Single);
begin
 if FAttack <> Value then
  begin
   FAttack := Value;
   AttackChanged;
  end;
end;

procedure TCustomVoiceSynth.SetRelease(const Value: Single);
begin
 if FRelease <> Value then
  begin
   FRelease := Value;
   ReleaseChanged;
  end;
end;

procedure TCustomVoiceSynth.SetThreshold(const Value: Single);
begin
 FThreshold := Limit(Value, -1, 1);
end;

procedure TCustomVoiceSynth.AttackChanged;
begin
 CalculateAttackFactor;
end;

procedure TCustomVoiceSynth.ReleaseChanged;
begin
 CalculateReleaseFactor;
end;

procedure TCustomVoiceSynth.CalculateAttackFactor;
begin
  if FAttack = 0 then FAttackFactor := 0
  else FAttackFactor := 1 - exp( -ln2 / (FAttack * 0.001 * SampleRate));
end;

procedure TCustomVoiceSynth.CalculateReleaseFactor;
begin
  if FRelease = 0 then FReleaseFactor := 0
  else FReleaseFactor := exp( -ln2 / (FRelease * 0.001 * SampleRate));
end;

end.
