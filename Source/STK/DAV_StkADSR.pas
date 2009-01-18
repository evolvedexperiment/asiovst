unit DAV_StkADSR;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkEnvelope, DAV_Stk;

type
  TADSRStates = (asAttack, asDecay, asSustain, asRelease, asDone);

  TADSR = class(TStkEnvelope)
  protected
    FAttackRate   : Single;
    FDecayRate    : Single;
    FSustainLevel : Single;
    FReleaseRate  : Single;
    FState        : TADSRStates;
  public
    constructor Create(SampleRate: Single); override;
    destructor Destroy; override;

    procedure KeyOn; override;
    procedure KeyOff; override;

  //! Set the asAttack FRate.
    procedure SetAttackRate(aRate: Single);

  //! Set the asDecay FRate.
    procedure SetDecayRate(aRate: Single);

  //! Set the asSustain level.
    procedure SetSustainLevel(aLevel: Single);

  //! Set the asRelease FRate.
    procedure setReleaseRate(aRate: Single);

  //! Set the asAttack FRate based on a time duration.
    procedure SetAttackTime(aTime: Single);

  //! Set the asDecay FRate based on a time duration.
    procedure SetDecayTime(aTime: Single);

  //! Set the asRelease FRate based on a time duration.
    procedure SetReleaseTime(aTime: Single);

  //! Set asSustain level and asAttack, asDecay, and asRelease FState rates based on time durations.
    procedure SetAllTimes(aTime, dTime, sLevel, rTime: Single);

  //! Set the FTarget FCurrentValue.
    procedure SetTarget(aTarget: Single);

  //! Return the current envelope \e FState (asAttack, asDecay, asSustain, asRelease, asDone).
    function GetState: TADSRStates;

  //! Set to FState := ADSR::asSustain with current and FTarget values of \e Value.
    procedure SetValue(Value: Single);

    function Tick: Single; overload; override;
  end;

implementation

constructor TADSR.Create;
begin
  inherited Create(SampleRate);
  FTarget       := 0.0;
  FCurrentValue := 0.0;
  FAttackRate   := 0.001;
  FDecayRate    := 0.001;
  FSustainLevel := 0.5;
  FReleaseRate  := 0.01;
  FState        := asAttack;
end;

destructor TADSR.Destroy;
begin
 inherited Destroy;
end;

procedure TADSR.KeyOn;
begin
 FTarget := 1.0;
 FRate := FAttackRate;
 FState := asAttack;
end;

procedure TADSR.KeyOff;
begin
 FTarget := 0.0;
 FRate := FReleaseRate;
 FState := asRelease;
end;

procedure TADSR.SetAttackRate(aRate: Single);
begin
  if (aRate < 0.0) then
    FAttackRate := -aRate
  else
    FAttackRate := aRate;
end;

procedure TADSR.SetDecayRate(aRate: Single);
begin
  if (aRate < 0.0) then
    FDecayRate := -aRate
  else
    FDecayRate := aRate;
end;

procedure TADSR.SetSustainLevel(aLevel: Single);
begin
  if (aLevel < 0.0) then
    FSustainLevel := 0.0
  else
    FSustainLevel := aLevel;
end;

procedure TADSR.setReleaseRate(aRate: Single);
begin
  if (aRate < 0.0)
   then FReleaseRate := -aRate
   else FReleaseRate := aRate;
end;

procedure TADSR.SetAttackTime(aTime: Single);
begin
  if (aTime < 0.0)
   then FAttackRate := 1.0 / (-aTime * SampleRate)
   else FAttackRate := 1.0 / (aTime * SampleRate);
end;

procedure TADSR.SetDecayTime(aTime: Single);
begin
  if (aTime < 0.0) then
    FDecayRate := 1.0 / (-aTime * SampleRate)
  else
    FDecayRate := 1.0 / (aTime * SampleRate);
end;

procedure TADSR.SetReleaseTime(aTime: Single);
begin
  if (aTime < 0.0) then
    FReleaseRate := 1.0 / (-aTime * SampleRate)
  else
    FReleaseRate := 1.0 / (aTime * SampleRate);
end;

procedure TADSR.SetAllTimes;
begin
  SetAttackTime(aTime);
  SetDecayTime(dTime);
  SetSustainLevel(sLevel);
  SetReleaseTime(rTime);
end;

procedure TADSR.SetTarget(aTarget: Single);
begin
  FTarget := aTarget;
  if (FCurrentValue < FTarget) then
   begin
    FState := asAttack;
    SetSustainLevel(FTarget);
    FRate := FAttackRate;
   end;
  if (FCurrentValue > FTarget) then
   begin
    SetSustainLevel(FTarget);
    FState := asDecay;
    FRate := FDecayRate;
   end;
end;

procedure TADSR.SetValue(Value: Single);
begin
  FState := asSustain;
  FTarget := Value;
  FCurrentValue := Value;
  SetSustainLevel(Value);
  FRate := 0.0;
end;

function TADSR.GetState: TADSRStates;
begin
  Result := FState;
end;

function TADSR.Tick: Single;
begin
  case FState of
    asAttack :
     begin
      FCurrentValue := FCurrentValue + FRate;
      if (FCurrentValue >= FTarget) then
       begin
        FCurrentValue := FTarget;
        FRate := FDecayRate;
        FTarget := FSustainLevel;
        FState := asDecay;
       end;
     end;
    asDecay :
     begin
      FCurrentValue := FCurrentValue - FDecayRate;
      if (FCurrentValue <= FSustainLevel) then
       begin
        FCurrentValue := FSustainLevel;
        FRate := 0.0;
        FState := asSustain;
       end;
     end;
    asRelease :
     begin
      FCurrentValue := FCurrentValue - FReleaseRate;
      if (FCurrentValue <= 0.0) then
       begin
        FCurrentValue := 0.0;
        FState := asDone;
       end;
     end;
   end;
  Result := FCurrentValue;
end;

end.
