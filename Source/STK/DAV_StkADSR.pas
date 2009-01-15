unit DAV_ADSR;

interface

{
/***************************************************/
/*! \class TADSR.
    \brief STK TADSR. envelope class.

    This Envelope subclass implements a
    traditional TADSR. (Attack, Decay,
    Sustain, Release) envelope.  It
    responds to simple keyOn and keyOff
    messages, keeping track of its state.
    The \e state := TADSR.::DONE after the
    envelope value reaches 0.0 in the
    TADSR.::RELEASE state.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}

uses
  DAV_Envelope, DAV_Stk;

  //! Envelope states.
type
  states = (ATTACK, DECAY, SUSTAIN, Release, DONE);

  TADSR = class(TEnvelope)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set target := 1, state := \e ADSR::ATTACK.
    procedure keyOn;

  //! Set target := 0, state := \e ADSR::RELEASE.
    procedure keyOff;

  //! Set the attack rate.
    procedure setAttackRate(aRate: my_float);

  //! Set the decay rate.
    procedure setDecayRate(aRate: my_float);

  //! Set the sustain level.
    procedure setSustainLevel(aLevel: my_float);

  //! Set the release rate.
    procedure setReleaseRate(aRate: my_float);

  //! Set the attack rate based on a time duration.
    procedure setAttackTime(aTime: my_float);

  //! Set the decay rate based on a time duration.
    procedure setDecayTime(aTime: my_float);

  //! Set the release rate based on a time duration.
    procedure setReleaseTime(aTime: my_float);

  //! Set sustain level and attack, decay, and release state rates based on time durations.
    procedure setAllTimes(aTime, dTime, sLevel, rTime: my_float);

  //! Set the target value.
    procedure setTarget(aTarget: my_float);

  //! Return the current envelope \e state (ATTACK, DECAY, SUSTAIN, RELEASE, DONE).
    function getState: states;

  //! Set to state := ADSR::SUSTAIN with current and target values of \e aValue.
    procedure setValue(aValue: my_float);

  //! Return one envelope output value.
    function tick: my_float; overload;

  //! Return \e vectorSize envelope outputs in \e vector.
    function tick(vector: pmy_float; vectorSize: longword): pmy_float; overload;

  protected
    attackRate, decayRate, sustainLevel, releaseRate: my_float;
    state: states;
  end;

implementation

constructor TADSR.Create;
begin
  inherited Create(sr);
  target := 0.0;
  Value := 0.0;
  attackRate := 0.001;
  decayRate := 0.001;
  sustainLevel := 0.5;
  releaseRate := 0.01;
  state := ATTACK;
end;

destructor TADSR.Destroy;
begin
  inherited Destroy;
end;

procedure TADSR.keyOn;
begin
  target := 1.0;
  rate := attackRate;
  state := ATTACK;
end;

procedure TADSR.keyOff;
begin
  target := 0.0;
  rate := releaseRate;
  state := Release;
end;

procedure TADSR.setAttackRate(aRate: MY_FLOAT);
begin
  if (aRate < 0.0) then
    attackRate := -aRate
  else
    attackRate := aRate;
end;

procedure TADSR.setDecayRate(aRate: MY_FLOAT);
begin
  if (aRate < 0.0) then
    decayRate := -aRate
  else
    decayRate := aRate;
end;

procedure TADSR.setSustainLevel(aLevel: MY_FLOAT);
begin
  if (aLevel < 0.0) then
    sustainLevel := 0.0
  else
    sustainLevel := aLevel;
end;

procedure TADSR.setReleaseRate(aRate: MY_FLOAT);
begin
  if (aRate < 0.0) then
    releaseRate := -aRate
  else
    releaseRate := aRate;
end;

procedure TADSR.setAttackTime(aTime: MY_FLOAT);
begin
  if (aTime < 0.0) then
    attackRate := 1.0 / (-aTime * srate)
  else
    attackRate := 1.0 / (aTime * srate);
end;

procedure TADSR.setDecayTime(aTime: MY_FLOAT);
begin
  if (aTime < 0.0) then
    decayRate := 1.0 / (-aTime * srate)
  else
    decayRate := 1.0 / (aTime * srate);
end;

procedure TADSR.setReleaseTime(aTime: MY_FLOAT);
begin
  if (aTime < 0.0) then
    releaseRate := 1.0 / (-aTime * srate)
  else
    releaseRate := 1.0 / (aTime * srate);
end;

procedure TADSR.setAllTimes;
begin
  setAttackTime(aTime);
  setDecayTime(dTime);
  setSustainLevel(sLevel);
  setReleaseTime(rTime);
end;

procedure TADSR.setTarget(aTarget: MY_FLOAT);
begin
  target := aTarget;
  if (Value < target) then
   begin
    state := ATTACK;
    setSustainLevel(target);
    rate := attackRate;
   end;
  if (Value > target) then
   begin
    setSustainLevel(target);
    state := DECAY;
    rate := decayRate;
   end;
end;

procedure TADSR.setValue(aValue: MY_FLOAT);
begin
  state := SUSTAIN;
  target := aValue;
  Value := aValue;
  setSustainLevel(aValue);
  rate := 0.0;
end;

function TADSR.getState: states;
begin
  Result := state;
end;

function TADSR.tick: MY_FLOAT;
begin
  case state of
    ATTACK :
     begin
      Value := Value + rate;
      if (Value >= target) then
       begin
        Value := target;
        rate := decayRate;
        target := sustainLevel;
        state := DECAY;
       end;
     end;
    DECAY :
     begin
      Value := Value - decayRate;
      if (Value <= sustainLevel) then
       begin
        Value := sustainLevel;
        rate := 0.0;
        state := SUSTAIN;
       end;
     end;
    Release :
     begin
      Value := Value - releaseRate;
      if (Value <= 0.0) then
       begin
        Value := 0.0;
        state := DONE;
       end;
     end;
   end;
  Result := Value;
end;

function TADSR.tick(vector: PMY_FLOAT; vectorSize: longword): PMY_FLOAT;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick;
    Inc(p);
   end;
  Result := vector;
end;

end.
