unit DAV_StkEnvelope;

interface

uses
  DAV_StkCommon, Windows;

{
/***************************************************/
/*! \class TEnvelope
    \brief STK TEnvelope base class.

    This class implements a simple TEnvelope
    generator which is capable of ramping to
    a target value by a specified \e rate.
    It also responds to simple \e keyOn and
    \e keyOff messages, ramping to 1.0 on
    keyOn and to 0.0 on keyOff.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
type
  TEnvelope = class(TStk)
  public

  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set target = 1.
    procedure keyOn;

  //! Set target = 0.
    procedure keyOff;

  //! Set the \e rate.
    procedure setRate(aRate: my_float);

  //! Set the \e rate based on a time duration.
    procedure setTime(aTime: my_float);

  //! Set the target value.
    procedure setTarget(aTarget: my_float);

  //! Set current and target values to \e aValue.
    procedure setValue(aValue: my_float);

  //! Return the current envelope \e state (0 = at target, 1 otherwise).
    function getState: integer;

  //! Return one envelope output value.
    function tick: my_float; overload;

  //! Return \e vectorSize envelope outputs in \e vector.
    function tick(vector: pmy_float; vectorSize: longword): pmy_float; overload;

  //! Return the last computed output value.
    function lastOut: my_float;

  protected
    Value, target, rate: my_float;
    state: integer;
  end;

implementation

constructor TEnvelope.Create;
begin
  inherited Create(sr);
  target := 0;
  Value := 0;
  rate := 0.001;
  state := 0;
end;

destructor TEnvelope.Destroy;
begin
  inherited Destroy;
end;

procedure TEnvelope.keyOn;
begin
  target := 1;
  if (Value <> target) then
    state := 1;
end;

procedure TEnvelope.keyOff;
begin
  target := 0;
  if (Value <> target) then
    state := 1;
end;

procedure TEnvelope.setRate(aRate: my_float);
begin
  if (aRate < 0.0) then
    rate := -aRate
  else
    rate := aRate;
end;

procedure TEnvelope.setTime(aTime: my_float);
begin
  if (aTime < 0.0) then
    rate := 1.0 / (-aTime * srate)
  else
    rate := 1.0 / (aTime * srate);
end;

procedure TEnvelope.setTarget(aTarget: my_float);
begin
  target := aTarget;
  if (Value <> target) then
    state := 1;
end;

procedure TEnvelope.setValue(aValue: my_float);
begin
  state := 0;
  target := aValue;
  Value := aValue;
end;

function TEnvelope.getState: integer;
begin
  Result := state;
end;

function TEnvelope.tick: my_float;
begin
  if (state > 0) then
    if (target > Value) then
     begin
      Value := Value + rate;
      if (Value >= target) then
       begin
        Value := target;
        state := 0;
       end;
     end
    else
     begin
      Value := Value - rate;
      if (Value <= target) then
       begin
        Value := target;
        state := 0;
       end;
     end;
  Result := Value;
end;

function TEnvelope.tick(vector: pmy_float; vectorSize: longword): pmy_float;
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

function TEnvelope.lastOut: my_float;
begin
  Result := Value;
end;

end.
