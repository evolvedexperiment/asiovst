unit DAV_StkPitShift;

{
/***************************************************/
/*! \class TPitShift
    \brief STK simple pitch shifter effect class.

    This class implements a simple pitch shifter
    using delay lines.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, delayl;

type
  TPitShift = class(TStk)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set the pitch shift factor (1.0 produces no shift).
    procedure setShift(shift: MY_FLOAT);

  //! Set the mixture of input and processed levels in the output (0.0 := input only, 1.0 := processed only). 
    procedure setEffectMix(mix: MY_FLOAT);

  //! Return the last output value.
    function lastOut: MY_FLOAT;

  //! Compute one output sample.
    function tick(input: MY_FLOAT): MY_FLOAT; overload;

  //! Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    delayLine: array[0..1] of TDelayL;
    effectMix, rate, lastOutput: my_float;
    delay, env: array[0..1] of my_float;
  end;

implementation

constructor TPitShift.Create;
begin
  inherited Create(sr);
  delay[0] := 12;
  delay[1] := 512;
  delayLine[0] := TDelayL.Create(srate, delay[0], 1024);
  delayLine[1] := TDelayL.Create(srate, delay[1], 1024);
  effectMix := 0.5;
  rate := 1.0;
end;

destructor TPitShift.Destroy;
begin
  inherited Destroy;
  delayLine[0].Free;
  delayLine[1].Free;
end;

procedure TPitShift.Clear;
begin
  delayLine[0].Clear;
  delayLine[1].Clear;
  lastOutput := 0.0;
end;

procedure TPitShift.setEffectMix;
begin
  effectMix := mix;
  if (mix < 0.0) then
    effectMix := 0.0
  else if (mix > 1.0) then
    effectMix := 1.0;
end;

procedure TPitShift.setShift;
begin
  if (shift < 1.0) then
    rate := 1.0 - shift
  else if (shift > 1.0) then
    rate := 1.0 - shift
  else
   begin
    rate := 0.0;
    delay[0] := 512;
   end;
end;

function TPitShift.lastOut: my_float;
begin
  Result := lastOutput;
end;

function TPitShift.tick(input: my_float): my_float;
begin
  delay[0] := delay[0] + rate;
  while (delay[0] > 1012) do
    delay[0] := delay[0] - 1000;
  while (delay[0] < 12) do
    delay[0] := delay[0] + 1000;
  delay[1] := delay[0] + 500;
  while (delay[1] > 1012) do
    delay[1] := delay[1] - 1000;
  while (delay[1] < 12) do
    delay[1] := delay[1] + 1000;
  delayLine[0].setDelay(round(delay[0]));
  delayLine[1].setDelay(round(delay[1]));
  env[1] := abs(delay[0] - 512) * 0.002;
  env[0] := 1.0 - env[1];
  lastOutput := env[0] * delayLine[0].tick(input);
  lastOutput := lastoutput + env[1] * delayLine[1].tick(input);
  lastOutput := lastoutput * effectMix;
  lastOutput := lastoutput + (1.0 - effectMix) * input;
  Result := lastOutput;
end;

function TPitShift.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
