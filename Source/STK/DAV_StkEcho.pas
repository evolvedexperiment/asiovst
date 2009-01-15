unit DAV_StkEcho;

{
/***************************************************/
/*! \class Echo
    \brief STK echo effect class.

    This class implements a echo effect.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkDelay;

type
  TEcho = class(TStk)
  public
  //! Class constructor, taking the longest desired delay length.
    constructor Create(sr, longestDelay: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set the delay line length in samples.
    procedure setDelay(delay: MY_FLOAT);

  //! Set the mixture of input and processed levels in the output (0.0 := input only, 1.0 := processed only). 
    procedure setEffectMix(mix: MY_FLOAT);

  //! Return the last output value.
    function lastOut: MY_FLOAT;

  //! Compute one output sample.
    function tick(input: MY_FLOAT): MY_FLOAT; overload;

  //! Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    delayLine: TDelay;
    length: longint;
    lastOutput, effectMix: MY_FLOAT;
  end;

implementation

constructor TEcho.Create(sr, longestDelay: MY_FLOAT);
begin
  inherited Create(sr);
  length := round(longestDelay) + 2;
  delayLine := TDelay.Create(srate, length shr 1, length);
  effectMix := 0.5;
  Clear;
end;

destructor TEcho.Destroy;
begin
  inherited Destroy;
  delayLine.Free;
end;

procedure TEcho.Clear;
begin
  delayLine.Clear;
  lastOutput := 0.0;
end;

procedure TEcho.setDelay;
var
  size: MY_FLOAT;
begin
  size := delay;
  if (delay < 0.0) then
    size := 0.0
  else if (delay > length) then
    size := length;
  delayLine.setDelay(round(size));
end;

procedure TEcho.setEffectMix;
begin
  effectMix := mix;
  if (mix < 0.0) then
    effectMix := 0.0
  else if (mix > 1.0) then
    effectMix := 1.0;
end;

function TEcho.lastOut: MY_FLOAT;
begin
  Result := lastOutput;
end;

function TEcho.tick(input: MY_FLOAT): MY_FLOAT;
begin
  lastOutput := effectMix * delayLine.tick(input);
  lastOutput := lastOutput + input * (1.0 - effectMix);
  Result := lastOutput;
end;

function TEcho.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
