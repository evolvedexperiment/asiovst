unit DAV_StkChorus;

{
/***************************************************/
/*! \class TChorus
    \brief STK TChorus effect class.

    This class implements a TChorus effect.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_Stk, DAV_StkDelayl, DAV_StkLfo;

type
  TChorus = class(TStk)
  public
    mods: array[0..1] of TLFO;

  //! Class constructor, taking the longest desired delay length.
    constructor Create(sr, baseDelay: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set modulation depth.
    procedure setModDepth(depth: MY_FLOAT);

  //! Set modulation frequency.
    procedure setModFrequency(frequency: MY_FLOAT);

  //! Set the mixture of input and processed levels in the output (0.0 := input only, 1.0 := processed only). 
    procedure setEffectMix(mix: MY_FLOAT);

  //! Return the last output value.
    function lastOut: my_float;

  //! Return the last left output value.
    function lastOutLeft: my_float;

  //! Return the last right output value.
    function lastOutRight: MY_FLOAT;

  //! Compute one output sample.
    function tick(input: MY_FLOAT): MY_FLOAT; overload;

  //! Take \e vectorSize inputs, compute the same number of outputs and return them in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    delayLine: array[0..1] of TDelayL;
    baseLength, modDepth, effectMix: my_float;
    lastOutput: array[0..1] of my_float;
  end;

implementation

constructor TChorus.Create(sr, baseDelay: MY_FLOAT);
begin
  inherited Create(sr);
  delayLine[0] := TDelayL.Create(sr, round(baseDelay), round(baseDelay * 1.414) + 2);
  delayLine[1] := TDelayL.Create(sr, round(baseDelay), round(baseDelay) + 2);
  baseLength := baseDelay;

  mods[0] := TLFO.Create(srate);
  mods[1] := TLFO.Create(srate);
  mods[0].SetFrequency(0.2);
  mods[1].SetFrequency(0.222222);
  modDepth := 0.05;
  effectMix := 0.5;
  Clear;
end;

destructor TChorus.Destroy;
begin
  inherited Destroy;
  delayLine[0].Free;
  delayLine[1].Free;
  mods[0].Free;
  mods[1].Free;
end;

procedure TChorus.Clear;
begin
  delayLine[0].Clear;
  delayLine[1].Clear;
  lastOutput[0] := 0.0;
  lastOutput[1] := 0.0;
end;

procedure TChorus.setEffectMix;
begin
  effectMix := mix;
  if (mix < 0.0) then
    effectMix := 0.0
  else if (mix > 1.0) then
    effectMix := 1.0
end;

procedure TChorus.setModDepth;
begin
  modDepth := depth;
end;

procedure TChorus.setModFrequency;
begin
  mods[0].setFrequency(frequency);
  mods[1].setFrequency(frequency * 1.1111);
end;

function TChorus.lastOut: MY_FLOAT;
begin
  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

function TChorus.lastOutLeft: MY_FLOAT;
begin
  Result := lastOutput[0];
end;

function TChorus.lastOutRight: MY_FLOAT;
begin
  Result := lastOutput[1];
end;

function TChorus.tick(input: MY_FLOAT): MY_FLOAT;
begin
  delayLine[0].setDelay(baseLength * 0.707 * (1.0 + mods[0].tick));
  delayLine[1].setDelay(baseLength * 0.5 * (1.0 - mods[1].tick));
  lastOutput[0] := input * (1.0 - effectMix);
  lastOutput[0] := lastOutput[0] + effectMix * delayLine[0].tick(input);
  lastOutput[1] := input * (1.0 - effectMix);
  lastOutput[1] := lastOutput[1] + effectMix * delayLine[1].tick(input);
  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

function TChorus.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
