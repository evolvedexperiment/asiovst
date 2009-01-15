unit DAV_StkNRev;

{
/***************************************************/
/*! \class TNRev
    \brief CCRMA's TNRev reverberator class.

    This class is derived from the CLM TNRev
    function, which is based on the use of
    networks of simple allpass and comb delay
    filters.  This particular arrangement consists
    of 6 comb filters in parallel, followed by 3
    allpass filters, a lowpass filter, and another
    allpass in series, followed by two allpass
    filters in parallel with corresponding right
    and left outputs.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, reverb, delay, Math;

type
  TNRev = class(TReverb)
  public
  //! Class constructor taking a T60 decay time argument.
    constructor Create(sr, T60: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Compute one output sample.
    function tick(input: my_float): my_float;

  protected
    allpassDelays: array[0..7] of tdelay;
    combDelays: array[0..5] of tdelay;
    lowpassState, allpassCoefficient: my_float;
    combCoefficient: array[0..5] of my_float;
  end;

implementation

constructor TNRev.Create;
const
  lengths: array[0..14] of integer = (
    1433, 1601, 1867, 2053, 2251, 2399, 347, 113, 37, 59, 53, 43, 37, 29, 19);
var
  scaler: double;
  delay, i: integer;
begin
  inherited Create(sr);
  scaler := srate / 25641.0;
  for i := 0 to 14 do
   begin
    delay := round(floor(scaler * lengths[i]));
    if ((delay and 1) = 0) then
      delay := delay + 1;
    while (not isPrime(delay)) do
      delay := delay + 2;
    lengths[i] := delay;
   end;

  for i := 0 to 5 do
   begin
    combDelays[i] := TDelay.Create(srate, lengths[i], lengths[i]);
    combCoefficient[i] := power(10, (-3 * lengths[i] / (T60 * srate)));
   end;

  for i := 0 to 7 do
    allpassDelays[i] := TDelay.Create(srate, lengths[i + 6], lengths[i + 6]);

  allpassCoefficient := 0.7;
  effectMix := 0.3;
  Clear;
end;

destructor TNRev.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to 5 do
    combDelays[i].Free;
  for i := 0 to 7 do
    allpassDelays[i].Free;
end;

procedure TNRev.Clear;
var
  i: integer;
begin
  for i := 0 to 5 do
    combDelays[i].Clear;
  for i := 0 to 7 do
    allpassDelays[i].Clear;
  lastOutput[0] := 0.0;
  lastOutput[1] := 0.0;
  lowpassState := 0.0;
end;

function TNRev.tick(input: my_float): my_float;
var
  temp, temp0, temp1, temp2, temp3: my_float;
  i: integer;
begin
  temp0 := 0.0;
  for i := 0 to 5 do
   begin
    temp := input + (combCoefficient[i] * combDelays[i].lastOut());
    temp0 := temp0 + combDelays[i].tick(temp);
   end;
  for i := 0 to 2 do
   begin
    temp := allpassDelays[i].lastOut();
    temp1 := allpassCoefficient * temp;
    temp1 := temp1 + temp0;
    allpassDelays[i].tick(temp1);
    temp0 := -(allpassCoefficient * temp1) + temp;
   end;

  // One-pole lowpass filter.
  lowpassState := 0.7 * lowpassState + 0.3 * temp0;
  temp := allpassDelays[3].lastOut();
  temp1 := allpassCoefficient * temp;
  temp1 := temp1 + lowpassState;
  allpassDelays[3].tick(temp1);
  temp1 := -(allpassCoefficient * temp1) + temp;

  temp := allpassDelays[4].lastOut();
  temp2 := allpassCoefficient * temp;
  temp2 := temp2 + temp1;
  allpassDelays[4].tick(temp2);
  lastOutput[0] := effectMix * (-(allpassCoefficient * temp2) + temp);

  temp := allpassDelays[5].lastOut();
  temp3 := allpassCoefficient * temp;
  temp3 := temp3 + temp1;
  allpassDelays[5].tick(temp3);
  lastOutput[1] := effectMix * (-(allpassCoefficient * temp3) + temp);

  temp := (1.0 - effectMix) * input;
  lastOutput[0] := lastOutput[0] + temp;
  lastOutput[1] := lastOutput[1] + temp;

  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

end.
