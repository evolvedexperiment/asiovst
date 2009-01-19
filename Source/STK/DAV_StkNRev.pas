unit DAV_StkNRev;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ CCRMA's TNRev reverberator class.

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

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkReverb, DAV_StkDelay, Math;

type
  TNRev = class(TReverb)
  public
    // Class constructor taking a T60 decay time argument.
    constructor Create(SampleRate, T60: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Compute one output sample.
    function tick(input: Single): Single;

  protected
    FAllpassDelays: array[0..7] of TDelay;
    FCombDelays: array[0..5] of TDelay;
    FLowpassState, FAllpassCoefficient: Single;
    FCombCoefficient: array[0..5] of Single;
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
  inherited Create(SampleRate);
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
    FCombDelays[i] := TDelay.Create(srate, lengths[i], lengths[i]);
    FCombCoefficient[i] := power(10, (-3 * lengths[i] / (T60 * srate)));
   end;

  for i := 0 to 7 do
    FAllpassDelays[i] := TDelay.Create(srate, lengths[i + 6], lengths[i + 6]);

  FAllpassCoefficient := 0.7;
  effectMix := 0.3;
  Clear;
end;

destructor TNRev.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to 5 do
    FCombDelays[i].Free;
  for i := 0 to 7 do
    FAllpassDelays[i].Free;
end;

procedure TNRev.Clear;
var
  i: integer;
begin
  for i := 0 to 5 do
    FCombDelays[i].Clear;
  for i := 0 to 7 do
    FAllpassDelays[i].Clear;
  lastOutput[0] := 0.0;
  lastOutput[1] := 0.0;
  FLowpassState := 0.0;
end;

function TNRev.tick(input: Single): Single;
var
  temp, temp0, temp1, temp2, temp3: Single;
  i: integer;
begin
  temp0 := 0.0;
  for i := 0 to 5 do
   begin
    temp := input + (FCombCoefficient[i] * FCombDelays[i].lastOut());
    temp0 := temp0 + FCombDelays[i].tick(temp);
   end;
  for i := 0 to 2 do
   begin
    temp := FAllpassDelays[i].lastOut();
    temp1 := FAllpassCoefficient * temp;
    temp1 := temp1 + temp0;
    FAllpassDelays[i].tick(temp1);
    temp0 := -(FAllpassCoefficient * temp1) + temp;
   end;

  // One-pole lowpass filter.
  FLowpassState := 0.7 * FLowpassState + 0.3 * temp0;
  temp := FAllpassDelays[3].lastOut();
  temp1 := FAllpassCoefficient * temp;
  temp1 := temp1 + FLowpassState;
  FAllpassDelays[3].tick(temp1);
  temp1 := -(FAllpassCoefficient * temp1) + temp;

  temp := FAllpassDelays[4].lastOut();
  temp2 := FAllpassCoefficient * temp;
  temp2 := temp2 + temp1;
  FAllpassDelays[4].tick(temp2);
  lastOutput[0] := effectMix * (-(FAllpassCoefficient * temp2) + temp);

  temp := FAllpassDelays[5].lastOut();
  temp3 := FAllpassCoefficient * temp;
  temp3 := temp3 + temp1;
  FAllpassDelays[5].tick(temp3);
  lastOutput[1] := effectMix * (-(FAllpassCoefficient * temp3) + temp);

  temp := (1.0 - effectMix) * input;
  lastOutput[0] := lastOutput[0] + temp;
  lastOutput[1] := lastOutput[1] + temp;

  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

end.
