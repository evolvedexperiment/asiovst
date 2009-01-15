unit DAV_StkJCRev;

{
/***************************************************/
/*! \class TJCRev
    \brief John Chowning's reverberator class.

    This class is derived from the CLM TJCRev
    function, which is based on the use of
    networks of simple allpass and comb delay
    filters.  This class implements three series
    allpass units, followed by four parallel comb
    filters, and two decorrelation delay lines in
    parallel at the output.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, reverb, delay, Math;

type
  TJCRev = class(TReverb)
  public
  //! Class constructor taking a T60 decay time argument.
    constructor Create(sr, T60: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Compute one output sample.
    function tick(input: MY_FLOAT): MY_FLOAT;

  protected
    allpassDelays: array[0..2] of tdelay;
    combDelays: array[0..3] of tdelay;
    outLeftDelay: tdelay;
    outRightDelay: tdelay;
    allpassCoefficient: my_float;
    combCoefficient: array[0..3] of my_float;
  end;

implementation

constructor TJCRev.Create;
var
  lengths: array[0..8] of integer;
  scaler: double;
  delay, i: integer;
begin
  inherited Create(sr);
  // Delay lengths for 44100 Hz sample rate.
  lengths[0] := 1777;
  lengths[1] := 1847;
  lengths[2] := 1993;
  lengths[3] := 2137;
  lengths[4] := 389;
  lengths[5] := 127;
  lengths[6] := 43;
  lengths[7] := 211;
  lengths[8] := 179;
  scaler := srate / 44100.0;

  if (scaler <> 1.0) then
    for i := 0 to 8 do
     begin
      delay := round(floor(scaler * lengths[i]));
      if ((delay and 1) = 0) then
        delay := delay + 1;
      while (not isPrime(delay)) do
        delay := delay + 2;
      lengths[i] := delay;
     end;

  for i := 0 to 2 do
    allpassDelays[i] := TDelay.Create(srate, lengths[i + 4], lengths[i + 4]);

  for i := 0 to 3 do
   begin
    combDelays[i] := TDelay.Create(srate, lengths[i], lengths[i]);
    combCoefficient[i] := power(10, (-3 * lengths[i] / (T60 * srate)));
   end;

  outLeftDelay := TDelay.Create(srate, lengths[7], lengths[7]);
  outRightDelay := TDelay.Create(srate, lengths[8], lengths[8]);
  allpassCoefficient := 0.7;
  effectMix := 0.3;
  Clear;
end;

destructor TJCRev.Destroy;
begin
  inherited Destroy;
  allpassDelays[0].Free;
  allpassDelays[1].Free;
  allpassDelays[2].Free;
  combDelays[0].Free;
  combDelays[1].Free;
  combDelays[2].Free;
  combDelays[3].Free;
  outLeftDelay.Free;
  outRightDelay.Free;
end;

procedure TJCRev.Clear;
begin
  allpassDelays[0].Clear;
  allpassDelays[1].Clear;
  allpassDelays[2].Clear;
  combDelays[0].Clear;
  combDelays[1].Clear;
  combDelays[2].Clear;
  combDelays[3].Clear;
  outRightDelay.Clear;
  outLeftDelay.Clear;
  lastOutput[0] := 0.0;
  lastOutput[1] := 0.0;
end;

function TJCRev.tick(input: MY_FLOAT): MY_FLOAT;
var
  temp, temp0, temp1, temp2, temp3, temp4, temp5, temp6, filtout: MY_FLOAT;
begin
  temp := allpassDelays[0].lastOut;
  temp0 := allpassCoefficient * temp;
  temp0 := temp0 + input;
  allpassDelays[0].tick(temp0);
  temp0 := -(allpassCoefficient * temp0) + temp;

  temp := allpassDelays[1].lastOut;
  temp1 := allpassCoefficient * temp;
  temp1 := temp1 + temp0;
  allpassDelays[1].tick(temp1);
  temp1 := -(allpassCoefficient * temp1) + temp;

  temp := allpassDelays[2].lastOut;
  temp2 := allpassCoefficient * temp;
  temp2 := temp2 + temp1;
  allpassDelays[2].tick(temp2);
  temp2 := -(allpassCoefficient * temp2) + temp;

  temp3 := temp2 + (combCoefficient[0] * combDelays[0].lastOut());
  temp4 := temp2 + (combCoefficient[1] * combDelays[1].lastOut());
  temp5 := temp2 + (combCoefficient[2] * combDelays[2].lastOut());
  temp6 := temp2 + (combCoefficient[3] * combDelays[3].lastOut());

  combDelays[0].tick(temp3);
  combDelays[1].tick(temp4);
  combDelays[2].tick(temp5);
  combDelays[3].tick(temp6);

  filtout := temp3 + temp4 + temp5 + temp6;

  lastOutput[0] := effectMix * (outLeftDelay.tick(filtout));
  lastOutput[1] := effectMix * (outRightDelay.tick(filtout));
  temp := (1.0 - effectMix) * input;
  lastOutput[0] := lastOutput[0] + temp;
  lastOutput[1] := lastOutput[1] + temp;

  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

end.
