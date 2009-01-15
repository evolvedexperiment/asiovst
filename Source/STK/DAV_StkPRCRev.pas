unit DAV_StkPRCRev;

{
/***************************************************/
/*! \class TPRCRev
    \brief Perry's simple reverberator class.

    This class is based on some of the famous
    Stanford/CCRMA reverbs (NRev, KipRev), which
    were based on the Chowning/Moorer/Schroeder
    reverberators using networks of simple allpass
    and comb delay filters.  This class implements
    two series allpass units and two parallel comb
    filters.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, reverb, delay, Math;

type
  TPRCRev = class(TReverb)
  public
  //! Class constructor taking a T60 decay time argument.
    constructor Create(sr, T60: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Compute one output sample.
    function tick(input: MY_FLOAT): MY_FLOAT;

  protected
    allpassDelays: array[0..1] of tdelay;
    combDelays: array[0..1] of tdelay;
    allpassCoefficient: my_float;
    combCoefficient: array[0..1] of my_float;
  end;

implementation

constructor TPRCRev.Create;
const
  lengths: array[0..3] of integer = (353, 1097, 1777, 2137);
var
  scaler: double;
  delay, i: integer;
begin
  inherited Create(sr);
  // Delay lengths for 44100 Hz sample rate.
  scaler := srate / 44100.0;

  // Scale the delay lengths if necessary.
  if (scaler <> 1.0) then
    for i := 0 to 3 do
     begin
      delay := round(floor(scaler * lengths[i]));
      if ((delay and 1) = 0) then
        delay := delay + 1;
      while (not isPrime(delay)) do
        delay := delay + 2;
      lengths[i] := delay;
     end;

  for i := 0 to 1 do
   begin
    allpassDelays[i] := TDelay.Create(srate, lengths[i], lengths[i]);
    combDelays[i] := TDelay.Create(srate, lengths[i + 2], lengths[i + 2]);
    combCoefficient[i] := power(10, (-3 * lengths[i + 2] / (T60 * srate)));
   end;

  allpassCoefficient := 0.7;
  effectMix := 0.5;
  Clear;
end;

destructor TPRCRev.Destroy;
begin
  inherited Destroy;
  allpassDelays[0].Free;
  allpassDelays[1].Free;
  combDelays[0].Free;
  combDelays[1].Free;
end;

procedure TPRCRev.Clear;
begin
  allpassDelays[0].Clear();
  allpassDelays[1].Clear();
  combDelays[0].Clear();
  combDelays[1].Clear();
  lastOutput[0] := 0.0;
  lastOutput[1] := 0.0;
end;

function TPRCRev.tick(input: my_float): my_float;
var
  temp, temp0, temp1, temp2, temp3: my_float;
begin
  temp := allpassDelays[0].lastOut();
  temp0 := allpassCoefficient * temp;
  temp0 := temp0 + input;
  allpassDelays[0].tick(temp0);
  temp0 := -(allpassCoefficient * temp0) + temp;

  temp := allpassDelays[1].lastOut();
  temp1 := allpassCoefficient * temp;
  temp1 := temp1 + temp0;
  allpassDelays[1].tick(temp1);
  temp1 := -(allpassCoefficient * temp1) + temp;

  temp2 := temp1 + (combCoefficient[0] * combDelays[0].lastOut());
  temp3 := temp1 + (combCoefficient[1] * combDelays[1].lastOut());

  lastOutput[0] := effectMix * (combDelays[0].tick(temp2));
  lastOutput[1] := effectMix * (combDelays[1].tick(temp3));
  temp := (1.0 - effectMix) * input;
  lastOutput[0] := lastOutput[0] + temp;
  lastOutput[1] := lastOutput[1] + temp;
  Result := (lastOutput[0] + lastOutput[1]) * 0.5;
end;

end.
