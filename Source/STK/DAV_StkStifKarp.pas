unit DAV_StkStifKarp;

{
/***************************************************/
/*! \class TStifKarp
    \brief STK plucked stiff string instrument.

    This class implements a simple plucked string
    algorithm (Karplus Strong) with enhancements
    (Jaffe-Smith, Smith, and others), including
    string stiffness and pluck position controls.
    The stiffness is modeled with allpass filters.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    Control Change Numbers:
       - Pickup Position := 4
       - String Sustain := 11
       - String Stretch := 1

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, delayl, delaya, onezero, noise, biquad;

type
  TStifKarp = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Set the stretch "factor" of the string (0.0 - 1.0).
    procedure setStretch(stretch: MY_FLOAT);

  //! Set the pluck or "excitation" position along the string (0.0 - 1.0).
    procedure setPickupPosition(position: MY_FLOAT);

  //! Set the base loop gain.
  {
    The actual loop gain is set according to the frequency.
    Because of high-frequency loop filter roll-off, higher
    frequency settings have greater loop gains.
  }
    procedure setBaseLoopGain(aGain: MY_FLOAT);

  //! Pluck the string with the given amplitude using the current frequency.
    procedure pluck(amplitude: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    delayLine: tdelaya;
    combDelay: tdelayl;
    filter: tonezero;
    noise: tnoise;
    biQuad: array[0..3] of tbiquad;
    length: longint;
    loopGain, baseLoopGain, lastFrequency, lastLength,
    stretching, pluckAmplitude, pickupPosition: my_float;
  end;

implementation

constructor TStifKarp.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  delayLine := TDelayA.Create(srate, 0.5 * length, length);
  combDelay := TDelayL.Create(srate, 0.2 * length, length);

  filter := TOneZero.Create(srate);
  noise := TNoise.Create(srate);
  biQuad[0] := TBiQuad.Create(srate);
  biQuad[1] := TBiQuad.Create(srate);
  biQuad[2] := TBiQuad.Create(srate);
  biQuad[3] := TBiQuad.Create(srate);

  pluckAmplitude := 0.3;
  pickupPosition := 0.4;
  lastFrequency := lowestFrequency * 2.0;
  lastLength := length * 0.5;
  stretching := 0.9999;
  baseLoopGain := 0.995;
  loopGain := 0.999;

  Clear;
end;

destructor TStifKarp.Destroy;
begin
  inherited Destroy;
  delayLine.Free;
  combDelay.Free;
  filter.Free;
  noise.Free;
  biQuad[0].Free;
  biQuad[1].Free;
  biQuad[2].Free;
  biQuad[3].Free;
end;

procedure TStifKarp.Clear;
begin
  delayLine.Clear;
  combDelay.Clear;
  filter.Clear;
end;

procedure TStifKarp.setFrequency;
var
  delay: my_float;
begin
  lastFrequency := frequency;
  if (frequency <= 0.0) then
    lastFrequency := 220.0;
  lastLength := srate / lastFrequency;
  delay := lastLength - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delayLine.setDelay(delay);

  loopGain := baseLoopGain + (frequency * 0.000005);
  if (loopGain >= 1.0) then
    loopGain := 0.99999;

  setStretch(stretching);

  combDelay.setDelay(0.5 * pickupPosition * lastLength);
end;

procedure TStifKarp.setStretch;
var
  temp, dfreq, freq, coefficient: MY_FLOAT;
  i: integer;
begin
  stretching := stretch;
  freq := lastFrequency * 2.0;
  dFreq := ((0.5 * srate) - freq) * 0.25;
  temp := 0.5 + (stretch * 0.5);
  if (temp > 0.9999) then
    temp := 0.9999;
  for i := 0 to 3 do
   begin
    coefficient := temp * temp;
    biQuad[i].setA2(coefficient);
    biQuad[i].setB0(coefficient);
    biQuad[i].setB2(1.0);

    coefficient := -2.0 * temp * cos(TWO_PI * freq / srate);
    biQuad[i].setA1(coefficient);
    biQuad[i].setB1(coefficient);

    freq := freq + dFreq;
   end;
end;

procedure TStifKarp.setPickupPosition;
begin
  pickupPosition := position;
  if (position < 0.0) then
    pickupPosition := 0.0
  else if (position > 1.0) then
    pickupPosition := 1.0;

  // Set the pick position, which puts zeroes at position * length.
  combDelay.setDelay(0.5 * pickupPosition * lastLength);
end;

procedure TStifKarp.setBaseLoopGain;
begin
  baseLoopGain := aGain;
  loopGain := baseLoopGain + (lastFrequency * 0.000005);
  if (loopGain > 0.99999) then
    loopGain := 0.99999;
end;

procedure TStifKarp.pluck;
var
  gain: my_float;
  i: longint;
begin
  gain := amplitude;
  if (gain > 1.0) then
    gain := 1.0
  else if (gain < 0.0) then
    gain := 0.0;

  pluckAmplitude := gain;
  for i := 0 to length - 1 do
    delayLine.tick((delayLine.lastOut * 0.6) + 0.4 * noise.tick *
      pluckAmplitude)// Fill delay with noise additively with current contents.
//delayLine->tick( combDelay->tick((delayLine->lastOut() * 0.6) + 0.4 * noise->tick() * pluckAmplitude));
  ;
end;

procedure TStifKarp.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
end;

procedure TStifKarp.noteOff;
var
  gain: my_float;
begin
  gain := amplitude;
  if (gain > 1.0) then
    gain := 1.0
  else if (gain < 0.0) then
    gain := 0.0;
  loopGain := (1.0 - gain) * 0.5;
end;

function TStifKarp.tick: my_float;
var
  temp: my_float;
  i: integer;
begin
  temp := delayLine.lastOut * loopGain;

  // Calculate allpass stretching.
  for i := 0 to 3 do
    temp := biQuad[i].tick(temp);

  // Moving average filter.
  temp := filter.tick(temp);

  lastOutput := delayLine.tick(temp);
  lastOutput := lastOutput - combDelay.tick(lastOutput);
  Result := lastOutput;
end;

procedure TStifKarp.controlChange;
var
  norm: my_float;
begin
  norm := Value; // * ONE_OVER_128;
  if (number = __SK_PickPosition_) then // 4
    setPickupPosition(norm)
  else if (number = __SK_StringDamping_) then // 11
    setBaseLoopGain(0.97 + (norm * 0.03))
  else if (number = __SK_StringDetune_) then // 1
    setStretch(0.9 + (0.1 * (1.0 - norm)));
end;

end.

