unit DAV_StkSaxofony;

{
/***************************************************/
/*! \class TSaxofony
    \brief STK faux conical bore reed instrument class.

    This class implements a "hybrid" digital
    waveguide instrument that can generate a
    variety of wind-like sounds.  It has also been
    referred to as the "blowed string" model.  The
    waveguide section is essentially that of a
    string, with one rigid and one lossy
    termination.  The non-linear function is a
    reed table.  The string can be "blown" at any
    point between the terminations, though just as
    with strings, it is impossible to excite the
    system at either end.  If the excitation is
    placed at the string mid-point, the sound is
    that of a clarinet.  At points closer to the
    "bridge", the sound is closer to that of a
    saxophone.  See Scavone (2002) for more details.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers:
       - Reed Stiffness := 2
       - Reed Aperture := 26
       - Noise Gain := 4
       - Blow Position := 11
       - Vibrato Frequency := 29
       - Vibrato Gain := 1
       - Breath Pressure := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, delayl, reedtabl, onezero, envelope, noise, lfo;

type
  TSaxofony = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Set the "blowing" position between the air column terminations (0.0 - 1.0).
    procedure setBlowPosition(aPosition: MY_FLOAT);

  //! Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure startBlowing(amplitude, rate: MY_FLOAT);

  //! Decrease breath pressure with given rate of decrease.
    procedure stopBlowing(rate: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    delays: array[0..1] of tdelayl;
    reedTable: treedtabl;
    filter: tonezero;
    envelope: tenvelope;
    noise: tnoise;
    vibrato: tlfo;
    length: longint;
    outputGain, noiseGain, vibratoGain, position: MY_FLOAT;
  end;

implementation

constructor TSaxofony.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  // Initialize blowing position to 0.2 of length / 2.
  position := 0.2;
  delays[0] := TDelayL.Create(srate, (1.0 - position) * (length shr 1), length);
  delays[1] := TDelayL.Create(srate, position * (length shr 1), length);

  reedTable := TReedTabl.Create(srate);
  reedTable.setOffset(0.7);
  reedTable.setSlope(0.3);
  filter := TOneZero.Create(srate);
  envelope := TEnvelope.Create(srate);
  noise := TNoise.Create(srate);

  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(5.735);

  outputGain := 0.3;
  noiseGain := 0.2;
  vibratoGain := 0.1;
end;

destructor TSaxofony.Destroy;
begin
  inherited Destroy;
  delays[0].Free;
  delays[1].Free;
  reedTable.Free;
  filter.Free;
  envelope.Free;
  noise.Free;
  vibrato.Free;
end;

procedure TSaxofony.Clear;
begin
  delays[0].Clear;
  delays[1].Clear;
  filter.tick(0.0);
end;

procedure TSaxofony.setFrequency;
var
  delay, freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  delay := (srate / freakency) - 3.0;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;

  delays[0].setDelay((1.0 - position) * delay);
  delays[1].setDelay(position * delay);
end;

procedure TSaxofony.setBlowPosition(aPosition: my_float);
var
  total_delay: my_float;
begin
  if (position = aPosition) then
    exit;
  if (aPosition < 0.0) then
    position := 0.0
  else if (aPosition > 1.0) then
    position := 1.0
  else
    position := aPosition;

  total_delay := delays[0].getDelay;
  total_delay := total_delay + delays[1].getDelay;

  delays[0].setDelay((1.0 - position) * total_delay);
  delays[1].setDelay(position * total_delay);
end;

procedure TSaxofony.startBlowing;
begin
  envelope.setRate(rate);
  envelope.setTarget(amplitude);
end;

procedure TSaxofony.stopBlowing;
begin
  envelope.setRate(rate);
  envelope.setTarget(0.0);
end;

procedure TSaxofony.noteOn;
begin
  setFrequency(frequency);
  startBlowing(0.55 + (amplitude * 0.30), amplitude * 0.005);
  outputGain := amplitude + 0.001;
end;

procedure TSaxofony.noteOff(amplitude: my_float);
begin
  stopBlowing(amplitude * 0.01);
end;

function TSaxofony.tick: my_float;
var
  pressureDiff, breathPressure, temp: my_float;
begin
  // Calculate the breath pressure (envelope + noise + vibrato)
  breathPressure := envelope.tick;
  breathPressure := breathPressure + breathPressure * noiseGain * noise.tick;
  breathPressure := breathPressure + breathPressure * vibratoGain *
    vibrato.tick;

  temp := -0.95 * filter.tick(delays[0].lastOut);
  lastOutput := temp - delays[1].lastOut;
  pressureDiff := breathPressure - lastOutput;
  delays[1].tick(temp);
  delays[0].tick(breathPressure - (pressureDiff *
    reedTable.tick(pressureDiff)) - temp);

  lastOutput := lastOutput * outputGain;
  Result := lastOutput;
end;

procedure TSaxofony.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_ReedStiffness_) then // 2
    reedTable.setSlope(0.1 + (0.4 * norm))
  else if (number = __SK_NoiseLevel_) then // 4
    noiseGain := (norm * 0.4)
  else if (number = 29) then // 29
    vibrato.setFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    vibratoGain := (norm * 0.5)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    envelope.setValue(norm)
  else if (number = 11) then // 11
    setBlowPosition(norm)
  else if (number = 26) then // reed table offset
    reedTable.setOffset(0.4 + (norm * 0.6));
end;

end.
