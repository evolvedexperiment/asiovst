unit DAV_Clarinet;

{
/***************************************************/
/*! \class TClarinet
    \brief STK TClarinet physical model class.

    This class implements a simple clarinet
    physical model, as discussed by Smith (1986),
    McIntyre, Schumacher, Woodhouse (1983), and
    others.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers: 
       - Reed Stiffness := 2
       - Noise Gain := 4
       - Vibrato Frequency := 11
       - Vibrato Gain := 1
       - Breath Pressure := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_Stk, DAV_Instrmnt, DAV_Delayl, DAV_Reedtabl, DAV_Onezero, DAV_Envelope,
  DAV_Noise, DAV_LFO;

type
  TClarinet = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: my_float);

  //! Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure startBlowing(amplitude, rate: my_float);

  //! Decrease breath pressure with given rate of decrease.
    procedure stopBlowing(rate: my_float);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: my_float);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: my_float);

  //! Compute one output sample.
    function tick: my_float;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: my_float);

  protected
    delayLine: tdelayl;
    reedTable: treedtabl;
    filter: tonezero;
    envelope: tenvelope;
    noise: tnoise;
    vibrato: tlfo;
    length: longint;
    outputGain, noiseGain, vibratoGain: my_float;
  end;

implementation

constructor TClarinet.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  delayLine := TDelayL.Create(srate, (length / 2.0), length);
  reedTable := TReedTabl.Create(srate);
  reedTable.setOffset(0.7);
  reedTable.setSlope(-0.3);
  filter := TOneZero.Create(srate);
  envelope := TEnvelope.Create(srate);
  noise := TNoise.Create(srate);
  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(5.735);
  outputGain := 1.0;
  noiseGain := 0.2;
  vibratoGain := 0.1;
end;

destructor TClarinet.Destroy;
begin
  inherited Destroy;
  delayLine.Free;
  reedTable.Free;
  filter.Free;
  envelope.Free;
  noise.Free;
  vibrato.Free;
end;

procedure TClarinet.Clear;
begin
  delayLine.Clear;
  filter.tick(0.0);
end;

procedure TClarinet.setFrequency;
var
  delay, freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := length - approximate filter delay.
  delay := (srate / freakency) * 0.5 - 1.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delayLine.setDelay(delay);
end;

procedure TClarinet.startBlowing;
begin
  envelope.setRate(rate);
  envelope.setTarget(amplitude);
end;

procedure TClarinet.stopBlowing;
begin
  envelope.setRate(rate);
  envelope.setTarget(0.0);
end;

procedure TClarinet.noteOn(frequency, amplitude: MY_FLOAT);
begin
  setFrequency(frequency);
  startBlowing(0.55 + (amplitude * 0.30), amplitude * 0.005);
  outputGain := amplitude + 0.001;
end;

procedure TClarinet.noteOff;
begin
  stopBlowing(amplitude * 0.01);
end;

function TClarinet.tick: MY_FLOAT;
var
  breathPressure, pressureDiff: MY_FLOAT;
begin
  // Calculate the breath pressure (envelope + noise + vibrato)
  breathPressure := envelope.tick;
  breathPressure := breathPressure + breathPressure * noiseGain * noise.tick;
  breathPressure := breathPressure + breathPressure * vibratoGain *
    vibrato.tick;

  // Perform commuted loss filtering.
  pressureDiff := -0.95 * filter.tick(delayLine.lastOut);

  // Calculate pressure difference of reflected and mouthpiece pressures.
  pressureDiff := pressureDiff - breathPressure;

  // Perform non-linear scattering using pressure difference in reed function.
  lastOutput := delayLine.tick(breathPressure + pressureDiff *
    reedTable.tick(pressureDiff));

  // Apply output gain.
  lastOutput := lastOutput * outputGain;

  Result := lastOutput;
end;

procedure TClarinet.controlChange(number: integer; Value: my_float);
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_ReedStiffness_) then // 2
    reedTable.setSlope(-0.44 + (0.26 * norm))
  else if (number = __SK_NoiseLevel_) then // 4
    noiseGain := (norm * 0.4)
  else if (number = __SK_ModFrequency_) then // 11
    vibrato.setFrequency((norm * 12.0))
  else if (number = __SK_ModWheel_) then // 1
    vibratoGain := (norm * 0.5)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    envelope.setValue(norm);
end;

end.
