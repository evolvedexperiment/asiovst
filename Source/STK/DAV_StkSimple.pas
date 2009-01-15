unit DAV_StkSimple;

{
/***************************************************/
/*! \class TSimple
    \brief STK wavetable/noise instrument.

    This class combines a looped wave, a
    noise source, a biquad resonance filter,
    a one-pole filter, and an ADSR envelope
    to create some interesting sounds.

    Control Change Numbers: 
       - Filter Pole Position:=2
       - Noise/Pitched Cross-Fade:=4
       - Envelope Rate:=11
       - Gain:=128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, adsr, waveplayer, onepole, biquad, noise;

type
  TSimple = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: my_float);

  //! Start envelope toward "on" target.
    procedure keyOn;

  //! Start envelope toward "off" target.
    procedure keyOff;

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    ADSR: tadsr;
    loop: twaveplayer;
    filter: tonepole;
    BiQuad: tbiquad;
    Noise: tnoise;
    baseFrequency, loopGain: MY_FLOAT;
  end;

implementation

constructor TSimple.Create;
begin
  inherited Create(sr);
  adsr := TADSR.Create(srate);
  baseFrequency := 440.0;

  loop := TWavePlayer.Create(srate, 'impuls10.raw');

  filter := TOnePole.Create(srate, 0.5);
  noise := TNoise.Create(srate);
  biquad := TBiQuad.Create(srate);

  setFrequency(baseFrequency);
  loopGain := 0.5;
end;

destructor TSimple.Destroy;
begin
  inherited Destroy;
  adsr.Free;
  loop.Free;
  filter.Free;
  biquad.Free;
end;

procedure TSimple.keyOn;
begin
  adsr.keyOn;
end;

procedure TSimple.keyOff;
begin
  adsr.keyOff;
end;

procedure TSimple.noteOn;
begin
  keyOn;
  setFrequency(frequency);
  filter.setGain(amplitude);
end;

procedure TSimple.noteOff;
begin
  keyOff;
end;

procedure TSimple.setFrequency;
begin
  biquad.setResonance(frequency, 0.98, True);
  loop.setFrequency(frequency);
end;

function TSimple.tick: MY_FLOAT;
begin
  lastOutput := loopGain * loop.tick;
  biquad.tick(noise.tick);
  lastOutput := lastoutput + (1.0 - loopGain) * biquad.lastOut;
  lastOutput := filter.tick(lastOutput);
  lastOutput := lastoutput * adsr.tick;
  Result := lastOutput;
end;

procedure TSimple.controlChange;
var
  norm: MY_FLOAT;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_Breath_) then // 2
    filter.setPole(0.99 * (1.0 - (norm * 2.0)))
  else if (number = __SK_NoiseLevel_) then // 4
    loopGain := norm
  else if (number = __SK_ModFrequency_) then
   begin // 11
    norm := norm / (0.2 * srate);
    adsr.setAttackRate(norm);
    adsr.setDecayRate(norm);
    adsr.setReleaseRate(norm);
   end
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

end.
