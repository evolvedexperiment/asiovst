unit DAV_StkResonate;

{
/***************************************************/
/*! \class TResonate
    \brief STK noise driven formant filter.

    This instrument contains a noise source, which
    excites a biquad resonance filter, with volume
    controlled by an ADSR.

    Control Change Numbers:
       - Resonance Frequency (0-Nyquist) := 2
       - Pole Radii := 4
       - Notch Frequency (0-Nyquist) := 11
       - Zero Radii := 1
       - Envelope Gain := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, adsr, biquad, noise;

type
  TResonate = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set the filter for a resonance at the given frequency (Hz) and radius.
    procedure setResonance(frequency, radius: my_float);

  //! Set the filter for a notch at the given frequency (Hz) and radius.
    procedure setNotch(frequency, radius: my_float);

  //! Set the filter zero coefficients for contant resonance gain.
    procedure setEqualGainZeroes();

  //! Initiate the envelope with a key-on event.
    procedure keyOn();

  //! Signal a key-off event to the envelope.
    procedure keyOff();

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: my_float);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: my_float);

  //! Compute one output sample.
    function tick: my_float;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: my_float);

  protected
    ADSR: Tadsr;
    filter: tbiquad;
    noise: tnoise;
    poleFrequency, poleRadius, zeroFrequency, zeroRadius: my_float;
  end;

implementation

constructor TResonate.Create;
begin
  inherited Create(sr);
  adsr := TADSR.Create(srate);
  noise := TNoise.Create(srate);
  filter := TBiQuad.Create(srate);
  poleFrequency := 4000.0;
  poleRadius := 0.95;
  // Set the filter parameters.
  filter.setResonance(poleFrequency, poleRadius, True);
  zeroFrequency := 0.0;
  zeroRadius := 0.0;
end;

destructor TResonate.Destroy;
begin
  inherited Destroy;
  adsr.Free;
  filter.Free;
  noise.Free;
end;

procedure TResonate.keyOn;
begin
  adsr.keyOn();
end;

procedure TResonate.keyOff;
begin
  adsr.keyOff();
end;

procedure TResonate.noteOn;
begin
  adsr.setTarget(amplitude);
  keyOn();
  setResonance(frequency, poleRadius);
end;

procedure TResonate.noteOff;
begin
  keyOff();
end;

procedure TResonate.setResonance;
begin
  poleFrequency := frequency;
  if (frequency < 0.0) then
    poleFrequency := 0.0;
  poleRadius := radius;
  if (radius < 0.0) then
    poleRadius := 0.0
  else if (radius >= 1.0) then
    poleRadius := 0.9999;
  filter.setResonance(poleFrequency, poleRadius, True);
end;

procedure TResonate.setNotch;
begin
  zeroFrequency := frequency;
  if (frequency < 0.0) then
    zeroFrequency := 0.0;
  zeroRadius := radius;
  if (radius < 0.0) then
    zeroRadius := 0.0;
  filter.setNotch(zeroFrequency, zeroRadius);
end;

procedure TResonate.setEqualGainZeroes;
begin
  filter.setEqualGainZeroes;
end;

function TResonate.tick: my_float;
begin
  lastOutput := filter.tick(noise.tick());
  lastOutput := lastOutput * adsr.tick();
  Result := lastOutput;
end;

procedure TResonate.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = 2) then // 2
    setResonance(norm * srate * 0.5, poleRadius)
  else if (number = 4) then // 4
    setResonance(poleFrequency, norm * 0.9999)
  else if (number = 11) then // 11
    setNotch(norm * srate * 0.5, zeroRadius)
  else if (number = 1) then
    setNotch(zeroFrequency, norm)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

procedure TResonate.Clear;
begin
  filter.Clear;
end;

end.
