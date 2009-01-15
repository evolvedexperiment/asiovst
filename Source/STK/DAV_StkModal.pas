unit DAV_StkModal;

{
/***************************************************/
/*! \class TModal
    \brief STK resonance model instrument.

    This class contains an excitation wavetable,
    an envelope, an oscillator, and N resonances
    (non-sweeping BiQuad filters), where N is set
    during instantiation.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, envelope, biquad, onepole, lfo, waveplayer;

const
  maxModes = 20;

type
  TModal = class(TInstrmnt)
  public
  //! Class constructor, taking the desired number of modes to create.
    constructor Create(sr: my_float; modes: integer = 4);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: my_float);

  //! Set the ratio and radius for a specified mode filter.
    procedure setRatioAndRadius(modeIndex: integer; ratio, radius: my_float);

  //! Set the master gain.
    procedure setMasterGain(aGain: my_float);

  //! Set the direct gain.
    procedure setDirectGain(aGain: my_float);

  //! Set the gain for a specified mode filter.
    procedure setModeGain(modeIndex: integer; gain: MY_FLOAT);

  //! Initiate a strike with the given amplitude (0.0 - 1.0).
    procedure strike(amplitude: MY_FLOAT);

  //! Damp modes with a given decay factor (0.0 - 1.0).
    procedure damp(amplitude: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    envelope: tenvelope;
    wave: twaveplayer;
    filters: array[0..maxmodes - 1] of tbiquad;
    OnePole: tonepole;
    vibrato: tlfo;
    nModes: integer;
    vibratoGain, masterGain, directGain, stickHardness,
    strikePosition, baseFrequency: my_float;
    radii, ratios: array[0..maxmodes - 1] of my_float;
  end;

implementation

constructor TModal.Create;
var
  i: integer;
begin
  inherited Create(sr);
  if (nModes <= 0) then
    nModes := -nModes;
  if (nModes > maxModes) then
    nModes := maxModes;
  // We don't make the excitation wave here yet, because we don't know
  // what it's going to be.

  for i := 0 to maxmodes - 1 do
   begin
    filters[i] := tbiquad.Create(srate);
    filters[i].setEqualGainZeroes;
   end;

  envelope := TEnvelope.Create(srate);
  onepole := TOnePole.Create(srate);

  vibrato := TLFO.Create(srate);

  // Set some default values.
  vibrato.setFrequency(6.0);
  vibratoGain := 0.0;
  directGain := 0.0;
  masterGain := 1.0;
  baseFrequency := 440.0;

  Clear;

  stickHardness := 0.5;
  strikePosition := 0.561;
end;

destructor TModal.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  envelope.Free;
  onepole.Free;
  vibrato.Free;
  for i := 0 to maxModes - 1 do
    filters[i].Free;
end;

procedure TModal.Clear;
var
  i: integer;
begin
  onepole.Clear;
  for i := 0 to nModes - 1 do
    filters[i].Clear;
end;

procedure TModal.setFrequency;
var
  i: integer;
begin
  baseFrequency := frequency;
  for i := 0 to nModes - 1 do
    setRatioAndRadius(i, ratios[i], radii[i]);
end;

procedure TModal.setRatioAndRadius;
var
  nyquist, temp: my_float;
begin
  if (modeIndex < 0) then
    exit
  else if (modeIndex >= nModes) then
    exit;

  nyquist := srate * 0.5;
  if (ratio * baseFrequency < nyquist) then
    ratios[modeIndex] := ratio
  else
   begin
    temp := ratio;
    while (temp * baseFrequency > nyquist) do
      temp := temp * 0.5;
    ratios[modeIndex] := temp;
   end;
  radii[modeIndex] := radius;
  if (ratio < 0) then
    temp := -ratio
  else
    temp := ratio * baseFrequency;

  filters[modeIndex].setResonance(temp, radius);
end;

procedure TModal.setMasterGain;
begin
  masterGain := aGain;
end;

procedure TModal.setDirectGain;
begin
  directGain := aGain;
end;

procedure TModal.setModeGain;
begin
  if (modeIndex < 0) then
    exit
  else if (modeIndex >= nModes) then
    exit;
  filters[modeIndex].setGain(gain);
end;

procedure TModal.strike;
var
  temp, gain: my_float;
  i: integer;
begin
  gain := amplitude;
  if (amplitude < 0.0) then
    gain := 0.0
  else if (amplitude > 1.0) then
    gain := 1.0;

  envelope.setRate(1.0);
  envelope.setTarget(gain);
  onepole.setPole(1.0 - gain);
  envelope.tick;
  wave.reset;

  for i := 0 to nModes - 1 do
   begin
    if (ratios[i] < 0) then
      temp := -ratios[i]
    else
      temp := ratios[i] * baseFrequency;
    filters[i].setResonance(temp, radii[i]);
   end;
end;

procedure TModal.noteOn;
begin
  strike(amplitude);
  setFrequency(frequency);
end;

procedure TModal.noteOff;
begin
  // This calls damp, but inverts the meaning of amplitude (high
  // amplitude means fast damping).
  damp(1.0 - (amplitude * 0.03));
end;

procedure TModal.damp;
var
  temp: my_float;
  i: integer;
begin
  for i := 0 to nModes - 1 do
   begin
    if (ratios[i] < 0) then
      temp := -ratios[i]
    else
      temp := ratios[i] * baseFrequency;
    filters[i].setResonance(temp, radii[i] * amplitude);
   end;
end;

function TModal.tick: my_float;
var
  temp, temp2: my_float;
  i: integer;
begin
  temp := masterGain * onepole.tick(wave.tick * envelope.tick);

  temp2 := 0.0;
  for i := 0 to nModes - 1 do
    temp2 := temp2 + filters[i].tick(temp);

  temp2 := temp2 - temp2 * directGain;
  temp2 := temp2 + directGain * temp;

  if (vibratoGain <> 0.0) then
   begin
    // Calculate AM and apply to master out
    temp := 1.0 + (vibrato.tick * vibratoGain);
    temp2 := temp * temp2;
   end;

  lastOutput := temp2;
  Result := lastOutput;
end;

procedure TModal.controlChange;
begin
end;

end.
