unit DAV_StkVoicForm;

{
/***************************************************/
/*! \class TVoicForm
    \brief Four formant synthesis instrument.

    This instrument contains an excitation singing
    wavetable (looping wave with random and
    periodic vibrato, smoothing on frequency,
    etc.), excitation noise, and four sweepable
    complex resonances.

    Measured formant data is included, and enough
    data is there to support either parallel or
    cascade synthesis.  In the floating point case
    cascade synthesis is the most natural so
    that's what you'll find here.

    Control Change Numbers: 
       - Voiced/Unvoiced Mix:=2
       - Vowel/Phoneme Selection:=4
       - Vibrato Frequency:=11
       - Vibrato Gain:=1
       - Loudness (Spectral Tilt):=128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, envelope, noise, singwave, formswep, onepole,
  onezero, Math, phonemes;

type
  TVoicForm = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Set instrument parameters for the given phoneme.  Returns FALSE if phoneme not found.
    function setPhoneme(phoneme: string): boolean;

  //! Set the voiced component gain.
    procedure setVoiced(vGain: MY_FLOAT);

  //! Set the unvoiced component gain.
    procedure setUnVoiced(nGain: MY_FLOAT);

  //! Set the sweep rate for a particular formant filter (0-3).
    procedure setFilterSweepRate(whichOne: integer; rate: MY_FLOAT);

  //! Set voiced component pitch sweep rate.
    procedure setPitchSweepRate(rate: MY_FLOAT);

  //! Start the voice.
    procedure speak;

  //! Stop the voice.
    procedure quiet;

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    voiced: tsingwave;
    ph: tphonemes;
    Noise: tnoise;
    noiseEnv: tenvelope;
    filters: array[0..3] of tformswep;
    OnePole: tonepole;
    OneZero: tonezero;
  end;

implementation

constructor TVoicForm.Create;
var
  i: integer;
begin
  inherited Create(sr);
  voiced := TSingWave.Create(srate, 'c:\stk\impuls20.wav');
  voiced.setGainRate(0.001);
  voiced.setGainTarget(0.0);
  noise := TNoise.Create(srate);
  for i := 0 to 3 do
   begin
    filters[i] := TFormSwep.Create(srate);
    filters[i].setSweepRate(0.001);
   end;

  ph := tphonemes.Create(srate);
  onezero := TOneZero.Create(srate);
  onezero.setZero(-0.9);
  onepole := TOnePole.Create(srate);
  onepole.setPole(0.9);

  noiseEnv := TEnvelope.Create(srate);
  noiseEnv.setRate(0.001);
  noiseEnv.setTarget(0.0);

  setPhoneme('eee');
  Clear;
end;

destructor TVoicForm.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  voiced.Free;
  noise.Free;
  onezero.Free;
  onepole.Free;
  noiseEnv.Free;
  ph.Free;
  for i := 0 to 3 do
    filters[i].Free;
end;

procedure TVoicForm.Clear;
var
  i: integer;
begin
  onezero.Clear;
  onepole.Clear;
  for i := 0 to 3 do
    filters[i].Clear;
end;

procedure TVoicForm.setFrequency;
var
  freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;
  voiced.setFrequency(freakency);
end;

function TVoicForm.setPhoneme(phoneme: string): boolean;
var
  found: boolean;
  i: integer;
begin
  found := False;
  i := 0;
  while ((i < 32) and (not found)) do
   begin
    if (ph.Name(i) = phoneme) then
     begin
      found := True;
      filters[0].setTargets(ph.formantFrequency(i, 0), ph.formantRadius(i, 0),
        power(10.0, ph.formantGain(i, 0) / 20.0));
      filters[1].setTargets(ph.formantFrequency(i, 1), ph.formantRadius(i, 1),
        power(10.0, ph.formantGain(i, 1) / 20.0));
      filters[2].setTargets(ph.formantFrequency(i, 2), ph.formantRadius(i, 2),
        power(10.0, ph.formantGain(i, 2) / 20.0));
      filters[3].setTargets(ph.formantFrequency(i, 3), ph.formantRadius(i, 3),
        power(10.0, ph.formantGain(i, 3) / 20.0));
      setVoiced(ph.voiceGain(i));
      setUnVoiced(ph.noiseGain(i));
     end;
    i := i + 1;
   end;
  Result := found;
end;

procedure TVoicForm.setVoiced;
begin
  voiced.setGainTarget(vGain);
end;

procedure TVoicForm.setUnVoiced;
begin
  noiseEnv.setTarget(nGain);
end;

procedure TVoicForm.setFilterSweepRate;
begin
  if (whichOne < 0) or (whichOne > 3) then
    exit;
  filters[whichOne].setSweepRate(rate);
end;

procedure TVoicForm.setPitchSweepRate;
begin
  voiced.setSweepRate(rate);
end;

procedure TVoicForm.speak;
begin
  voiced.noteOn;
end;

procedure TVoicForm.quiet;
begin
  voiced.noteOff;
  noiseEnv.setTarget(0.0);
end;

procedure TVoicForm.noteOn;
begin
  setFrequency(frequency);
  voiced.setGainTarget(amplitude);
  onepole.setPole(0.97 - (amplitude * 0.2));
end;

procedure TVoicForm.noteOff;
begin
  quiet;
end;

function TVoicForm.tick: my_float;
var
  temp: my_float;
begin
  temp := onepole.tick(onezero.tick(voiced.tick));
  temp := temp + noiseEnv.tick * noise.tick;
  lastOutput := filters[0].tick(temp);
  lastOutput := lastoutput + filters[1].tick(temp);
  lastOutput := lastoutput + filters[2].tick(temp);
  lastOutput := lastoutput + filters[3].tick(temp);
  Result := lastOutput;
end;

procedure TVoicForm.controlChange;
var
  temp, norm: my_float;
  i: integer;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;
  if (number = __SK_Breath_) then
   begin // 2
    setVoiced(1.0 - norm);
    setUnVoiced(0.01 * norm);
   end
  else if (number = __SK_FootControl_) then
   begin // 4
    temp := 0.0;
    i := round(norm * 128);
    if (i < 32) then
      temp := 0.9
    else if (i < 64) then
     begin
      i := i - 32;
      temp := 1.0;
     end
    else if (i < 96) then
     begin
      i := i - 64;
      temp := 1.1;
     end
    else if (i < 128) then
     begin
      i := i - 96;
      temp := 1.2;
     end
    else if (i = 128) then
     begin
      i := 0;
      temp := 1.4;
     end;
    filters[0].setTargets(temp * ph.formantFrequency(i, 0),
      ph.formantRadius(i, 0), power(10.0, ph.formantGain(i, 0) / 20.0));
    filters[1].setTargets(temp * ph.formantFrequency(i, 1),
      ph.formantRadius(i, 1), power(10.0, ph.formantGain(i, 1) / 20.0));
    filters[2].setTargets(temp * ph.formantFrequency(i, 2),
      ph.formantRadius(i, 2), power(10.0, ph.formantGain(i, 2) / 20.0));
    filters[3].setTargets(temp * ph.formantFrequency(i, 3),
      ph.formantRadius(i, 3), power(10.0, ph.formantGain(i, 3) / 20.0));
    setVoiced(ph.voiceGain(i));
    setUnVoiced(ph.noiseGain(i));
   end
  else if (number = __SK_ModFrequency_) then // 11
    voiced.setVibratoRate(norm * 12.0)  // 0 to 12 Hz
  else if (number = __SK_ModWheel_) then // 1
    voiced.setVibratoGain(norm * 0.2)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    setVoiced(norm);
    onepole.setPole(0.97 - (norm * 0.2));
   end;
end;

end.
