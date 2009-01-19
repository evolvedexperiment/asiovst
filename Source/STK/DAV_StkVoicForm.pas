unit DAV_StkVoicForm;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  Four formant synthesis instrument.

   This instrument contains an excitation singing wavetable (looping wave
   with random and periodic vibrato, smoothing on Frequency, etc.),
   excitation FNoise, and four sweepable complex resonances.

  Measured formant data is included, and enough data is there to support
  either parallel or cascade synthesis.  In the floating point case cascade
  synthesis is the most natural so that's what you'll find here.

  Control Change Numbers:
    - FVoiced/Unvoiced Mix = 2
    - Vowel/Phoneme Selection = 4
    - Vibrato Frequency = 11
    - Vibrato Gain = 1
    - Loudness (Spectral Tilt) = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkEnvelope, DAV_StkNoise, DAV_StkSingWave,
  DAV_StkFormantSweep, DAV_StkOnePole, DAV_StkOneZero, DAV_StkPhonemes, Math;

type
  TStkVoiceFormant = class(TStkInstrument)
  protected
    FVoiced: TSingWave;
    FPhonemes: TPhonemes;
    FNoise: TNoise;
    FNoiseEnv: TEnvelope;
    FFilters: array[0..3] of TFormswep;
    FOnePole: TOnepole;
    FOneZero: TOnezero;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

    // Set instrument parameters for the given phoneme.  Returns FALSE if phoneme not found.
    function setPhoneme(phoneme: string): boolean;

    // Set the FVoiced component gain.
    procedure setVoiced(vGain: Single);

    // Set the unvoiced component gain.
    procedure setUnVoiced(nGain: Single);

    // Set the sweep rate for a particular formant filter (0-3).
    procedure setFilterSweepRate(whichOne: integer; rate: Single);

    // Set FVoiced component pitch sweep rate.
    procedure setPitchSweepRate(rate: Single);

    // Start the voice.
    procedure Speak;

    // Stop the voice.
    procedure Quiet;

    // Start a note with the given Frequency and amplitude.
    procedure NoteOn(Frequency, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(amplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(number: integer; Value: Single);
  end;

implementation

constructor TVoicForm.Create;
var
  i: integer;
begin
  inherited Create(SampleRate);
  FVoiced := TSingWave.Create(SampleRate, 'impuls20.wav');
  FVoiced.setGainRate(0.001);
  FVoiced.setGainTarget(0.0);
  FNoise := TNoise.Create(SampleRate);
  for i := 0 to 3 do
   begin
    FFilters[i] := TFormswep.Create(SampleRate);
    FFilters[i].setSweepRate(0.001);
   end;

  FPhonemes := TPhonemes.Create(SampleRate);
  FOneZero := TOnezero.Create(SampleRate);
  FOneZero.setZero(-0.9);
  FOnePole := TOnepole.Create(SampleRate);
  FOnePole.setPole(0.9);

  FNoiseEnv := TEnvelope.Create(SampleRate);
  FNoiseEnv.setRate(0.001);
  FNoiseEnv.setTarget(0.0);

  setPhoneme('eee');
  Clear;
end;

destructor TVoicForm.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  FVoiced.Free;
  FNoise.Free;
  FOneZero.Free;
  FOnePole.Free;
  FNoiseEnv.Free;
  FPhonemes.Free;
  for i := 0 to 3 do
    FFilters[i].Free;
end;

procedure TVoicForm.Clear;
var
  i: integer;
begin
  FOneZero.Clear;
  FOnePole.Clear;
  for i := 0 to 3 do
    FFilters[i].Clear;
end;

procedure TVoicForm.setFrequency;
var
  Freakency: Single;
begin
  Freakency := Frequency;
  if (Frequency <= 0.0)
   then Freakency := 220.0;
  FVoiced.setFrequency(Freakency);
end;

function TVoicForm.setPhoneme(phoneme: string): boolean;
var
  found : Boolean;
  i     : Integer;
begin
  found := False;
  i := 0;
  while ((i < 32) and (not found)) do
   begin
    if (FPhonemes.Name(i) = phoneme) then
     begin
      found := True;
      FFilters[0].setTargets(FPhonemes.formantFrequency(i, 0), FPhonemes.formantRadius(i, 0),
        power(10.0, FPhonemes.formantGain(i, 0) / 20.0));
      FFilters[1].setTargets(FPhonemes.formantFrequency(i, 1), FPhonemes.formantRadius(i, 1),
        power(10.0, FPhonemes.formantGain(i, 1) / 20.0));
      FFilters[2].setTargets(FPhonemes.formantFrequency(i, 2), FPhonemes.formantRadius(i, 2),
        power(10.0, FPhonemes.formantGain(i, 2) / 20.0));
      FFilters[3].setTargets(FPhonemes.formantFrequency(i, 3), FPhonemes.formantRadius(i, 3),
        power(10.0, FPhonemes.formantGain(i, 3) / 20.0));
      setVoiced(FPhonemes.voiceGain(i));
      setUnVoiced(FPhonemes.noiseGain(i));
     end;
    i := i + 1;
   end;
  Result := found;
end;

procedure TVoicForm.setVoiced;
begin
  FVoiced.setGainTarget(vGain);
end;

procedure TVoicForm.setUnVoiced;
begin
  FNoiseEnv.setTarget(nGain);
end;

procedure TVoicForm.setFilterSweepRate;
begin
  if (whichOne < 0) or (whichOne > 3) then
    exit;
  FFilters[whichOne].setSweepRate(rate);
end;

procedure TVoicForm.setPitchSweepRate;
begin
  FVoiced.setSweepRate(rate);
end;

procedure TVoicForm.Speak;
begin
  FVoiced.NoteOn;
end;

procedure TVoicForm.uiet;
begin
  FVoiced.NoteOff;
  FNoiseEnv.setTarget(0.0);
end;

procedure TVoicForm.NoteOn;
begin
  setFrequency(Frequency);
  FVoiced.setGainTarget(amplitude);
  FOnePole.setPole(0.97 - (amplitude * 0.2));
end;

procedure TVoicForm.NoteOff;
begin
  uiet;
end;

function TVoicForm.Tick: Single;
var
  temp: Single;
begin
  temp := FOnePole.Tick(FOneZero.Tick(FVoiced.Tick));
  temp := temp + FNoiseEnv.Tick * FNoise.Tick;
  lastOutput := FFilters[0].Tick(temp);
  lastOutput := lastoutput + FFilters[1].Tick(temp);
  lastOutput := lastoutput + FFilters[2].Tick(temp);
  lastOutput := lastoutput + FFilters[3].Tick(temp);
  Result := lastOutput;
end;

procedure TVoicForm.ControlChange;
var
  temp, norm: Single;
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
    FFilters[0].setTargets(temp * FPhonemes.formantFrequency(i, 0),
      FPhonemes.formantRadius(i, 0), power(10.0, FPhonemes.formantGain(i, 0) / 20.0));
    FFilters[1].setTargets(temp * FPhonemes.formantFrequency(i, 1),
      FPhonemes.formantRadius(i, 1), power(10.0, FPhonemes.formantGain(i, 1) / 20.0));
    FFilters[2].setTargets(temp * FPhonemes.formantFrequency(i, 2),
      FPhonemes.formantRadius(i, 2), power(10.0, FPhonemes.formantGain(i, 2) / 20.0));
    FFilters[3].setTargets(temp * FPhonemes.formantFrequency(i, 3),
      FPhonemes.formantRadius(i, 3), power(10.0, FPhonemes.formantGain(i, 3) / 20.0));
    setVoiced(FPhonemes.voiceGain(i));
    setUnVoiced(FPhonemes.noiseGain(i));
   end
  else if (number = __SK_ModFrequency_) then // 11
    FVoiced.setVibratoRate(norm * 12.0)  // 0 to 12 Hz
  else if (number = __SK_ModWheel_) then // 1
    FVoiced.setVibratoGain(norm * 0.2)
  else if (number = __SK_AfterTouch_Cont_) then
   begin // 128
    setVoiced(norm);
    FOnePole.setPole(0.97 - (norm * 0.2));
   end;
end;

end.
