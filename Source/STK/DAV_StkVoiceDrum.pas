unit DAV_StkVoicDrum;

{
/***************************************************/
/*! \class VoicDrum
    \brief STK vocal drum sample player class.

    This class implements a drum sampling
    synthesizer using WvIn objects and one-pole
    filters.  The drum rawwave files are sampled
    at 22050 Hz, but will be appropriately
    interpolated for other sample rates.  You can
    specify the maximum polyphony (maximum number
    of simultaneous voices) via a #define in the
    Drummer.h.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, waveplayer, onepole;

const
  VOICE_NUMWAVES = 11;
  VOICE_POLYPHONY = 4;

type
  TVoicDrum = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Start a note with the given drum type and amplitude.
    procedure noteOn(instrument, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  protected
    waves: array[0..VOICE_POLYPHONY - 1] of twaveplayer;
    filters: array[0..VOICE_POLYPHONY - 1] of tonepole;
    sounding: array[0..VOICE_POLYPHONY - 1] of integer;
    nSounding: integer;
  end;

implementation

constructor TVoicDrum.Create;
var
  i: integer;
begin
  inherited Create(sr);
  for i := 0 to VOICE_POLYPHONY - 1 do
   begin
    filters[i] := TOnePole.Create(srate);
    sounding[i] := -1;
   end;
 // This counts the number of sounding voices.
  nSounding := 0;
end;

destructor TVoicDrum.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to nSounding - 2 do
    waves[i].Free;
  for i := 0 to VOICE_POLYPHONY - 1 do
    filters[i].Free;
end;

procedure TVoicDrum.noteOn;
const
  voicenames: array[0..VOICE_NUMWAVES - 1] of string = (
    'tak2.wav', 'tak1.wav', 'bee1.wav', 'dee1.wav', 'dee2.wav',
    'din1.wav', 'gun1.wav', 'jun1.wav', 'jun2.wav', 'tak3.wav', 'tak4.wav');
var
  gain: my_float;
  i, waveindex, notenum: integer;
  tempwv: ^twaveplayer;
  tempfilt: ^tonepole;
begin
  gain := amplitude;
  if (amplitude > 1.0) then
    gain := 1.0
  else if (amplitude < 0.0) then
    gain := 0;
  noteNum := round(instrument) mod 11;
  // Check first to see if there's already one like this sounding.
  waveIndex := -1;
  for i := 0 to VOICE_POLYPHONY - 1 do
    if (sounding[i] = noteNum) then
      waveIndex := i;

  if (waveIndex >= 0) then
   begin
    // Reset this sound.
    waves[waveIndex].reset;
    filters[waveIndex].setPole(0.999 - (gain * 0.6));
    filters[waveIndex].setGain(gain);
   end else
   begin
    if (nSounding = VOICE_POLYPHONY) then
     begin
      // If we're already at maximum polyphony, then preempt the oldest voice.
      waves[0].Free;
      filters[0].Clear;
      tempWv := @waves[0];
      tempFilt := @filters[0];
      // Re-order the list.
      for i := 0 to VOICE_POLYPHONY - 2 do
       begin
        waves[i] := waves[i + 1];
        filters[i] := filters[i + 1];
       end;
      waves[VOICE_POLYPHONY - 1] := tempWv^;
      filters[VOICE_POLYPHONY - 1] := tempFilt^;
     end else
      nSounding := nSounding + 1;
    sounding[nSounding - 1] := noteNum;
    waves[nSounding - 1] :=
      TWavePlayer.Create(srate, 'c:\stk\' + voicenames[notenum]);
    waves[nSounding - 1].SetOneShot(True);
    waves[nSounding - 1].reset;
    waves[nSounding - 1].setfrequency((1 / waves[nSounding - 1].length));
    filters[nSounding - 1].setPole(0.999 - (gain * 0.6));
    filters[nSounding - 1].setGain(gain);
   end;
end;

procedure TVoicDrum.noteOff;
var
  i: integer;
begin
  // Set all sounding wave filter gains low.
  i := 0;
  while (i < nSounding) do
   begin
    filters[i].setGain(amplitude * 0.01);
    i := i + 1;
   end;
end;

function TVoicDrum.tick: MY_FLOAT;
var
  output: MY_FLOAT;
  tempfilt: ^tonepole;
  i, j: integer;
begin
  output := 0.0;
  i := 0;
  while (i < nSounding) do
   begin
    if (waves[i].isFinished) then
     begin
      waves[i].Free;
      tempFilt := @filters[i];
      // Re-order the list.
      for j := i to nSounding - 2 do
       begin
        sounding[j] := sounding[j + 1];
        waves[j] := waves[j + 1];
        filters[j] := filters[j + 1];
       end;
      filters[nsounding - 2] := tempFilt^;
      filters[nsounding - 2].Clear;
      sounding[nsounding - 2] := -1;
      nSounding := nsounding - 1;
      i := i - 1;
     end
    else
      output := output + filters[i].tick(waves[i].tick);
    i := i + 1;
   end;
  Result := output;
end;

end.
