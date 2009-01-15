unit DAV_StkDrummer;

{
/***************************************************/
/*! \class TDrummer
    \brief STK drum sample player class.

    This class implements a drum sampling
    synthesizer using WvIn objects and one-pole
    filters.  The drum rawwave files are sampled
    at 22050 Hz, but will be appropriately
    interpolated for other sample rates.  You can
    specify the maximum polyphony (maximum number
    of simultaneous voices) via a #define in the
    TDrummer.h.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkWaveplayer, DAV_StkOnePole,
  Dialogs, SysUtils;

const
  DRUM_NUMWAVES = 11;
  DRUM_POLYPHONY = 8;

type
  TDrummer = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Start a note with the given drum type and amplitude.
  {*!
    Use general MIDI drum instrument numbers, converted to
    frequency values as if MIDI note numbers, to select a
    particular instrument.
  *}
    procedure noteOn(instrument, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  protected
    waves: array[0..DRUM_POLYPHONY - 1] of Twaveplayer;
    filters: array[0..DRUM_POLYPHONY - 1] of tonepole;
    sounding: array[0..DRUM_POLYPHONY - 1] of integer;
    nSounding: integer;
  end;

implementation

// Not really General MIDI yet.  Coming soon.
const
  genMIDIMap: array[0..127] of byte =
    (0, 0, 0, 0, 0, 0, 0, 0,    // 0-7
    0, 0, 0, 0, 0, 0, 0, 0,    // 8-15
    0, 0, 0, 0, 0, 0, 0, 0,    // 16-23
    0, 0, 0, 0, 0, 0, 0, 0,    // 24-31
    0, 0, 0, 0, 1, 0, 2, 0,    // 32-39
    2, 3, 6, 3, 6, 4, 7, 4,    // 40-47
    5, 8, 5, 0, 0, 0, 10, 0,    // 48-55
    9, 0, 0, 0, 0, 0, 0, 0,    // 56-63
    0, 0, 0, 0, 0, 0, 0, 0,    // 64-71
    0, 0, 0, 0, 0, 0, 0, 0,    // 72-79
    0, 0, 0, 0, 0, 0, 0, 0,    // 80-87
    0, 0, 0, 0, 0, 0, 0, 0,    // 88-95
    0, 0, 0, 0, 0, 0, 0, 0,    // 96-103
    0, 0, 0, 0, 0, 0, 0, 0,    // 104-111
    0, 0, 0, 0, 0, 0, 0, 0,    // 112-119
    0, 0, 0, 0, 0, 0, 0, 0     // 120-127
    );

const
  waveNames: array[0..DRUM_NUMWAVES - 1] of string =
    (
    'd:\moomoo\wav\dope.wav',
    'd:\moomoo\wav\bassdrum.wav',
    'd:\moomoo\wav\snardrum.wav',
    'd:\moomoo\wav\tomlowdr.wav',
    'd:\moomoo\wav\tommiddr.wav',
    'd:\moomoo\wav\tomhidrm.wav',
    'd:\moomoo\wav\hihatcym.wav',
    'd:\moomoo\wav\ridecymb.wav',
    'd:\moomoo\wav\crashcym.wav',
    'd:\moomoo\wav\cowbell1.wav',
    'd:\moomoo\wav\tambourn.wav'
    );

constructor TDrummer.Create;
var
  i: integer;
begin
  inherited Create(sr);
  for i := 0 to DRUM_POLYPHONY - 1 do
   begin
    filters[i] := TOnePole.Create(srate);
    sounding[i] := -1;
   end;
 // This counts the number of sounding voices.
  nSounding := 0;
end;

destructor TDrummer.Destroy;
var
  i: integer;
begin
  inherited Destroy;
  for i := 0 to nSounding - 2 do
    waves[i].Free;
  for i := 0 to DRUM_POLYPHONY - 1 do
    filters[i].Free;
end;

procedure TDrummer.noteOn;
var
  gain: my_float;
  i, waveindex, notenum: integer;
  tempWv: ^twaveplayer;
  tempFilt: ^tonepole;
begin
  gain := amplitude;
  if (amplitude > 1.0) then
    gain := 1.0
  else if (amplitude < 0.0) then
    gain := 0;

  // Yes, this is tres kludgey.
//  int noteNum:=(int) ((12*log(instrument/220.0)/log(2.0)) + 57.01);
  notenum := round(instrument);

  // Check first to see if there's already one like this sounding.
  waveIndex := -1;
  for i := 0 to DRUM_POLYPHONY - 1 do
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
    if (nSounding = DRUM_POLYPHONY) then
     begin
      // If we're already at maximum polyphony, then preempt the oldest voice.
      waves[0].Free;
      filters[0].Clear;
      tempWv := @waves[0];
      tempFilt := @filters[0];
      // Re-order the list.
      for i := 0 to DRUM_POLYPHONY - 2 do
       begin
        waves[i] := waves[i + 1];
        filters[i] := filters[i + 1];
       end;
      waves[DRUM_POLYPHONY - 1] := tempWv^;
      filters[DRUM_POLYPHONY - 1] := tempFilt^;
     end
    else
      nSounding := nSounding + 1;

    sounding[nSounding - 1] := noteNum;

    waves[nSounding - 1] := TWavePlayer.Create(
      srate, wavenames[genmidimap[notenum]]);
    waves[nSounding - 1].SetOneShot(True);
    waves[nSounding - 1].reset;
//    inputbox(floattostr(waves[nSounding-1].length),'','');
{    if (srate <> 22050.0) then
      waves[nSounding-1].setRate( 22050.0 / srate );}
    waves[nSounding - 1].setfrequency((1 / waves[nSounding - 1].length));
    filters[nSounding - 1].setPole(0.999 - (gain * 0.6));
    filters[nSounding - 1].setGain(gain);
   end;
end;

procedure TDrummer.noteOff;
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

function TDrummer.tick: MY_FLOAT;
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
