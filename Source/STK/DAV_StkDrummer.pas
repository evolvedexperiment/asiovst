unit DAV_StkDrummer;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK drum sample player class.

  This class implements a drum sampling synthesizer using WvIn objects and
  one-pole FFilters.  The drum rawwave files are sampled at 22050 Hz, but will
  be appropriately interpolated for other sample rates. You can specify the
  maximum polyphony (maximum number of simultaneous voices)
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkWaveplayer, DAV_StkOnePole,
  Dialogs, SysUtils;

const
  CDrumWaveCount = 11;
  CDrumPolyphony = 8;

type
  TDrummer = class(TInstrmnt)
  protected
    FWaves     : array[0..CDrumPolyphony - 1] of TWavePlayer;
    FFilters   : array[0..CDrumPolyphony - 1] of TOnePole;
    FSounding  : array[0..CDrumPolyphony - 1] of Integer;
    FNSounding : Integer;
  public
    constructor Create(SampleRate: Single);
    destructor Destroy;

  //! Start a note with the given drum type and amplitude.
  {*!
    Use general MIDI drum instrument numbers, converted to
    frequency values as if MIDI note numbers, to select a
    particular instrument.
  *}
    procedure NoteOn(instrument, amplitude: Single);

  //! Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(amplitude: Single);

  //! Compute one output sample.
    function Tick: Single;
  end;

implementation

// Not really General MIDI yet.  Coming soon.
const
  genMIDIMap: array[0..127] of Byte =
    (0, 0, 0, 0, 0, 0, 0, 0,    // 0-7
     0, 0, 0, 0, 0, 0, 0, 0,    // 8-15
     0, 0, 0, 0, 0, 0, 0, 0,    // 16-23
     0, 0, 0, 0, 0, 0, 0, 0,    // 24-31
     0, 0, 0, 0, 1, 0, 2, 0,    // 32-39
     2, 3, 6, 3, 6, 4, 7, 4,    // 40-47
     5, 8, 5, 0, 0, 0, 10, 0,   // 48-55
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
    'dope.wav',
    'bassdrum.wav',
    'snardrum.wav',
    'tomlowdr.wav',
    'tommiddr.wav',
    'tomhidrm.wav',
    'hihatcym.wav',
    'ridecymb.wav',
    'crashcym.wav',
    'cowbell1.wav',
    'tambourn.wav'
    );

constructor TDrummer.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  for i := 0 to CDrumPolyphony - 1 do
   begin
    FFilters[i] := TOnePole.Create(srate);
    FSounding[i] := -1;
   end;
 // This counts the number of FSounding voices.
  FNSounding := 0;
end;

destructor TDrummer.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to FNSounding - 2 do
    FWaves[i].Free;
  for i := 0 to CDrumPolyphony - 1 do
    FFilters[i].Free;
end;

procedure TDrummer.NoteOn;
var
  gain: Single;
  i, waveindex, notenum: Integer;
  tempWv: ^TWavePlayer;
  tempFilt: ^TOnePole;
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
  for i := 0 to CDrumPolyphony - 1 do
    if (FSounding[i] = noteNum) then
      waveIndex := i;

  if (waveIndex >= 0) then
   begin
    // Reset this sound.
    FWaves[waveIndex].reset;
    FFilters[waveIndex].setPole(0.999 - (gain * 0.6));
    FFilters[waveIndex].setGain(gain);
   end else
   begin
    if (FNSounding = CDrumPolyphony) then
     begin
      // If we're already at maximum polyphony, then preempt the oldest voice.
      FWaves[0].Free;
      FFilters[0].Clear;
      tempWv := @FWaves[0];
      tempFilt := @FFilters[0];
      // Re-order the list.
      for i := 0 to CDrumPolyphony - 2 do
       begin
        FWaves[i] := FWaves[i + 1];
        FFilters[i] := FFilters[i + 1];
       end;
      FWaves[CDrumPolyphony - 1] := tempWv^;
      FFilters[CDrumPolyphony - 1] := tempFilt^;
     end
    else
      FNSounding := FNSounding + 1;

    FSounding[FNSounding - 1] := noteNum;

    FWaves[FNSounding - 1] := TWavePlayer.Create(
      srate, wavenames[genmidimap[notenum]]);
    FWaves[FNSounding - 1].SetOneShot(True);
    FWaves[FNSounding - 1].reset;
//    inputbox(floattostr(FWaves[FNSounding-1].length),'','');
{    if (srate <> 22050.0) then
      FWaves[FNSounding-1].setRate( 22050.0 / srate );}
    FWaves[FNSounding - 1].setfrequency((1 / FWaves[FNSounding - 1].length));
    FFilters[FNSounding - 1].setPole(0.999 - (gain * 0.6));
    FFilters[FNSounding - 1].setGain(gain);
   end;
end;

procedure TDrummer.NoteOff;
var
  i: Integer;
begin
  // Set all FSounding wave filter gains low.
  i := 0;
  while (i < FNSounding) do
   begin
    FFilters[i].setGain(amplitude * 0.01);
    i := i + 1;
   end;
end;

function TDrummer.Tick: Single;
var
  output: Single;
  tempfilt: ^TOnePole;
  i, j: Integer;
begin
  output := 0.0;
  i := 0;
  while (i < FNSounding) do
   begin
    if (FWaves[i].isFinished) then
     begin
      FWaves[i].Free;
      tempFilt := @FFilters[i];
      // Re-order the list.
      for j := i to FNSounding - 2 do
       begin
        FSounding[j] := FSounding[j + 1];
        FWaves[j] := FWaves[j + 1];
        FFilters[j] := FFilters[j + 1];
       end;
      FFilters[FNSounding - 2] := tempFilt^;
      FFilters[FNSounding - 2].Clear;
      FSounding[FNSounding - 2] := -1;
      FNSounding := FNSounding - 1;
      i := i - 1;
     end
    else
      output := output + FFilters[i].Tick(FWaves[i].Tick);
    i := i + 1;
   end;
  Result := output;
end;

end.
