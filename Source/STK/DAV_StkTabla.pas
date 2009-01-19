unit DAV_StkTabla;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK tabla drum class.

  This class implements a drum sampling synthesizer using WvIn objects and
  one-pole FFilters. The drum rawwave files are sampled at 22050 Hz, but will be
  appropriately interpolated for other sample rates. You can specify the
  maximum polyphony (maximum number of simultaneous voices) via a #define in
  the Drummer.h.

}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkWavePlayer, DAV_StkOnePole;

const
  CTablaNumWaves = 15;
  CTablaPolyphony = 4;

type
  TTabla = class(TInstrmnt)
  protected
    FWaves: array[0..CTablaPolyphony - 1] of TWavePlayer;
    FFilters: array[0..CTablaPolyphony - 1] of TOnePole;
    FSounding: array[0..CTablaPolyphony - 1] of Integer;
    FNumSounding: Integer;
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Start a note with the given drum type and amplitude.
    procedure noteOn(instrument, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

    // Compute one output sample.
    function tick: Single;
  end;

implementation

constructor TTabla.Create;
var
  i: Integer;
begin
  inherited Create(SampleRate);
  for i := 0 to CTablaPolyphony - 1 do
   begin
    FFilters[i] := TOnePole.Create(srate);
    FSounding[i] := -1;
   end;
  // This counts the number of FSounding voices.
  FNumSounding := 0;
end;

destructor TTabla.Destroy;
var
  i: Integer;
begin
  inherited Destroy;
  for i := 0 to FNumSounding - 2 do
    FWaves[i].Free;
  for i := 0 to CTablaPolyphony - 1 do
    FFilters[i].Free;
end;

procedure TTabla.noteOn;
const
  tablawaves: array[0..CTablaNumWaves - 1] of string =
    ('Drdak2.wav', 'Drdak3.wav', 'Drdak4.wav', 'Drddak1.wav',
    'Drdee1.wav', 'Drdee2.wav', 'Drdoo1.wav', 'Drdoo2.wav',
    'Drdoo3.wav', 'Drjun1.wav', 'Drjun2.wav', 'DrDoi1.wav',
    'DrDoi2.wav', 'DrTak1.wav', 'DrTak2.wav');
var
  gain: Single;
  i, waveindex, notenum: Integer;
  tempwv: ^TWavePlayer;
  tempfilt: ^TOnePole;
begin
  gain := amplitude;
  if (amplitude > 1.0) then
    gain := 1.0
  else if (amplitude < 0.0) then
    gain := 0;
  noteNum := round(instrument) mod 11;
  // Check first to see if there's already one like this sounding.
  waveIndex := -1;
  for i := 0 to CTablaPolyphony - 1 do
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
    if (FNumSounding = CTablaPolyphony) then
     begin
      // If we're already at maximum polyphony, then preempt the oldest voice.
      FWaves[0].Free;
      FFilters[0].Clear;
      tempWv := @FWaves[0];
      tempFilt := @FFilters[0];
      // Re-order the list.
      for i := 0 to CTablaPolyphony - 2 do
       begin
        FWaves[i] := FWaves[i + 1];
        FFilters[i] := FFilters[i + 1];
       end;
      FWaves[CTablaPolyphony - 1] := tempWv^;
      FFilters[CTablaPolyphony - 1] := tempFilt^;
     end else
      FNumSounding := FNumSounding + 1;
    FSounding[FNumSounding - 1] := noteNum;
    FWaves[FNumSounding - 1] :=
      TWavePlayer.Create(srate, 'c:\stk\' + tablawaves[notenum]);
    FWaves[FNumSounding - 1].SetOneShot(True);
    FWaves[FNumSounding - 1].reset;
    FWaves[FNumSounding - 1].setfrequency((1 / FWaves[FNumSounding - 1].length));
    FFilters[FNumSounding - 1].setPole(0.999 - (gain * 0.6));
    FFilters[FNumSounding - 1].setGain(gain);
   end;
end;

procedure TTabla.noteOff;
var
  i: Integer;
begin
  // Set all FSounding wave filter gains low.
  i := 0;
  while (i < FNumSounding) do
   begin
    FFilters[i].setGain(amplitude * 0.01);
    i := i + 1;
   end;
end;

function TTabla.tick: Single;
var
  output: Single;
  tempfilt: ^TOnePole;
  i, j: Integer;
begin
  output := 0.0;
  i := 0;
  while (i < FNumSounding) do
   begin
    if (FWaves[i].isFinished) then
     begin
      FWaves[i].Free;
      tempFilt := @FFilters[i];
      // Re-order the list.
      for j := i to FNumSounding - 2 do
       begin
        FSounding[j] := FSounding[j + 1];
        FWaves[j] := FWaves[j + 1];
        FFilters[j] := FFilters[j + 1];
       end;
      FFilters[FNumSounding - 2] := tempFilt^;
      FFilters[FNumSounding - 2].Clear;
      FSounding[FNumSounding - 2] := -1;
      FNumSounding := FNumSounding - 1;
      i := i - 1;
     end
    else
      output := output + FFilters[i].tick(FWaves[i].tick);
    i := i + 1;
   end;
  Result := output;
end;

end.
