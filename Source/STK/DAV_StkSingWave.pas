unit DAV_StkSingWave;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK "singing" looped soundfile class.

  This class contains all that is needed to make a pitched musical sound, like
  a simple voice or violin.  In general, it will not be used alone because of
  munchkinification effects from pitch shifting.  It will be used as an
  excitation source for other instruments.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkWavePlayer, DAV_StkModulate, DAV_StkEnvelope;

type
  TSingWave = class(TStk)
  public
    // Class constructor taking filename argument.
  {
    An StkError will be thrown if the file is not found, its format is
    unknown, or a read error occurs.
  }
    constructor Create(SampleRate: Single; FileName: string);

    // Class destructor.
    destructor Destroy;

    // Reset file to beginning.
    procedure Reset;

    // Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

    // Set the vibrato frequency in Hz.
    procedure setVibratoRate(aRate: Single);

    // Set the vibrato gain.
    procedure setVibratoGain(gain: Single);

    // Set the random-ness amount.
    procedure setRandomGain(gain: Single);

    // Set the sweep FRate.
    procedure setSweepRate(aRate: Single);

    // Set the gain FRate.
    procedure setGainRate(aRate: Single);

    // Set the gain target value.
    procedure setGainTarget(target: Single);

    // Start a note.
    procedure noteOn;

    // Stop a note.
    procedure noteOff;

    // Return the last output value.
    function lastOut: Single;

    // Compute one output sample.
    function tick: Single;

  protected

    FWave: TWavePlayer;
    FModulator: TModulate;
    FEnvelope: TEnvelope;
    FPitchEnvelope: TEnvelope;
    FRate, FSweepRate, FLastOutput: Single;
  end;

implementation

constructor TSingWave.Create;
begin
  inherited Create(SampleRate);
  FWave := TWavePlayer.Create(SampleRate, FileName);
  FWave.SetOneShot(False);
  FRate := 1.0;
  FSweepRate := 0.001;
  FModulator := TModulate.Create(SampleRate);
  FModulator.setVibratoRate(6.0);
  FModulator.setVibratoGain(0.04);
  FModulator.setRandomGain(0.005);
  FEnvelope := TEnvelope.Create(SampleRate);
  FPitchEnvelope := TEnvelope.Create(SampleRate);
  setFrequency(75.0);
  FPitchEnvelope.setRate(1.0);
  tick;
  tick;
  FPitchEnvelope.setRate(FSweepRate * FRate);
end;

destructor TSingWave.Destroy;
begin
  inherited Destroy;
  FWave.Free;
  FModulator.Free;
  FEnvelope.Free;
  FPitchEnvelope.Free;
end;

procedure TSingWave.Reset;
begin
  FWave.Reset;
  FLastOutput := 0.0;
end;

procedure TSingWave.setFrequency;
var
  temp: Single;
begin
  temp := FRate;
  FRate := frequency / SampleRate;
  temp := temp - FRate;
  if (temp < 0) then
    temp := -temp;
  FPitchEnvelope.setTarget(FRate);
  FPitchEnvelope.setRate(FSweepRate * temp);
end;

procedure TSingWave.setVibratoRate;
begin
  FModulator.setVibratoRate(aRate);
end;

procedure TSingWave.setVibratoGain;
begin
  FModulator.setVibratoGain(gain);
end;

procedure TSingWave.setRandomGain;
begin
  FModulator.setRandomGain(gain);
end;

procedure TSingWave.setSweepRate;
begin
  FSweepRate := aRate;
end;

procedure TSingWave.setGainRate;
begin
  FEnvelope.setRate(aRate);
end;

procedure TSingWave.setGainTarget;
begin
  FEnvelope.setTarget(target);
end;

procedure TSingWave.noteOn;
begin
  FEnvelope.keyOn;
end;

procedure TSingWave.noteOff;
begin
  FEnvelope.keyOff;
end;

function TSingWave.tick: Single;
var
  newrate: Single;
begin
  // Set the FWave FRate.
  newRate := FPitchEnvelope.tick;
  newRate := newrate + newRate * FModulator.tick;
  FWave.setfrequency(newRate);
  FLastOutput := FWave.tick;
  FLastOutput := FLastOutput * FEnvelope.tick;
  Result := FLastOutput;
end;

function TSingWave.lastOut: Single;
begin
  Result := FLastOutput;
end;

end.
