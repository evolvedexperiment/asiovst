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
  DAV_Common, DAV_StkCommon, DAV_StkWavePlayer, DAV_StkModulate,
  DAV_StkEnvelope;

type
  TStkSingWave = class(TStk)
  protected
    FWave          : TStkWavePlayer;
    FModulator     : TStkModulate;
    FEnvelope      : TStkEnvelope;
    FPitchEnvelope : TStkEnvelope;
    FRate          : Single;
    FSweepRate     : Single;
    FLastOutput    : Single;
  public
    // Class constructor taking filename argument.
  {
    An StkError will be thrown if the file is not found, its format is
    unknown, or a read error occurs.
  }
    constructor Create(const SampleRate: Single; const FileName: string); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset file to beginning.
    procedure Reset;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(Frequency: Single);

    // Set the vibrato frequency in Hz.
    procedure SetVibratoRate(aRate: Single);

    // Set the vibrato gain.
    procedure SetVibratoGain(gain: Single);

    // Set the random-ness amount.
    procedure SetRandomGain(gain: Single);

    // Set the sweep FRate.
    procedure SetSweepRate(aRate: Single);

    // Set the gain FRate.
    procedure SetGainRate(aRate: Single);

    // Set the gain Target value.
    procedure SetGainTarget(Target: Single);

    // Start a note.
    procedure NoteOn;

    // Stop a note.
    procedure NoteOff;

    // Return the last output value.
    function LastOut: Single;

    // Compute one output sample.
    function Tick: Single;
  end;

implementation

constructor TStkSingWave.Create;
begin
  inherited Create(SampleRate);
  FWave := TStkWavePlayer.Create(SampleRate, FileName);
  FWave.SetOneShot(False);
  FRate := 1.0;
  FSweepRate := 0.001;
  FModulator := TStkModulate.Create(SampleRate);
  FModulator.SetVibratoRate(6.0);
  FModulator.SetVibratoGain(0.04);
  FModulator.SetRandomGain(0.005);
  FEnvelope := TStkEnvelope.Create(SampleRate);
  FPitchEnvelope := TStkEnvelope.Create(SampleRate);
  setFrequency(75.0);
  FPitchEnvelope.SetRate(1.0);
  Tick;
  Tick;
  FPitchEnvelope.SetRate(FSweepRate * FRate);
end;

destructor TStkSingWave.Destroy;
begin
 FreeAndNil(FWave);
 FreeAndNil(FModulator);
 FreeAndNil(FEnvelope);
 FreeAndNil(FPitchEnvelope);
 inherited Destroy;
end;

procedure TStkSingWave.Reset;
begin
  FWave.Reset;
  FLastOutput := 0.0;
end;

procedure TStkSingWave.setFrequency;
var
  temp: Single;
begin
  temp := FRate;
  FRate := frequency * FSampleRateInv;
  temp := temp - FRate;
  if (temp < 0) then
    temp := -temp;
  FPitchEnvelope.setTarget(FRate);
  FPitchEnvelope.SetRate(FSweepRate * temp);
end;

procedure TStkSingWave.SetVibratoRate;
begin
  FModulator.SetVibratoRate(aRate);
end;

procedure TStkSingWave.SetVibratoGain;
begin
  FModulator.SetVibratoGain(gain);
end;

procedure TStkSingWave.SetRandomGain;
begin
  FModulator.SetRandomGain(gain);
end;

procedure TStkSingWave.setSweepRate;
begin
  FSweepRate := aRate;
end;

procedure TStkSingWave.SetGainRate;
begin
  FEnvelope.SetRate(aRate);
end;

procedure TStkSingWave.SetGainTarget;
begin
  FEnvelope.setTarget(Target);
end;

procedure TStkSingWave.NoteOn;
begin
  FEnvelope.keyOn;
end;

procedure TStkSingWave.NoteOff;
begin
  FEnvelope.keyOff;
end;

function TStkSingWave.Tick: Single;
var
  newrate: Single;
begin
  // Set the FWave FRate.
  newRate := FPitchEnvelope.Tick;
  newRate := newrate + newRate * FModulator.Tick;
  FWave.setfrequency(newRate);
  FLastOutput := FWave.Tick;
  FLastOutput := FLastOutput * FEnvelope.Tick;
  Result := FLastOutput;
end;

function TStkSingWave.LastOut: Single;
begin
  Result := FLastOutput;
end;

end.
