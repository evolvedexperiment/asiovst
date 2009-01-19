unit DAV_StkSampler;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK sampling synthesis abstract base class.

  This instrument contains up to 5 attack waves, 5 looped waves, and an FADSR
  envelope.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkWavePlayer, DAV_StkInstrument, DAV_StkAdsr, DAV_StkOnePole;

type
  TStkSampler = class(TStkInstrument)
  protected
    FADSR: TAdsr;
    FAttacks: array[0..4] of TWavePlayer;
    FLoops: array[0..4] of TWavePlayer;
    FFilter: TOnepole;
    FAttackGain, FLoopGain, FBaseFrequency: Single;
    FLoopratios, FAttackRatios: array[0..4] of Single;
    FWhichOne: Integer;
  public
    // Default constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

    // Initiate the envelopes with a key-on event and reset the attack waves.
    procedure KeyOn;

    // Signal a key-off event to the envelopes.
    procedure KeyOff;

    // Stop a note with the given amplitude (speed of decay).
    procedure NoteOff(amplitude: Single);

    // Compute one output sample.
    function Tick: Single;

    // Perform the control change specified by \e Number and \e value (0.0 - 128.0).
    procedure ControlChange(Number: Integer; Value: Single);
  end;

implementation


constructor TStkSampler.Create;
begin
  inherited Create(SampleRate);
  // We don't make the waves here yet, because
  // we don't know what they will be.
  FADSR := TAdsr.Create(srate);
  FBaseFrequency := 440.0;
  FFilter := TOnepole.Create(srate);
  FAttackGain := 0.25;
  FLoopGain := 0.25;
  FWhichOne := 0;
end;

destructor TStkSampler.Destroy;
begin
  inherited Destroy;
  FADSR.Free;
  FFilter.Free;
end;

procedure TStkSampler.KeyOn;
begin
  FADSR.KeyOn;
  FAttacks[0].reset;
end;

procedure TStkSampler.KeyOff;
begin
  FADSR.KeyOff;
end;

procedure TStkSampler.NoteOff;
begin
  KeyOff;
end;

function TStkSampler.Tick: Single;
begin
  lastOutput := FAttackGain * FAttacks[FWhichOne].Tick;
  lastOutput := lastoutput + FLoopGain * FLoops[FWhichOne].Tick;
  lastOutput := FFilter.Tick(lastOutput);
  lastOutput := lastoutput * FADSR.Tick;
  Result := lastOutput;
end;

procedure TStkSampler.Clear;
begin

end;

procedure TStkSampler.ControlChange(Number: Integer; Value: Single);
begin

end;

procedure TStkSampler.setFrequency(Frequency: Single);
begin

end;

end.
