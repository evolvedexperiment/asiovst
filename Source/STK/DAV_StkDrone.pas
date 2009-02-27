unit DAV_StkDrone;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK "drone" plucked string model.

   This class implements a simple plucked string physical model based on the
   Karplus-Strong algorithm.

   This is a digital waveguide model, making its use possibly subject to
   patents held by Stanford University, Yamaha, and others.
   There exist at least two patents, assigned to Stanford, bearing the names
   of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkDelaya, DAV_StkOneZero, DAV_StkAdsr,
  DAV_StkNoise;

type
  TStkDrone = class(TStkInstrument)
  protected
    FDelayLine  : TStkDelayA;
    FLoopFilter : TStkOneZero;
    FEnvelope   : TStkAdsr;
    FNoise      : TStkNoise;
    FLength     : Integer;
    FLoopGain   : Single;

    // Set instrument parameters for a particular Frequency.
    procedure SetFrequency(const Frequency: Single); override;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Pluck the string with the given Amplitude using the current Frequency.
    procedure Pluck(Amplitude: Single);

    // Start a note with the given Frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkDrone.Create(const SampleRate, LowestFrequency: Single);
begin
  inherited Create(SampleRate);
  FLength := round(FSampleRate / LowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TStkDelayA.Create(FSampleRate, (FLength / 2.0), FLength);
  FLoopFilter := TStkOneZero.Create(FSampleRate);
  FNoise := TStkNoise.Create(FSampleRate);
  FEnvelope := TStkAdsr.Create(FSampleRate);
  FEnvelope.setAllTimes(2.0, 0.5, 0.0, 0.5);
  Clear;
end;

destructor TStkDrone.Destroy;
begin
 FreeAndNil(FDelayLine);
 FreeAndNil(FLoopFilter);
 FreeAndNil(FEnvelope);
 FreeAndNil(FNoise);
 inherited;
end;

procedure TStkDrone.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
end;

procedure TStkDrone.SetFrequency;
var
  delay, freakency: Single;
begin
  freakency := Frequency;
  if (Frequency <= 0.0) then
    freakency := 220.0;
 // Delay=FLength - approximate filter delay.
  delay := (FSampleRate / freakency) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;
  FDelayLine.setDelay(delay);
  FLoopGain := 0.997 + (freakency * 0.000002);
  if (FLoopGain >= 1.0) then
    FLoopGain := 0.99999;
end;

procedure TStkDrone.Pluck;
begin
  FEnvelope.keyOn;
end;

procedure TStkDrone.NoteOn;
begin
  SetFrequency(Frequency);
  Pluck(Amplitude);
end;

procedure TStkDrone.NoteOff;
begin
  FLoopGain := 1.0 - Amplitude;
  if (FLoopGain < 0.0) then
    FLoopGain := 0.0
  else if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

function TStkDrone.Tick: Single;
begin
 // Here's the whole inner loop of the instrument!!
  FLastOutput := FDelayLine.Tick(FLoopFilter.Tick(FDelayLine.LastOutput * FLoopGain) +
    (0.005 * FEnvelope.Tick * FNoise.Tick));
  Result := FLastOutput;
end;

end.
