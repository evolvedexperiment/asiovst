unit DAV_StkPlucked;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkPlucked string model class.

  This class implements a simple TStkPlucked string physical model based on the
  Karplus-Strong algorithm.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others. There exist at least two
  patents, assigned to Stanford, bearing the names of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkDelaya, DAV_StkOneZero,
  DAV_StkOnePole, DAV_StkNoise;

type
  TStkPlucked = class(TStkInstrument)
  protected
    FDelayLine   : TStkDelayA;
    FLoopFilter  : TStkOneZero;
    FPpickFilter : TStkOnePole;
    FNoise       : TStkNoise;
    FLength      : Integer;
    FLoopGain    : Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Frequency: Single); override;

  public
    // Class constructor, taking the lowest desired playing frequency.
    constructor Create(const SampleRate, LowestFrequency: Single); reintroduce; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Pluck the string with the given Amplitude using the current frequency.
    procedure Pluck(const Amplitude: Single);

    // Start a note with the given frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;
  end;

implementation

uses
  SysUtils;

constructor TStkPlucked.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TStkDelayA.Create(SampleRate, (FLength / 2.0), FLength);
  FLoopFilter := TStkOneZero.Create(SampleRate);
  FPpickFilter := TStkOnePole.Create(SampleRate);
  FNoise := TStkNoise.Create(SampleRate);
  Clear;
end;

destructor TStkPlucked.Destroy;
begin
 FreeAndNil(FDelayLine);
 FreeAndNil(FLoopFilter);
 FreeAndNil(FPpickFilter);
 FreeAndNil(FNoise);
 inherited Destroy;
end;

procedure TStkPlucked.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
  FPpickFilter.Clear;
end;

procedure TStkPlucked.SetFrequency;
var
  delay, freakency: Single;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := FLength - approximate filter delay.
  delay := (SampleRate / freakency) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;
  FDelayLine.setDelay(delay);
  FLoopGain := 0.995 + (freakency * 0.000005);
  if (FLoopGain >= 1.0) then
    FLoopGain := 0.99999;
end;

procedure TStkPlucked.Pluck;
var
  gain: Single;
  i: Integer;
begin
  gain := Amplitude;
  if (gain > 1.0) then
    gain := 1.0
  else if (gain < 0.0) then
    gain := 0.0;

  FPpickFilter.setPole(0.999 - (gain * 0.15));
  FPpickFilter.Gain := gain * 0.5;
  for i := 0 to FLength - 1 do

  // Fill delay with FNoise additively with current contents.
    FDelayLine.Tick(0.6 * FDelayLine.LastOutput + FPpickFilter.Tick(FNoise.Tick));
end;

procedure TStkPlucked.NoteOn;
begin
  SetFrequency(frequency);
  Pluck(Amplitude);
end;

procedure TStkPlucked.NoteOff;
begin
  FLoopGain := 1.0 - Amplitude;
  if (FLoopGain < 0.0) then
    FLoopGain := 0.0
  else if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

function TStkPlucked.Tick: Single;
begin
  // Here's the whole inner loop of the instrument!!
  FLastOutput := FDelayLine.Tick(FLoopFilter.Tick(
    FDelayLine.LastOutput * FLoopGain));
  FLastOutput := FLastOutput * 3;
  Result := FLastOutput;
end;

end.
