unit DAV_StkPlucked;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TPlucked string model class.

  This class implements a simple TPlucked string physical model based on the
  Karplus-Strong algorithm.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others. There exist at least two
  patents, assigned to Stanford, bearing the names of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrmnt, DAV_StkDelaya, DAV_StkOneZero, DAV_StkOnePole,
  DAV_StkNoise;

type
  TStkPlucked = class(TStkInstrument)
  public
    // Class constructor, taking the lowest desired playing frequency.
    constructor Create(const SampleRate, LowestFrequency: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

    // Pluck the string with the given amplitude using the current frequency.
    procedure pluck(amplitude: Single);

    // Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

    // Compute one output sample.
    function tick: Single;

  protected
    FDelayLine: TDelayA;
    FLoopFilter: TOneZero;
    FPpickFilter: TOnePole;
    FNoise: TNoise;
    FLength: Integer;
    FLoopGain: Single;
  end;

implementation

constructor TPlucked.Create;
begin
  inherited Create(SampleRate);
  FLength := round(srate / LowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TDelayA.Create(SampleRate, (FLength / 2.0), FLength);
  FLoopFilter := TOneZero.Create(SampleRate);
  FPpickFilter := TOnePole.Create(SampleRate);
  FNoise := TNoise.Create(SampleRate);
  Clear;
end;

destructor TPlucked.Destroy;
begin
  inherited Destroy;
  FDelayLine.Free;
  FLoopFilter.Free;
  FPpickFilter.Free;
  FNoise.Free;
end;

procedure TPlucked.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
  FPpickFilter.Clear;
end;

procedure TPlucked.setFrequency;
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

procedure TPlucked.pluck;
var
  gain: Single;
  i: Integer;
begin
  gain := amplitude;
  if (gain > 1.0) then
    gain := 1.0
  else if (gain < 0.0) then
    gain := 0.0;

  FPpickFilter.setPole(0.999 - (gain * 0.15));
  FPpickFilter.setGain(gain * 0.5);
  for i := 0 to FLength - 1 do

  // Fill delay with FNoise additively with current contents.
    FDelayLine.tick(0.6 * FDelayLine.lastOut + FPpickFilter.tick(FNoise.tick));
end;

procedure TPlucked.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
end;

procedure TPlucked.noteOff;
begin
  FLoopGain := 1.0 - amplitude;
  if (FLoopGain < 0.0) then
    FLoopGain := 0.0
  else if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

function TPlucked.tick: Single;
begin
  // Here's the whole inner loop of the instrument!!
  lastOutput := FDelayLine.tick(FLoopFilter.tick(
    FDelayLine.lastOut * FLoopGain));
  lastOutput := lastoutput * 3;
  Result := lastOutput;
end;

end.
