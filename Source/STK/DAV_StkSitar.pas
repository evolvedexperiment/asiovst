unit DAV_StkSitar;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkSitar string model class.

  This class implements a TStkSitar plucked string physical model based on the
  Karplus-Strong algorithm.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others. There exist at least two
  patents, assigned to Stanford, bearing the names of Karplus and/or Strong.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkDelaya, DAV_StkOneZero, DAV_StkNoise,
  DAV_StkAdsr;

type
  TStkSitar = class(TStkInstrument)
  protected
    FDelayLine   : TDelayA;
    FLoopFilter  : TOneZero;
    FNoise       : TNoise;
    FLength      : Longint;
    FAmGain      : Single;
    FDelay       : Single;
    FTargetDelay : Single;
    FLoopGain    : Single;
  public
    Envelope     : TADSR;

    // Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: Single);

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

  end;

implementation

constructor TStkSitar.Create;
begin
  inherited Create(sr);
  FLength := round(srate / lowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TDelayA.Create(srate, (FLength / 2.0), FLength);
  FDelay := FLength / 2.0;
  FTargetDelay := FDelay;

  FLoopFilter := TOneZero.Create(srate);
  FLoopFilter.setZero(0.01);

  envelope := TADSR.Create(srate);
  envelope.setAllTimes(0.001, 0.04, 0.0, 0.5);
  FNoise := TNoise.Create(srate);
  Clear;
end;

destructor TStkSitar.Destroy;
begin
  inherited Destroy;
  FDelayLine.Free;
  FLoopFilter.Free;
  Envelope.Free;
  FNoise.Free;
end;

procedure TStkSitar.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
end;

procedure TStkSitar.setFrequency;
var
  freakency: Single;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  FTargetDelay := (srate / freakency);
  FDelay := FTargetDelay * (1.0 + (0.05 * FNoise.tick()));
  FDelayLine.setDelay(FDelay);
  FLoopGain := 0.995 + (freakency * 0.0000005);
  if (FLoopGain > 0.9995) then
    FLoopGain := 0.9995;
end;

procedure TStkSitar.pluck;
begin
  envelope.keyOn;
end;

procedure TStkSitar.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
  FAmGain := 0.1 * amplitude;
end;

procedure TStkSitar.noteOff;
begin
  FLoopGain := 1.0 - amplitude;
  if (FLoopGain < 0.0) then
    FLoopGain := 0.0
  else if (FLoopGain > 1.0) then
    FLoopGain := 0.99999;
end;

function TStkSitar.tick: Single;
begin
  if (abs(FTargetDelay - FDelay) > 0.001) then
   begin
    if (FTargetDelay < FDelay) then
      FDelay := FDelay * 0.99999
    else
      FDelay := FDelay * 1.00001;
    FDelayLine.setDelay(FDelay);
   end;
  lastOutput := FDelayLine.tick(FLoopFilter.tick(FDelayLine.lastOut *
    FLoopGain) + (FAmGain * envelope.tick * FNoise.tick));
  Result := lastOutput;
end;

end.
