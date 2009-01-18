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
  TStkDrone = class(TInstrmnt)
  protected
    FDelayLine: TDelayA;
    FLoopFilter: TOneZero;
    FEnvelope: TAdsr;
    FNoise: TNoise;
    FLength: Integer;
    FLoopGain: Single;
  public
  //! Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate, LowestFrequency: Single);

    destructor Destroy; override;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

  //! Pluck the string with the given Amplitude using the current Frequency.
    procedure pluck(Amplitude: Single);

  //! Start a note with the given Frequency and Amplitude.
    procedure noteOn(Frequency, Amplitude: Single);

  //! Stop a note with the given Amplitude (speed of decay).
    procedure noteOff(Amplitude: Single);

  //! Compute one output sample.
    function Tick: Single;
  end;

implementation

constructor TStkDrone.Create(SampleRate: Single, LowestFrequency: Single);
begin
  inherited Create(SampleRate);
  FLength := round(FSampleRate / LowestFrequency + 1);
  FLoopGain := 0.999;
  FDelayLine := TDelayA.Create(FSampleRate, (FLength / 2.0), FLength);
  FLoopFilter := TOneZero.Create(FSampleRate);
  FNoise := TNoise.Create(FSampleRate);
  FEnvelope := TAdsr.Create(FSampleRate);
  FEnvelope.setAllTimes(2.0, 0.5, 0.0, 0.5);
  Clear;
end;

destructor TStkDrone.Destroy;
begin
  FDelayLine.Free;
  FLoopFilter.Free;
  FEnvelope.Free;
  FNoise.Free;
end;

procedure TStkDrone.Clear;
begin
  FDelayLine.Clear;
  FLoopFilter.Clear;
end;

procedure TStkDrone.setFrequency;
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

procedure TStkDrone.pluck;
begin
  FEnvelope.keyOn;
end;

procedure TStkDrone.noteOn;
begin
  setFrequency(Frequency);
  pluck(Amplitude);
end;

procedure TStkDrone.noteOff;
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
  lastOutput := FDelayLine.Tick(FLoopFilter.Tick(FDelayLine.lastOut * FLoopGain) +
    (0.005 * FEnvelope.Tick * FNoise.Tick));
  Result := lastOutput;
end;

end.
