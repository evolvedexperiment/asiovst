unit DAV_StkBlowBottle;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK blown bottle instrument class.

  This class implements a helmholtz FResonator (Biquad Filter) with a
  polynomial jet excitation (a la Cook).

  Control Change Numbers:
    - FNoise Gain := 4
    - FVibrato Frequency := 11
    - FVibrato Gain := 1
    - Volume := 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkInstrument, DAV_StkJetTable, DAV_StkNoise,
  DAV_StkBiquad, DAV_StkPolezero, DAV_StkAdsr, DAV_StkLfo;

type
  TStkBlowBottle = class(TStkControlableInstrument)
  protected
    FJetTable    : TStkJetTable;
    FResonator   : TStkBiquad;
    FDCBlock     : TStkPoleZero;
    FNoise       : TStkNoise;
    FAdsr        : TStkAdsr;
    FVibrato     : TStkLfo;
    FMaxPressure : Single;
    FNoiseGain   : Single;
    FVibratoGain : Single;
    FOutputGain  : Single;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(const Frequency: Single); override;

  public
    // Class constructor.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Apply breath velocity to instrument with given Amplitude and rate of increase.
    procedure StartBlowing(const Amplitude, Rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure StopBlowing(const Rate: Single);

    // Start a note with the given frequency and Amplitude.
    procedure NoteOn(const Frequency, Amplitude: Single); override;

    // Stop a note with the given Amplitude (speed of decay).
    procedure NoteOff(const Amplitude: Single); override;

    // Compute one output sample.
    function Tick: Single; override;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(const Number: Integer; const Value: Single); override;
  end;

implementation

uses
  SysUtils, DAV_StkFilter;

const
  CBottleRadius = 0.999;

constructor TStkBlowBottle.Create(const SampleRate: Single);
begin
  inherited Create(SampleRate);
  FJetTable := TStkJetTable.Create(Samplerate);
  FDCBlock := TStkPoleZero.Create(Samplerate);
  FDCBlock.SetBlockZero;
  FVibrato := TStkLfo.Create(Samplerate);
  FVibrato.Frequency := 5.925;
  FVibratoGain := 0.0;
  FResonator := TStkBiquad.Create(Samplerate);
  FResonator.SetResonance(500.0, CBottleRadius, True);
  FAdsr := TStkAdsr.Create(Samplerate);
  FAdsr.SetAllTimes(0.005, 0.01, 0.8, 0.010);
  FNoise := TStkNoise.Create(Samplerate);
  FNoiseGain := 20.0;
  FMaxPressure := 0.0;
end;

destructor TStkBlowBottle.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FJetTable);
  FreeAndNil(FResonator);
  FreeAndNil(FDCBlock);
  FreeAndNil(FNoise);
  FreeAndNil(FAdsr);
  FreeAndNil(FVibrato);
end;

procedure TStkBlowBottle.Clear;
begin
  FResonator.Clear;
end;

procedure TStkBlowBottle.SetFrequency(const Frequency: Single);
var
  Freakency: Single;
begin
  Freakency := frequency;
  if (frequency <= 0.0) then Freakency := 220.0;
  FResonator.SetResonance(Freakency, CBottleRadius, True);
end;

procedure TStkBlowBottle.StartBlowing(const Amplitude, Rate: Single);
begin
  FAdsr.AttackRate := rate;
  FMaxPressure := Amplitude;
  FAdsr.KeyOn;
end;

procedure TStkBlowBottle.StopBlowing(const Rate: Single);
begin
  FAdsr.ReleaseRate := rate;
  FAdsr.KeyOff;
end;

procedure TStkBlowBottle.NoteOn(const Frequency, Amplitude: Single);
begin
  SetFrequency(Frequency);
  StartBlowing(1.1 + (Amplitude * 0.20), Amplitude * 0.02);
  FOutputGain := Amplitude + 0.001;
end;

procedure TStkBlowBottle.NoteOff(const Amplitude: Single);
begin
  StopBlowing(Amplitude * 0.02);
end;

function TStkBlowBottle.Tick: Single;
var
  breathPressure, randPressure, pressureDiff: Single;
begin
  // Calculate the breath pressure (envelope + FVibrato)
  breathPressure := FMaxPressure * FAdsr.tick;
  breathPressure := breathPressure + FVibratoGain * FVibrato.tick;

  pressureDiff := breathPressure - FResonator.LastOutput;

  randPressure := FNoiseGain * FNoise.tick;
  randPressure := randPressure * breathPressure;
  randPressure := randpressure * (1.0 + pressureDiff);

  FResonator.tick(breathPressure + randPressure -
    (FJetTable.tick(pressureDiff) * pressureDiff));
  FLastOutput := 0.2 * FOutputGain * FDCBlock.tick(pressureDiff);

  Result := lastOutput;
end;

procedure TStkBlowBottle.ControlChange;
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = CMidiNoiseLevel) then // 4
    FNoiseGain := norm * 30.0
  else if (number = CMidiModFrequency) then // 11
    FVibrato.Frequency := norm * 12.0
  else if (number = CMidiModWheel) then // 1
    FVibratoGain := norm * 0.4
  else if (number = CMidiAfterTouchCont) then // 128
    FAdsr.Target := norm;
end;

end.
