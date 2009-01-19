unit DAV_StkBlowBotl;

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
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkJettabl, DAV_StkBiquad,
  DAV_StkPolezero, DAV_StkNoise, DAV_StkAdsr, DAV_StkLfo;

type
  TBlowBotl = class(TInstrmnt)
  protected
    FJetTable    : TJetTable;
    FResonator   : TBiquad;
    FDCBlock     : TPoleZero;
    FNoise       : TNoise;
    FAdsr        : TAdsr;
    FVibrato     : TLfo;
    FMaxPressure : Single;
    FNoiseGain   : Single;
    FVibratoGain : Single;
    FOutputGain  : Single;
  public
    // Class constructor.
    constructor Create(sr: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular frequency.
    procedure SetFrequency(frequency: Single);

    // Apply breath velocity to instrument with given Amplitude and rate of increase.
    procedure StartBlowing(Amplitude, rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure StopBlowing(rate: Single);

    // Start a note with the given frequency and Amplitude.
    procedure noteOn(frequency, Amplitude: Single);

    // Stop a note with the given Amplitude (speed of decay).
    procedure noteOff(Amplitude: Single);

    // Compute one output sample.
    function tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure ControlChange(number: Integer; Value: Single);

  end;

implementation

const
  CBottleRadius = 0.999;

constructor TBlowBotl.Create;
begin
  inherited Create(sr);
  FJetTable := TJetTable.Create(Samplerate);
  FDCBlock := TPoleZero.Create(Samplerate);
  FDCBlock.SetBlockZero;
  FVibrato := TLfo.Create(Samplerate);
  FVibrato.SetFrequency(5.925);
  FVibratoGain := 0.0;
  FResonator := TBiquad.Create(Samplerate);
  FResonator.SetResonance(500.0, CBottleRadius, True);
  FAdsr := TAdsr.Create(Samplerate);
  FAdsr.SetAllTimes(0.005, 0.01, 0.8, 0.010);
  FNoise := TNoise.Create(Samplerate);
  FNoiseGain := 20.0;
  FMaxPressure := 0.0;
end;

destructor TBlowBotl.Destroy;
begin
  inherited Destroy;
  FJetTable.Free;
  FResonator.Free;
  FDCBlock.Free;
  FNoise.Free;
  FAdsr.Free;
  FVibrato.Free;
end;

procedure TBlowBotl.Clear;
begin
  FResonator.Clear;
end;

procedure TBlowBotl.SetFrequency;
var
  Freakency: Single;
begin
  Freakency := frequency;
  if (frequency <= 0.0) then
    Freakency := 220.0;
  FResonator.SetResonance(Freakency, CBottleRadius, True);
end;

procedure TBlowBotl.StartBlowing;
begin
  FAdsr.SetAttackRate(rate);
  FMaxPressure := Amplitude;
  FAdsr.keyOn();
end;

procedure TBlowBotl.StopBlowing;
begin
  FAdsr.setReleaseRate(rate);
  FAdsr.keyOff();
end;

procedure TBlowBotl.noteOn;
begin
  SetFrequency(frequency);
  StartBlowing(1.1 + (Amplitude * 0.20), Amplitude * 0.02);
  FOutputGain := Amplitude + 0.001;
end;

procedure TBlowBotl.noteOff;
begin
  StopBlowing(Amplitude * 0.02);
end;

function TBlowBotl.tick: Single;
var
  breathPressure, randPressure, pressureDiff: Single;
begin
  // Calculate the breath pressure (envelope + FVibrato)
  breathPressure := FMaxPressure * FAdsr.tick();
  breathPressure := breathPressure + FVibratoGain * FVibrato.tick();

  pressureDiff := breathPressure - FResonator.lastOut();

  randPressure := FNoiseGain * FNoise.tick();
  randPressure := randPressure * breathPressure;
  randPressure := randpressure * (1.0 + pressureDiff);

  FResonator.tick(breathPressure + randPressure -
    (FJetTable.tick(pressureDiff) * pressureDiff));
  lastOutput := 0.2 * FOutputGain * FDCBlock.tick(pressureDiff);

  Result := lastOutput;
end;

procedure TBlowBotl.ControlChange;
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_NoiseLevel_) then // 4
    FNoiseGain := norm * 30.0
  else if (number = __SK_ModFrequency_) then // 11
    FVibrato.SetFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    FVibratoGain := norm * 0.4
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FAdsr.setTarget(norm);
end;

end.
