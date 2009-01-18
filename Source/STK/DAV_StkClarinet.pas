unit DAV_Clarinet;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.


{ STK TStkClarinet physical model class.
  -----------------------------------

  This class implements a simple clarinet physical model, as discussed by
  Smith (1986), McIntyre, Schumacher, Woodhouse (1983), and others.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Reed Stiffness := 2
    - FNoise Gain := 4
    - FVibrato Frequency := 11
    - FVibrato Gain := 1
    - Breath Pressure := 128
}

interface

{$Integer ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_Instrmnt, DAV_Delayl, DAV_Reedtabl, DAV_Onezero, DAV_Envelope,
  DAV_Noise, DAV_LFO;

type
  TStkClarinet = class(TInstrmnt)
  protected
    FDelayLine   : TDelayl;
    FReedTable   : TReedTable;
    FFilter      : TOneZero;
    FEnvelope    : TEnvelope;
    FNoise       : TNoise;
    FVibrato     : TLfo;
    FLength      : Integer;
    FOutputGain  : Single;
    FNoiseGain   : Single;
    FVibratoGain : Single;
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: Single);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

  //! Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure startBlowing(amplitude, rate: Single);

  //! Decrease breath pressure with given rate of decrease.
    procedure stopBlowing(rate: Single);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: Single);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

  //! Compute one output sample.
    function tick: Single;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: Single);
  end;

implementation

constructor TStkClarinet.Create;
begin
  inherited Create(sr);
  FLength := round(srate / lowestFrequency + 1);
  FDelayLine := TDelayl.Create(srate, (FLength / 2.0), FLength);
  FReedTable := TReedTable.Create(srate);
  FReedTable.setOffset(0.7);
  FReedTable.setSlope(-0.3);
  FFilter := TOneZero.Create(srate);
  FEnvelope := TEnvelope.Create(srate);
  FNoise := TNoise.Create(srate);
  FVibrato := TLfo.Create(srate);
  FVibrato.setFrequency(5.735);
  FOutputGain := 1.0;
  FNoiseGain := 0.2;
  FVibratoGain := 0.1;
end;

destructor TStkClarinet.Destroy;
begin
  inherited Destroy;
  FDelayLine.Free;
  FReedTable.Free;
  FFilter.Free;
  FEnvelope.Free;
  FNoise.Free;
  FVibrato.Free;
end;

procedure TStkClarinet.Clear;
begin
  FDelayLine.Clear;
  FFilter.tick(0.0);
end;

procedure TStkClarinet.setFrequency;
var
  delay, freakency: Single;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := FLength - approximate FFilter delay.
  delay := (srate / freakency) * 0.5 - 1.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;
  FDelayLine.setDelay(delay);
end;

procedure TStkClarinet.startBlowing;
begin
  FEnvelope.setRate(rate);
  FEnvelope.setTarget(amplitude);
end;

procedure TStkClarinet.stopBlowing;
begin
  FEnvelope.setRate(rate);
  FEnvelope.setTarget(0.0);
end;

procedure TStkClarinet.noteOn(frequency, amplitude: Single);
begin
  setFrequency(frequency);
  startBlowing(0.55 + (amplitude * 0.30), amplitude * 0.005);
  FOutputGain := amplitude + 0.001;
end;

procedure TStkClarinet.noteOff;
begin
  stopBlowing(amplitude * 0.01);
end;

function TStkClarinet.tick: Single;
var
  breathPressure, pressureDiff: Single;
begin
  // Calculate the breath pressure (FEnvelope + FNoise + FVibrato)
  breathPressure := FEnvelope.tick;
  breathPressure := breathPressure + breathPressure * FNoiseGain * FNoise.tick;
  breathPressure := breathPressure + breathPressure * FVibratoGain *
    FVibrato.tick;

  // Perform commuted loss filtering.
  pressureDiff := -0.95 * FFilter.tick(FDelayLine.lastOut);

  // Calculate pressure difference of reflected and mouthpiece pressures.
  pressureDiff := pressureDiff - breathPressure;

  // Perform non-linear scattering using pressure difference in reed function.
  lastOutput := FDelayLine.tick(breathPressure + pressureDiff *
    FReedTable.tick(pressureDiff));

  // Apply output gain.
  lastOutput := lastOutput * FOutputGain;

  Result := lastOutput;
end;

procedure TStkClarinet.controlChange(number: integer; Value: Single);
var
  norm: Single;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_ReedStiffness_) then // 2
    FReedTable.setSlope(-0.44 + (0.26 * norm))
  else if (number = __SK_NoiseLevel_) then // 4
    FNoiseGain := (norm * 0.4)
  else if (number = __SK_ModFrequency_) then // 11
    FVibrato.setFrequency((norm * 12.0))
  else if (number = __SK_ModWheel_) then // 1
    FVibratoGain := (norm * 0.5)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FEnvelope.setValue(norm);
end;

end.
