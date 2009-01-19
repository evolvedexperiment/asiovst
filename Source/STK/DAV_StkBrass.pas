unit DAV_StkBrass;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK simple TStkBrass instrument class.

  This class implements a simple Brass instrument waveguide model, a la Cook
  (TBone, HosePlayer).

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Lip Tension = 2
    - Slide FLength = 4
    - FVibrato Frequency = 11
    - FVibrato Gain = 1
    - Volume = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkDelaya, DAV_StkBiquad,
  DAV_StkPolezero, DAV_StkAdsr, DAV_StkLfo, Math;

type
  TStkBrass = class(TInstrmnt)
  protected
    FDelayLine   : TDelayA;
    FLipFilter   : TBiQuad;
    FDcBlock     : TPoleZero;
    FAdsr        : TADSR;
    FVibrato     : TLFO;
    FLength      : Integer;
    FLipTarget   : Single;
    FSlideTarget : Single;
    FVibratoGain : Single;
    FMaxPressure : Single;
  public
    // Class constructor, taking the lowest desired playing Frequency.
    constructor Create(const SampleRate, LowestFrequency: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

    // Set the lips Frequency.
    procedure setLip(Frequency: Single);

    // Apply breath pressure to instrument with given Amplitude and Rate of increase.
    procedure startBlowing(Amplitude, Rate: Single);

    // Decrease breath pressure with given Rate of decrease.
    procedure stopBlowing(Rate: Single);

    // Start a note with the given Frequency and Amplitude.
    procedure noteOn(Frequency, Amplitude: Single);

    // Stop a note with the given Amplitude (speed of decay).
    procedure noteOff(Amplitude: Single);

    // Compute one output sample.
    function tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: Integer; Value: Single);

  end;

implementation

constructor TStkBrass.Create;
begin
  inherited Create(SampleRate);
  FLength := round(SampleRate / LowestFrequency + 1);
  FDelayLine := TDelayA.Create(SampleRate, 0.5 * FLength, FLength);

  FLipFilter := TBiQuad.Create(SampleRate);
  FLipFilter.SetGain(0.03);
  FDcBlock := TPoleZero.Create(SampleRate);
  FDcBlock.setBlockZero(0.99);

  FAdsr := TADSR.Create(SampleRate);
  FAdsr.setAllTimes(0.005, 0.001, 1.0, 0.010);

  FVibrato := TLFO.Create(SampleRate);
  FVibrato.setFrequency(6.137);
  FVibratoGain := 0.0;

  Clear;
  FMaxPressure := 0.0;
  FLipTarget := 0.0;

  // Necessary to initialize variables.
  setFrequency(220.0);
end;

destructor TStkBrass.Destroy;
begin
  inherited Destroy;
  FDelayLine.Free;
  FLipFilter.Free;
  FDcBlock.Free;
  FAdsr.Free;
  FVibrato.Free;
end;

procedure TStkBrass.Clear;
begin
  FDelayLine.Clear;
  FLipFilter.Clear;
  FDcBlock.Clear;
end;

procedure TStkBrass.setFrequency;
var
  Freakency: Single;
begin
  Freakency := Frequency;
  if (Frequency <= 0.0) then
    Freakency := 220.0;

  // Fudge correction for filter delays.
  FSlideTarget := (SampleRate / Freakency * 2.0) + 3.0;
  FDelayLine.setDelay(FSlideTarget); // play a harmonic

  FLipTarget := Freakency;
  FLipFilter.setResonance(Freakency, 0.997, False);
end;

procedure TStkBrass.setLip;
var
  Freakency: Single;
begin
  Freakency := Frequency;
  if (Frequency <= 0.0) then
    Freakency := 220.0;

  FLipFilter.setResonance(Freakency, 0.997, False);
end;

procedure TStkBrass.startBlowing;
begin
  FAdsr.setAttackRate(Rate);
  FMaxPressure := Amplitude;
  FAdsr.keyOn;
end;

procedure TStkBrass.stopBlowing;
begin
  FAdsr.setReleaseRate(Rate);
  FAdsr.keyOff;
end;

procedure TStkBrass.noteOn;
begin
  setFrequency(Frequency);
  startBlowing(Amplitude, Amplitude * 0.001);
end;

procedure TStkBrass.noteOff;
begin
  stopBlowing(Amplitude * 0.005);
end;

function TStkBrass.tick: Single;
var
  deltaPressure, borePressure, mouthPressure, breathPressure: Single;
begin
  breathPressure := FMaxPressure * FAdsr.tick;
  breathPressure := breathPressure + FVibratoGain * FVibrato.tick;

  mouthPressure := 0.3 * breathPressure;
  borePressure := 0.85 * FDelayLine.lastOut;
  deltaPressure := mouthPressure - borePressure; // Differential pressure.
  deltaPressure := FLipFilter.tick(deltaPressure);      // Force - > position.
  deltaPressure := deltaPressure * deltaPressure;
          // Basic position to area mapping.
  if (deltaPressure > 1.0) then
    deltaPressure := 1.0;         // Non-linear saturation.
  // The following input scattering assumes the mouthPressure := area.
  lastOutput := deltaPressure * mouthPressure + (1.0 - deltaPressure) *
    borePressure;
  lastOutput := FDelayLine.tick(FDcBlock.tick(lastOutput));

  Result := lastOutput;
end;

procedure TStkBrass.controlChange;
var
  temp, norm: Single;
begin
  norm := Value; // * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_LipTension_) then
   begin // 2
    temp := FLipTarget * power(4.0, (2.0 * norm) - 1.0);
    setLip(temp);
   end
  else if (number = __SK_SlideLength_) then // 4
    FDelayLine.setDelay(FSlideTarget * (0.5 + norm))
  else if (number = __SK_ModFrequency_) then // 11
    FVibrato.setFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    FVibratoGain := norm * 0.4
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FAdsr.setTarget(norm);
end;

end.
