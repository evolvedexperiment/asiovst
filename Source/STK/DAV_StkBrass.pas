unit DAV_StkBrass;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK simple TStkBrass instrument class.

  This class implements a simple Brass instrument waveguide model, a la Cook
  (TBone, HosePlayer).

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Lip Tension := 2
    - Slide Length := 4
    - Vibrato Frequency := 11
    - Vibrato Gain := 1
    - Volume := 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkDelaya, DAV_StkBiquad,
  DAV_StkPolezero, DAV_StkAdsr, DAV_StkLfo, Math;

type
  TStkBrass = class(TInstrmnt)
  protected
    DelayLine   : TDelayA;
    LipFilter   : TBiQuad;
    DcBlock     : TPoleZero;
    Adsr        : TADSR;
    Vibrato     : TLFO;
    Length      : Integer;
    LipTarget   : Single;
    SlideTarget : Single;
    VibratoGain : Single;
    MaxPressure : Single;
  public
  //! Class constructor, taking the lowest desired playing Frequency.
    constructor Create(SampleRate, LowestFrequency: Single);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular Frequency.
    procedure setFrequency(Frequency: Single);

  //! Set the lips Frequency.
    procedure setLip(Frequency: Single);

  //! Apply breath pressure to instrument with given Amplitude and Rate of increase.
    procedure startBlowing(Amplitude, Rate: Single);

  //! Decrease breath pressure with given Rate of decrease.
    procedure stopBlowing(Rate: Single);

  //! Start a note with the given Frequency and Amplitude.
    procedure noteOn(Frequency, Amplitude: Single);

  //! Stop a note with the given Amplitude (speed of decay).
    procedure noteOff(Amplitude: Single);

  //! Compute one output sample.
    function tick: Single;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: Single);

  end;

implementation

constructor TStkBrass.Create;
begin
  inherited Create(SampleRate);
  Length := round(SampleRate / LowestFrequency + 1);
  DelayLine := TDelayA.Create(SampleRate, 0.5 * Length, Length);

  LipFilter := TBiQuad.Create(SampleRate);
  LipFilter.SetGain(0.03);
  DcBlock := TPoleZero.Create(SampleRate);
  DcBlock.setBlockZero(0.99);

  Adsr := TADSR.Create(SampleRate);
  Adsr.setAllTimes(0.005, 0.001, 1.0, 0.010);

  Vibrato := TLFO.Create(SampleRate);
  Vibrato.setFrequency(6.137);
  VibratoGain := 0.0;

  Clear;
  MaxPressure := 0.0;
  LipTarget := 0.0;

  // Necessary to initialize variables.
  setFrequency(220.0);
end;

destructor TStkBrass.Destroy;
begin
  inherited Destroy;
  DelayLine.Free;
  LipFilter.Free;
  DcBlock.Free;
  Adsr.Free;
  Vibrato.Free;
end;

procedure TStkBrass.Clear;
begin
  DelayLine.Clear;
  LipFilter.Clear;
  DcBlock.Clear;
end;

procedure TStkBrass.setFrequency;
var
  Freakency: Single;
begin
  Freakency := Frequency;
  if (Frequency <= 0.0) then
    Freakency := 220.0;

  // Fudge correction for filter delays.
  SlideTarget := (SampleRate / Freakency * 2.0) + 3.0;
  DelayLine.setDelay(SlideTarget); // play a harmonic

  LipTarget := Freakency;
  LipFilter.setResonance(Freakency, 0.997, False);
end;

procedure TStkBrass.setLip;
var
  Freakency: Single;
begin
  Freakency := Frequency;
  if (Frequency <= 0.0) then
    Freakency := 220.0;

  LipFilter.setResonance(Freakency, 0.997, False);
end;

procedure TStkBrass.startBlowing;
begin
  Adsr.setAttackRate(Rate);
  MaxPressure := Amplitude;
  Adsr.keyOn;
end;

procedure TStkBrass.stopBlowing;
begin
  Adsr.setReleaseRate(Rate);
  Adsr.keyOff;
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
  breathPressure := MaxPressure * Adsr.tick;
  breathPressure := breathPressure + VibratoGain * Vibrato.tick;

  mouthPressure := 0.3 * breathPressure;
  borePressure := 0.85 * DelayLine.lastOut;
  deltaPressure := mouthPressure - borePressure; // Differential pressure.
  deltaPressure := LipFilter.tick(deltaPressure);      // Force - > position.
  deltaPressure := deltaPressure * deltaPressure;
          // Basic position to area mapping.
  if (deltaPressure > 1.0) then
    deltaPressure := 1.0;         // Non-linear saturation.
  // The following input scattering assumes the mouthPressure := area.
  lastOutput := deltaPressure * mouthPressure + (1.0 - deltaPressure) *
    borePressure;
  lastOutput := DelayLine.tick(DcBlock.tick(lastOutput));

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
    temp := LipTarget * power(4.0, (2.0 * norm) - 1.0);
    setLip(temp);
   end
  else if (number = __SK_SlideLength_) then // 4
    DelayLine.setDelay(SlideTarget * (0.5 + norm))
  else if (number = __SK_ModFrequency_) then // 11
    Vibrato.setFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    VibratoGain := norm * 0.4
  else if (number = __SK_AfterTouch_Cont_) then // 128
    Adsr.setTarget(norm);
end;

end.
