unit DAV_StkBlowHole;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK clarinet physical model with one register hole and one FTonehole
  -------------------------------------------------------------------

  This class is based on the clarinet model, with the addition of a two-port
  register hole and a three-port dynamic FTonehole implementation, as discussed
  by Scavone and Cook (1998).

  In this implementation, the distances between the reed/register hole and
  FTonehole/bell are fixed.  As a result, both the FTonehole and register hole
  will have variable influence on the playing frequency, which is dependent on
  the FLength of the air column.  In addition, the highest playing freqeuency is
  limited by these fixed lengths.

  This is a digital waveguide model, making its use possibly subject to patents
  held by Stanford University, Yamaha, and others.

  Control Change Numbers:
    - Reed Stiffness = 2
    - FNoise Gain = 4
    - FTonehole State = 11
    - Register State = 1
    - Breath Pressure = 128
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkDelayl, DAV_StkReedTable, DAV_StkLfo,
  DAV_StkOneZero, DAV_StkPoleZero, DAV_StkEnvelope, DAV_StkNoise, Math;

type
  TBlowHole = class(TInstrmnt)
  protected
    FDelays       : array[0..2] of tdelayl;
    FReedTable    : TReedTable;
    FFilter       : TOneZero;
    FTonehole     : TPoleZero;
    FVent         : TPoleZero;
    FEnvelope     : TEnvelope;
    FNoise        : TNoise;
    FVibrato      : TLfo;
    FLength       : Integer;
    FScatter      : Single;
    FThCoeff      : Single;
    FRth          : Single;
    FRhCoeff      : Single;
    FRhGain       : Single;
    FOutputGain   : Single;
    FNoiseGain    : Single;
    FVibratoGain  : Single;
  public
  //! Class constructor.
    constructor Create(sr, lowestFrequency: Single);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

  //! Set the FTonehole state (0.0 := closed, 1.0 := fully open).
    procedure setTonehole(newValue: Single);

  //! Set the register hole state (0.0 := closed, 1.0 := fully open).
    procedure setVent(newValue: Single);

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

constructor TBlowHole.Create;
var
  xi, zeta, psi, r_rh, r_b: double;
  te: Single;
begin
  inherited Create(sr);
  FLength := round(srate / lowestFrequency + 1);
  // FDelays[0] is the delay line between the reed and the register FVent.
  FDelays[0] := TDelayL.Create(srate, 5.0 * srate / 22050.0, 100);
  // FDelays[1] is the delay line between the register FVent and the FTonehole.
  FDelays[1] := TDelayL.Create(srate, FLength shr 1, FLength);
  // FDelays[2] is the delay line between the FTonehole and the end of the bore.
  FDelays[2] := TDelayL.Create(srate, 4.0 * srate / 22050.0, 100);
  FReedTable := TReedTable.Create(srate);
  FReedTable.setOffset(0.7);
  FReedTable.setSlope(-0.3);
  FFilter := TOneZero.Create(srate);
  FEnvelope := TEnvelope.Create(srate);
  FNoise := TNoise.Create(srate);

  // Calculate the initial FTonehole three-port scattering coefficient
  r_b := 0.0075;    // main bore radius
  FRth := 0.003;          // FTonehole radius
  FScatter := -power(FRth, 2) / (power(FRth, 2) + 2 * power(r_b, 2));

  // Calculate FTonehole coefficients
  te := 1.4 * FRth;    // effective FLength of the open hole
  FThCoeff := (te * 2 * srate - 347.23) / (te * 2 * srate + 347.23);
  FTonehole := TPoleZero.Create(srate);
  // Start with FTonehole open
  FTonehole.setA1(-FThCoeff);
  FTonehole.setB0(FThCoeff);
  FTonehole.setB1(-1.0);

  // Calculate register hole FFilter coefficients
  r_rh := 0.0015;    // register FVent radius
  te := 1.4 * r_rh;       // effective FLength of the open hole
  xi := 0.0;         // series resistance term
  zeta := 347.23 + 2 * PI * power(r_b, 2) * xi / 1.1769;
  psi := 2 * PI * power(r_b, 2) * te / (PI * power(r_rh, 2));
  FRhCoeff := (zeta - 2 * srate * psi) / (zeta + 2 * srate * psi);
  FRhGain := -347.23 / (zeta + 2 * srate * psi);
  FVent := TPoleZero.Create(srate);
  FVent.setA1(FRhCoeff);
  FVent.setB0(1.0);
  FVent.setB1(1.0);
  // Start with register FVent closed
  FVent.setGain(0.0);
  FVibrato := TLfo.Create(srate);
  FVibrato.setFrequency(5.735);
  FOutputGain := 1.0;
  FNoiseGain := 0.2;
  FVibratoGain := 0.01;
end;

destructor TBlowHole.Destroy;
begin
  inherited Destroy;
  FDelays[0].Free;
  FDelays[1].Free;
  FDelays[2].Free;
  FReedTable.Free;
  FFilter.Free;
  FTonehole.Free;
  FVent.Free;
  FEnvelope.Free;
  FNoise.Free;
  FVibrato.Free;
end;

procedure TBlowHole.Clear;
begin
  FDelays[0].Clear();
  FDelays[1].Clear();
  FDelays[2].Clear();
  FFilter.tick(0.0);
  FTonehole.tick(0.0);
  FVent.tick(0.0);
end;

procedure TBlowHole.setFrequency;
var
  delay, freakency: Single;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := FLength - approximate FFilter delay.
  delay := (srate / freakency) * 0.5 - 3.5;
  delay := delay - (FDelays[0].getDelay() + FDelays[2].getDelay());

  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > FLength) then
    delay := FLength;
  FDelays[1].setDelay(delay);
end;

procedure TBlowHole.setVent;
var
  gain: Single;
begin
  // This method allows setting of the register FVent "open-ness" at
  // any point between "Open" (newValue := 1) and "Closed"
  // (newValue := 0).

  if (newValue <= 0.0) then
    gain := 0.0
  else if (newValue >= 1.0) then
    gain := FRhGain
  else
    gain := newValue * FRhGain;
  FVent.setGain(gain);
end;

procedure TBlowHole.setTonehole;
var
  new_coeff: Single;
begin
  // This method allows setting of the FTonehole "open-ness" at
  // any point between "Open" (newValue := 1) and "Closed"
  // (newValue := 0).
  if (newValue <= 0.0) then
    new_coeff := 0.9995
  else if (newValue >= 1.0) then
    new_coeff := FThCoeff
  else
    new_coeff := (newValue * (FThCoeff - 0.9995)) + 0.9995;
  FTonehole.setA1(-new_coeff);
  FTonehole.setB0(new_coeff);
end;

procedure TBlowHole.startBlowing;
begin
  FEnvelope.setRate(rate);
  FEnvelope.setTarget(amplitude);
end;

procedure TBlowHole.stopBlowing;
begin
  FEnvelope.setRate(rate);
  FEnvelope.setTarget(0.0);
end;

procedure TBlowHole.noteOn;
begin
  setFrequency(frequency);
  startBlowing(0.55 + (amplitude * 0.30), amplitude * 0.005);
  FOutputGain := amplitude + 0.001;
end;

procedure TBlowHole.noteOff;
begin
  stopBlowing(amplitude * 0.01);
end;

function TBlowHole.tick: Single;
var
  pth, pa, pb, pressureDiff, breathPressure, temp: Single;
begin
  // Calculate the breath pressure (FEnvelope + FNoise + FVibrato)
  breathPressure := FEnvelope.tick();
  breathPressure := breathpressure + breathPressure * FNoiseGain * FNoise.tick();
  breathPressure := breathpressure + breathPressure * FVibratoGain *
    FVibrato.tick();

  // Calculate the differential pressure := reflected - mouthpiece pressures
  pressureDiff := FDelays[0].lastOut() - breathPressure;

  // Do two-port junction scattering for register FVent
  pa := breathPressure + pressureDiff * FReedTable.tick(pressureDiff);
  pb := FDelays[1].lastOut();
  FVent.tick(pa + pb);

  lastOutput := FDelays[0].tick(FVent.lastOut() + pb);
  lastOutput := lastoutput * FOutputGain;

  // Do three-port junction scattering (under FTonehole)
  pa := pa + FVent.lastOut();
  pb := FDelays[2].lastOut();
  pth := FTonehole.lastOut();
  temp := FScatter * (pa + pb - 2 * pth);

  FDelays[2].tick(FFilter.tick(pa + temp) * -0.95);
  FDelays[1].tick(pb + temp);
  FTonehole.tick(pa + pb - pth + temp);

  Result := lastOutput;
end;

procedure TBlowHole.controlChange;
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
    setTonehole(norm)
  else if (number = __SK_ModWheel_) then // 1
    setVent(norm)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    FEnvelope.setValue(norm);
end;

end.
