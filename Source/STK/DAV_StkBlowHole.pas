unit DAV_StkBlowHole;

{
/***************************************************/
/*! \class TBlowHole
    \brief STK clarinet physical model with one
           register hole and one tonehole.

    This class is based on the clarinet model,
    with the addition of a two-port register hole
    and a three-port dynamic tonehole
    implementation, as discussed by Scavone and
    Cook (1998).

    In this implementation, the distances between
    the reed/register hole and tonehole/bell are
    fixed.  As a result, both the tonehole and
    register hole will have variable influence on
    the playing frequency, which is dependent on
    the length of the air column.  In addition,
    the highest playing freqeuency is limited by
    these fixed lengths.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers:
       - Reed Stiffness := 2
       - Noise Gain := 4
       - Tonehole State := 11
       - Register State := 1
       - Breath Pressure := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkDelayl, DAV_StkReedtabl,
  DAV_StkOnezero, DAV_StkPolezero, DAV_StkEnvelope, DAV_StkNoise, DAV_StkLfo,
  Math;

type
  TBlowHole = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr, lowestFrequency: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Set the tonehole state (0.0 := closed, 1.0 := fully open).
    procedure setTonehole(newValue: MY_FLOAT);

  //! Set the register hole state (0.0 := closed, 1.0 := fully open).
    procedure setVent(newValue: MY_FLOAT);

  //! Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure startBlowing(amplitude, rate: MY_FLOAT);

  //! Decrease breath pressure with given rate of decrease.
    procedure stopBlowing(rate: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    delays: array[0..2] of tdelayl;
    reedTable: treedtabl;
    filter: tonezero;
    tonehole: tpolezero;
    vent: tpolezero;
    envelope: tenvelope;
    noise: tnoise;
    vibrato: tlfo;
    length: longint;
    scatter, th_coeff, r_th, rh_coeff, rh_gain, outputGain,
    noiseGain, vibratoGain: MY_FLOAT;
  end;

implementation

constructor TBlowHole.Create;
var
  xi, zeta, psi, r_rh, r_b: double;
  te: my_float;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  // delays[0] is the delay line between the reed and the register vent.
  delays[0] := TDelayL.Create(srate, 5.0 * srate / 22050.0, 100);
  // delays[1] is the delay line between the register vent and the tonehole.
  delays[1] := TDelayL.Create(srate, length shr 1, length);
  // delays[2] is the delay line between the tonehole and the end of the bore.
  delays[2] := TDelayL.Create(srate, 4.0 * srate / 22050.0, 100);
  reedTable := TReedTabl.Create(srate);
  reedTable.setOffset(0.7);
  reedTable.setSlope(-0.3);
  filter := TOneZero.Create(srate);
  envelope := TEnvelope.Create(srate);
  noise := TNoise.Create(srate);

  // Calculate the initial tonehole three-port scattering coefficient
  r_b := 0.0075;    // main bore radius
  r_th := 0.003;          // tonehole radius
  scatter := -power(r_th, 2) / (power(r_th, 2) + 2 * power(r_b, 2));

  // Calculate tonehole coefficients
  te := 1.4 * r_th;    // effective length of the open hole
  th_coeff := (te * 2 * srate - 347.23) / (te * 2 * srate + 347.23);
  tonehole := TPoleZero.Create(srate);
  // Start with tonehole open
  tonehole.setA1(-th_coeff);
  tonehole.setB0(th_coeff);
  tonehole.setB1(-1.0);

  // Calculate register hole filter coefficients
  r_rh := 0.0015;    // register vent radius
  te := 1.4 * r_rh;       // effective length of the open hole
  xi := 0.0;         // series resistance term
  zeta := 347.23 + 2 * PI * power(r_b, 2) * xi / 1.1769;
  psi := 2 * PI * power(r_b, 2) * te / (PI * power(r_rh, 2));
  rh_coeff := (zeta - 2 * srate * psi) / (zeta + 2 * srate * psi);
  rh_gain := -347.23 / (zeta + 2 * srate * psi);
  vent := TPoleZero.Create(srate);
  vent.setA1(rh_coeff);
  vent.setB0(1.0);
  vent.setB1(1.0);
  // Start with register vent closed
  vent.setGain(0.0);
  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(5.735);
  outputGain := 1.0;
  noiseGain := 0.2;
  vibratoGain := 0.01;
end;

destructor TBlowHole.Destroy;
begin
  inherited Destroy;
  delays[0].Free;
  delays[1].Free;
  delays[2].Free;
  reedTable.Free;
  filter.Free;
  tonehole.Free;
  vent.Free;
  envelope.Free;
  noise.Free;
  vibrato.Free;
end;

procedure TBlowHole.Clear;
begin
  delays[0].Clear();
  delays[1].Clear();
  delays[2].Clear();
  filter.tick(0.0);
  tonehole.tick(0.0);
  vent.tick(0.0);
end;

procedure TBlowHole.setFrequency;
var
  delay, freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := length - approximate filter delay.
  delay := (srate / freakency) * 0.5 - 3.5;
  delay := delay - (delays[0].getDelay() + delays[2].getDelay());

  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delays[1].setDelay(delay);
end;

procedure TBlowHole.setVent;
var
  gain: my_float;
begin
  // This method allows setting of the register vent "open-ness" at
  // any point between "Open" (newValue := 1) and "Closed"
  // (newValue := 0).

  if (newValue <= 0.0) then
    gain := 0.0
  else if (newValue >= 1.0) then
    gain := rh_gain
  else
    gain := newValue * rh_gain;
  vent.setGain(gain);
end;

procedure TBlowHole.setTonehole;
var
  new_coeff: my_float;
begin
  // This method allows setting of the tonehole "open-ness" at
  // any point between "Open" (newValue := 1) and "Closed"
  // (newValue := 0).
  if (newValue <= 0.0) then
    new_coeff := 0.9995
  else if (newValue >= 1.0) then
    new_coeff := th_coeff
  else
    new_coeff := (newValue * (th_coeff - 0.9995)) + 0.9995;
  tonehole.setA1(-new_coeff);
  tonehole.setB0(new_coeff);
end;

procedure TBlowHole.startBlowing;
begin
  envelope.setRate(rate);
  envelope.setTarget(amplitude);
end;

procedure TBlowHole.stopBlowing;
begin
  envelope.setRate(rate);
  envelope.setTarget(0.0);
end;

procedure TBlowHole.noteOn;
begin
  setFrequency(frequency);
  startBlowing(0.55 + (amplitude * 0.30), amplitude * 0.005);
  outputGain := amplitude + 0.001;
end;

procedure TBlowHole.noteOff;
begin
  stopBlowing(amplitude * 0.01);
end;

function TBlowHole.tick: my_float;
var
  pth, pa, pb, pressureDiff, breathPressure, temp: my_float;
begin
  // Calculate the breath pressure (envelope + noise + vibrato)
  breathPressure := envelope.tick();
  breathPressure := breathpressure + breathPressure * noiseGain * noise.tick();
  breathPressure := breathpressure + breathPressure * vibratoGain *
    vibrato.tick();

  // Calculate the differential pressure := reflected - mouthpiece pressures
  pressureDiff := delays[0].lastOut() - breathPressure;

  // Do two-port junction scattering for register vent
  pa := breathPressure + pressureDiff * reedTable.tick(pressureDiff);
  pb := delays[1].lastOut();
  vent.tick(pa + pb);

  lastOutput := delays[0].tick(vent.lastOut() + pb);
  lastOutput := lastoutput * outputGain;

  // Do three-port junction scattering (under tonehole)
  pa := pa + vent.lastOut();
  pb := delays[2].lastOut();
  pth := tonehole.lastOut();
  temp := scatter * (pa + pb - 2 * pth);

  delays[2].tick(filter.tick(pa + temp) * -0.95);
  delays[1].tick(pb + temp);
  tonehole.tick(pa + pb - pth + temp);

  Result := lastOutput;
end;

procedure TBlowHole.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_ReedStiffness_) then // 2
    reedTable.setSlope(-0.44 + (0.26 * norm))
  else if (number = __SK_NoiseLevel_) then // 4
    noiseGain := (norm * 0.4)
  else if (number = __SK_ModFrequency_) then // 11
    setTonehole(norm)
  else if (number = __SK_ModWheel_) then // 1
    setVent(norm)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    envelope.setValue(norm);
end;

end.
