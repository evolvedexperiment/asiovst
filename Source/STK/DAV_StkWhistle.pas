unit DAV_StkWhistle;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK police/referee whistle instrument class.

  This class implements a hybrid physical/spectral model of a police
  whistle (a la Cook).

  Control Change Numbers:
    - Noise Gain = 4
    - Fipple Modulation Frequency = 11
    - Fipple Modulation Gain = 1
    - Blowing Frequency Modulation = 2
    - Volume = 128
}
interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkInstrument, DAV_StkSphere, DAV_StkVector3d, DAV_StkNoise,
  DAV_StkLfo, DAV_StkOnePole, DAV_StkEnvelope, Math, Windows;

type
  TStkWhistle = class(TStkInstrument)
  protected
    tempvector, tempvectorp: TVector3D;
    OnePole: Tonepole;
    Noise: Tnoise;
    Envelope: Tenvelope;
    pea, bumper, can: tsphere;           // Declare a Spherical "can".
    sine: tlfo;
    baseFrequency, maxPressure, noiseGain, fippleFreqMod,
    fippleGainMod, blowFreqMod, tickSize, canLoss: Single;
    subSample, subSampCount: Integer;
  public
    // Class constructor.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Reset and clear all internal state.
    procedure Clear;

    // Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: Single);

    // Apply breath velocity to instrument with given amplitude and rate of increase.
    procedure startBlowing(amplitude, rate: Single);

    // Decrease breath velocity with given rate of decrease.
    procedure stopBlowing(rate: Single);

    // Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: Single);

    // Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: Single);

    // Compute one output sample.
    function tick: Single;

    // Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: Integer; Value: Single);
  end;

implementation

const
  CAN_RADIUS = 100;
  PEA_RADIUS = 30;
  BUMP_RADIUS = 5;
  NORM_CAN_LOSS = 0.97;
  SLOW_CAN_LOSS = 0.90;
  GRAVITY = 20.0;
  NORM_TICK_SIZE = 0.004;
  SLOW_TICK_SIZE = 0.0001;
  ENV_RATE = 0.001;

constructor TStkWhistle.Create;
begin
  inherited Create(SampleRate);
  tempVector := TVector3D.Create(0, 0, 0);
  can := TSphere.Create(CAN_RADIUS);
  pea := TSphere.Create(PEA_RADIUS);
  bumper := TSphere.Create(BUMP_RADIUS);
  sine := TLFO.Create(srate);
  sine.setFrequency(2800.0);
  can.setPosition(0, 0, 0); // set can location
  can.setVelocity(0, 0, 0); // and the velocity
  envelope := tenvelope.Create(srate);
  onepole := tonepole.Create(srate);
  noise := tnoise.Create(srate);
  onepole.setPole(0.95);  // 0.99
  bumper.setPosition(0.0, CAN_RADIUS - BUMP_RADIUS, 0);
  bumper.setPosition(0.0, CAN_RADIUS - BUMP_RADIUS, 0);
  pea.setPosition(0, CAN_RADIUS / 2, 0);
  pea.setVelocity(35, 15, 0);

  envelope.setRate(ENV_RATE);
  envelope.keyOn;

  fippleFreqMod := 0.5;
  fippleGainMod := 0.5;
  blowFreqMod := 0.25;
  noiseGain := 0.125;
  maxPressure := 0.0;
  baseFrequency := 2000;

  tickSize := NORM_TICK_SIZE;
  canLoss := NORM_CAN_LOSS;

  subSample := 1;
  subSampCount := subSample;
end;

destructor TStkWhistle.Destroy;
begin
 FreeAndNil(tempVector);
 FreeAndNil(can);
 FreeAndNil(pea);
 FreeAndNil(bumper);
 FreeAndNil(sine);
 FreeAndNil(envelope);
 FreeAndNil(onepole);
 FreeAndNil(noise);
 inherited Destroy;
end;

procedure TStkWhistle.Clear;
begin
end;

procedure TStkWhistle.setFrequency;
var
  freakency: Single;
begin
  freakency := frequency * 4;  // the Whistle is a transposing instrument
  if (frequency <= 0.0) then
    freakency := 220.0;
  baseFrequency := freakency;
end;

procedure TStkWhistle.startBlowing;
begin
  envelope.setRate(ENV_RATE);
  envelope.setTarget(amplitude);
end;

procedure TStkWhistle.stopBlowing;
begin
  envelope.setRate(rate);
  envelope.keyOff;
end;

procedure TStkWhistle.noteOn;
begin
  setFrequency(frequency);
  startBlowing(amplitude * 2.0, amplitude * 0.2);
end;

procedure TStkWhistle.noteOff;
begin
  stopBlowing(amplitude * 0.02);
end;

function TStkWhistle.tick: Single;
var
  soundMix, tempFreq: Single;
  dmod, envout, temp, temp1, temp2, tempX, tempY, phi, cosphi,
  sinphi, gain: Double;
begin
  envOut := 0;
  gain := 0.5;
  subsampcount := subsampcount - 1;
  if (subSampCount <= 0) then
   begin
    tempVectorP := pea.getPosition;
    subSampCount := subSample;
    temp := bumper.isInside(tempVectorP);
{   envOut:=envelope.tick;

    if (temp < (BUMP_RADIUS + PEA_RADIUS)) then
    begin
      tempX:=envOut * tickSize * 2000 * noise.tick;
      tempY:=-envOut * tickSize * 1000 * (1.0 + noise.tick);
      pea.addVelocity(tempX,tempY,0);
      pea.tick(tickSize);
    end;

{    dmod :=exp(-temp * 0.01);  // exp. distance falloff of fipple/pea effect
    temp:=onepole.tick(dmod);  // smooth it a little
    gain:=(1.0 - (fippleGainMod*0.5)) + (2.0 * fippleGainMod * temp);
    gain :=gain*gain;        // squared distance/gain
    tempFreq:=1.0 + fippleFreqMod*(0.25-temp) + blowFreqMod*(envOut-1.0);
    tempFreq :=tempfreq* baseFrequency;

    sine.setFrequency(tempFreq);

    tempVectorP:=pea.getPosition;
    temp:=can.isInside(tempVectorP);
    temp :=-temp;       // We know (hope) it's inside, just how much??
    if (temp < (PEA_RADIUS * 1.25)) then
    begin
      pea.getVelocity(tempVector);  //  This is the can/pea collision
      tempX:=tempVectorP.getX;  // calculation.  Could probably
      tempY:=tempVectorP.getY;  // simplify using tables, etc.
      phi:=-arctan2(tempY,tempX);
      cosphi:=cos(phi);
      sinphi:=sin(phi);
      temp1:=(cosphi*tempVector.getX) - (sinphi*tempVector.getY);
      temp2:=(sinphi*tempVector.getX) + (cosphi*tempVector.getY);
      temp1:=-temp1;
      tempX:=(cosphi*temp1) + (sinphi*temp2);
      tempY:=(-sinphi*temp1) + (cosphi*temp2);
      pea.setVelocity(tempX, tempY, 0);
      pea.tick(tickSize);
      pea.setVelocity(tempX*canLoss, tempY*canLoss, 0);
      pea.tick(tickSize);
    end;

    temp:=tempVectorP.getLength;
    if (temp > 0.01) then
    begin
      tempX:=tempVectorP.getX;
      tempY:=tempVectorP.getY;
      phi:=arctan2(tempY,tempX);
      phi :=phi+( 0.3 * temp / CAN_RADIUS);
      cosphi:=cos(phi);
      sinphi:=sin(phi);
      tempX:=3.0 * temp * cosphi;
      tempY:=3.0 * temp * sinphi;
    end
    else begin
      tempX:=0.0;
      tempY:=0.0;
    end;

    temp:=(0.9 + 0.1*subSample*noise.tick) * envOut * 0.6 * tickSize;
    pea.addVelocity(temp * tempX,
    (temp*tempY) - (GRAVITY*tickSize),0);
    pea.tick(tickSize);
 }
    //    bumper.tick(0.0);
   end;
{
  temp:=envOut * envOut * gain / 2;
  soundMix:=temp * (sine.tick + (noiseGain*noise.tick));
  lastOutput:=0.25 * soundMix; // should probably do one-zero filter here
 }
  Result := lastOutput;
end;

procedure TStkWhistle.controlChange;
var
  norm: Single;
begin
  norm := Limit(Value, 0, 1);

  if (number = __SK_NoiseLevel_) then // 4
    noiseGain := 0.25 * norm
  else if (number = __SK_ModFrequency_) then // 11
    fippleFreqMod := norm
  else if (number = __SK_ModWheel_) then // 1
    fippleGainMod := norm
  else if (number = __SK_AfterTouch_Cont_) then // 128
    envelope.setTarget(norm * 2.0)
  else if (number = __SK_Breath_) then // 2
    blowFreqMod := norm * 0.5
  else if (number = __SK_Sustain_) then // 64
    if (Value < 1.0) then
      subSample := 1;
end;

end.
