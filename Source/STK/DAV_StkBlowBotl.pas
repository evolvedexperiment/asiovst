unit DAV_StkBlowBotl;

{
/***************************************************/
/*! \class TBlowBotl
    \brief STK blown bottle instrument class.

    This class implements a helmholtz resonator
    (biquad filter) with a polynomial jet
    excitation (a la Cook).

    Control Change Numbers: 
       - Noise Gain := 4
       - Vibrato Frequency := 11
       - Vibrato Gain := 1
       - Volume := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkJettabl, DAV_StkBiquad,
  DAV_StkPolezero, DAV_StkNoise, DAV_StkAdsr, DAV_StkLfo;

type
  TBlowBotl = class(TInstrmnt)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Apply breath velocity to instrument with given amplitude and rate of increase.
    procedure startBlowing(amplitude, rate: MY_FLOAT);

  //! Decrease breath velocity with given rate of decrease.
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
    jetTable: tjettabl;
    resonator: tbiquad;
    dcBlock: tpolezero;
    noise: tnoise;
    adsr: tadsr;
    vibrato: tlfo;
    maxPressure, noiseGain, vibratoGain, outputGain: MY_FLOAT;
  end;

implementation

const
  __BOTTLE_RADIUS_ = 0.999;

constructor TBlowBotl.Create;
begin
  inherited Create(sr);
  jetTable := TJetTabl.Create(srate);
  dcBlock := TPoleZero.Create(srate);
  dcBlock.setBlockZero;
  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(5.925);
  vibratoGain := 0.0;
  resonator := TBiQuad.Create(srate);
  resonator.setResonance(500.0, __BOTTLE_RADIUS_, True);
  adsr := TADSR.Create(srate);
  adsr.setAllTimes(0.005, 0.01, 0.8, 0.010);
  noise := TNoise.Create(srate);
  noiseGain := 20.0;
  maxPressure := 0.0;
end;

destructor TBlowBotl.Destroy;
begin
  inherited Destroy;
  jetTable.Free;
  resonator.Free;
  dcBlock.Free;
  noise.Free;
  adsr.Free;
  vibrato.Free;
end;

procedure TBlowBotl.Clear;
begin
  resonator.Clear;
end;

procedure TBlowBotl.setFrequency;
var
  freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;
  resonator.setResonance(freakency, __BOTTLE_RADIUS_, True);
end;

procedure TBlowBotl.startBlowing;
begin
  adsr.setAttackRate(rate);
  maxPressure := amplitude;
  adsr.keyOn();
end;

procedure TBlowBotl.stopBlowing;
begin
  adsr.setReleaseRate(rate);
  adsr.keyOff();
end;

procedure TBlowBotl.noteOn;
begin
  setFrequency(frequency);
  startBlowing(1.1 + (amplitude * 0.20), amplitude * 0.02);
  outputGain := amplitude + 0.001;
end;

procedure TBlowBotl.noteOff;
begin
  stopBlowing(amplitude * 0.02);
end;

function TBlowBotl.tick: my_float;
var
  breathPressure, randPressure, pressureDiff: MY_FLOAT;
begin
  // Calculate the breath pressure (envelope + vibrato)
  breathPressure := maxPressure * adsr.tick();
  breathPressure := breathPressure + vibratoGain * vibrato.tick();

  pressureDiff := breathPressure - resonator.lastOut();

  randPressure := noiseGain * noise.tick();
  randPressure := randPressure * breathPressure;
  randPressure := randpressure * (1.0 + pressureDiff);

  resonator.tick(breathPressure + randPressure -
    (jetTable.tick(pressureDiff) * pressureDiff));
  lastOutput := 0.2 * outputGain * dcBlock.tick(pressureDiff);

  Result := lastOutput;
end;

procedure TBlowBotl.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_NoiseLevel_) then // 4
    noiseGain := norm * 30.0
  else if (number = __SK_ModFrequency_) then // 11
    vibrato.setFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    vibratoGain := norm * 0.4
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

end.
