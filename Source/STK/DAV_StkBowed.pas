unit DAV_StkBowed;

{
/***************************************************/
/*! \class TBowed
    \brief STK TBowed string instrument class.

    This class implements a TBowed string model, a
    la Smith (1986), after McIntyre, Schumacher,
    Woodhouse (1983).

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    Control Change Numbers:
       - Bow Pressure := 2
       - Bow Position := 4
       - Vibrato Frequency := 11
       - Vibrato Gain := 1
       - Volume := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkInstrmnt, DAV_StkDelayl, DAV_StkBowtabl,
  DAV_StkOnepole, DAV_StkBiquad, DAV_StkLfo, DAV_StkAdsr, Windows;

type
  TBowed = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: my_float);

  //! Set vibrato gain.
    procedure setVibrato(gain: my_float);

  //! Apply breath pressure to instrument with given amplitude and rate of increase.
    procedure startBowing(amplitude, rate: my_float);

  //! Decrease breath pressure with given rate of decrease.
    procedure stopBowing(rate: my_float);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: my_float);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: my_float);

  //! Compute one output sample.
    function tick: my_float;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: my_float);

  protected
    neckDelay: tdelayl;
    bridgeDelay: tdelayl;
    bowTable: tbowtabl;
    stringFilter: tonepole;
    bodyFilter: tbiquad;
    vibrato: tlfo;
    adsr: tadsr;
    maxVelocity, baseDelay, vibratoGain, betaRatio: my_float;
  end;

implementation

constructor TBowed.Create;
var
  length: longint;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  neckDelay := TDelayL.Create(srate, 100.0, length);
  length := length shr 1;
  bridgeDelay := TDelayL.Create(srate, 29.0, length);
  bowTable := TBowTabl.Create(srate);
  bowTable.setSlope(3.0);
  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(6.12723);
  vibratoGain := 0.0;

  stringFilter := TOnePole.Create(srate);
  stringFilter.setPole((0.6 - (0.1 * 22050.0 / srate)));
  stringFilter.setGain(0.95);

  bodyFilter := TBiQuad.Create(srate);
  bodyFilter.setResonance(500.0, 0.85, True);
  bodyFilter.setGain(0.2);

  adsr := TADSR.Create(srate);
  adsr.setAllTimes(0.02, 0.005, 0.9, 0.01);

  betaRatio := 0.127236;

 // Necessary to initialize internal variables.
  setFrequency(220.0);
end;

destructor TBowed.Destroy;
begin
  inherited Destroy;
  neckDelay.Free;
  bridgeDelay.Free;
  bowTable.Free;
  stringFilter.Free;
  bodyFilter.Free;
  vibrato.Free;
  adsr.Free;
end;

procedure TBowed.Clear;
begin
  neckDelay.Clear();
  bridgeDelay.Clear();
end;

procedure TBowed.setFrequency;
var
  freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := length - approximate filter delay.
  baseDelay := srate / freakency - 4.0;
  if (baseDelay <= 0.0) then
    baseDelay := 0.3;
  bridgeDelay.setDelay(baseDelay * betaRatio);
                  // bow to bridge length
  neckDelay.setDelay(baseDelay * (1.0 - betaRatio));
 // bow to nut (finger) length
end;

procedure TBowed.startBowing;
begin
  adsr.setRate(rate);
  adsr.keyOn();
  maxVelocity := 0.03 + (0.2 * amplitude);
end;

procedure TBowed.stopBowing;
begin
  adsr.setRate(rate);
  adsr.keyOff();
end;

procedure TBowed.noteOn;
begin
  startBowing(amplitude, amplitude * 0.001);
  setFrequency(frequency);
end;

procedure TBowed.noteOff;
begin
  stopBowing((1.0 - amplitude) * 0.005);
end;

procedure TBowed.setVibrato;
begin
  vibratoGain := gain;
end;

function TBowed.tick: my_float;
var
  bowVelocity, bridgeRefl, nutRefl, newVel, velDiff, stringVel: my_float;
begin
  bowVelocity := maxVelocity * adsr.tick();
  bridgeRefl := -stringFilter.tick(bridgeDelay.lastOut());
  nutRefl := -neckDelay.lastOut();
  stringVel := bridgeRefl + nutRefl;               // Sum is String Velocity
  velDiff := bowVelocity - stringVel;              // Differential Velocity
  newVel := velDiff * bowTable.tick(velDiff);   // Non-Linear Bow Function
  neckDelay.tick(bridgeRefl + newVel);           // Do string propagations
  bridgeDelay.tick(nutRefl + newVel);

  if (vibratoGain > 0.0) then
    neckDelay.setDelay((baseDelay * (1.0 - betaRatio)) +
      (baseDelay * vibratoGain * vibrato.tick()));
  lastOutput := bodyFilter.tick(bridgeDelay.lastOut());
  Result := lastOutput;
end;

procedure TBowed.controlChange;
var
  norm: my_float;
begin
  norm := Value;// * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_BowPressure_) then // 2
    bowTable.setSlope(5.0 - (4.0 * norm))
  else if (number = __SK_BowPosition_) then
   begin // 4
    betaRatio := 0.027236 + (0.2 * norm);
    bridgeDelay.setDelay(baseDelay * betaRatio);
    neckDelay.setDelay(baseDelay * (1.0 - betaRatio));
   end
  else if (number = __SK_ModFrequency_) then // 11
    vibrato.setFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    vibratoGain := (norm * 0.4)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

end.
