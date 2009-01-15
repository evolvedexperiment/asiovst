unit DAV_StkFlute;

{
/***************************************************/
/*! \class TFlute
    \brief STK TFlute physical model class.

    This class implements a simple TFlute
    physical model, as discussed by Karjalainen,
    Smith, Waryznyk, etc.  The jet model uses
    a polynomial, a la Cook.

    This is a digital waveguide model, making its
    use possibly subject to patents held by Stanford
    University, Yamaha, and others.

    Control Change Numbers:
       - Jet Delay := 2
       - Noise Gain := 4
       - Vibrato Frequency := 11
       - Vibrato Gain := 1
       - Breath Pressure := 128

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkLfo, DAV_StkInstrument, DAV_StkJetTable, DAV_StkDelayl,
  DAV_StkOnePole, DAV_StkPoleZero, DAV_StkNoise, DAV_StkAdsr;

type
  TFlute = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Set the reflection coefficient for the jet delay (-1.0 - 1.0).
    procedure setJetReflection(coefficient: MY_FLOAT);

  //! Set the reflection coefficient for the air column delay (-1.0 - 1.0).
    procedure setEndReflection(coefficient: MY_FLOAT);

  //! Set the length of the jet delay in terms of a ratio of jet delay to air column delay lengths.
    procedure setJetDelay(aRatio: MY_FLOAT);

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
    procedure controlChange(number: integer; Value: my_float);

  protected
    jetDelay: tdelayl;
    boreDelay: tdelayl;
    jetTable: TJetTabl;
    filter: TOnePole;
    dcBlock: TPoleZero;
    noise: TNoise;
    adsr: TADSR;
    vibrato: TLFO;
    length: longint;
    lastFrequency, maxPressure, jetReflection, endReflection,
    noiseGain, vibratoGain, outputGain, jetRatio: my_float;
  end;

implementation

constructor TFlute.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  boreDelay := TDelayL.Create(srate, 100.0, length);
  length := length shr 1;
  jetDelay := TDelayL.Create(srate, 49.0, length);
  jetTable := TJetTabl.Create(srate);
  filter := TOnePole.Create(srate);
  dcBlock := TPoleZero.Create(srate);
  dcBlock.setBlockZero(0.99);
  noise := TNoise.Create(srate);
  adsr := TADSR.Create(srate);

  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(5.925);

  Clear;

  filter.setPole(0.7 - (0.1 * 22050.0 / srate));
  filter.setGain(-1.0);
  adsr.setAllTimes(0.005, 0.01, 0.8, 0.010);
  endReflection := 0.5;
  jetReflection := 0.5;
  noiseGain := 0.15;             // Breath pressure random component.
  vibratoGain := 0.05; // Breath periodic vibrato component.
  jetRatio := 0.32;

  maxPressure := 0.0;
  lastFrequency := 220.0;
end;

destructor TFlute.Destroy;
begin
  inherited Destroy;
  jetDelay.Free;
  boreDelay.Free;
  jetTable.Free;
  filter.Free;
  dcBlock.Free;
  noise.Free;
  adsr.Free;
  vibrato.Free;
end;

procedure TFlute.Clear;
begin
  jetDelay.Clear;
  boreDelay.Clear;
  filter.Clear;
  dcBlock.Clear;
end;

procedure TFlute.setFrequency;
var
  delay: my_float;
begin
  lastFrequency := frequency;
  if (frequency <= 0.0) then
    lastFrequency := 220.0;

  // We're overblowing here.
  lastFrequency := lastFrequency * 0.66666;
  // Delay := length - approximate filter delay.
  delay := srate / lastFrequency - 2.0;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;

  boreDelay.setDelay(delay);
  jetDelay.setDelay(delay * jetRatio);
end;

procedure TFlute.startBlowing;
begin
  adsr.setAttackRate(rate);
  maxPressure := amplitude / 0.8;
  adsr.keyOn;
end;

procedure TFlute.stopBlowing;
begin
  adsr.setReleaseRate(rate);
  adsr.keyOff;
end;

procedure TFlute.noteOn;
begin
  setFrequency(frequency);
  startBlowing(1.1 + (amplitude * 0.20), amplitude * 0.02);
  outputGain := amplitude + 0.001;
end;

procedure TFlute.noteOff;
begin
  stopBlowing(amplitude * 0.02);
end;

procedure TFlute.setJetReflection;
begin
  jetReflection := coefficient;
end;

procedure TFlute.setEndReflection;
begin
  endReflection := coefficient;
end;

procedure TFlute.setJetDelay;
var
  temp: my_float;
begin
  // Delay := length - approximate filter delay.
  temp := srate / lastFrequency - 2.0;
  jetRatio := aRatio;
  jetDelay.setDelay(temp * aRatio); // Scaled by ratio.
end;

function TFlute.tick: my_float;
var
  temp, pressureDiff, breathPressure: my_float;
begin
  // Calculate the breath pressure (envelope + noise + vibrato)
  breathPressure := maxPressure * adsr.tick;
  breathPressure := breathpressure + breathPressure * noiseGain * noise.tick;
  breathPressure := breathpressure + breathPressure * vibratoGain *
    vibrato.tick;

  temp := filter.tick(boreDelay.lastOut);
  temp := dcBlock.tick(temp); // Block DC on reflection.

  pressureDiff := breathPressure - (jetReflection * temp);
  pressureDiff := jetDelay.tick(pressureDiff);
  pressureDiff := jetTable.tick(pressureDiff) + (endReflection * temp);
  lastOutput := 0.3 * boreDelay.tick(pressureDiff);

  lastOutput := lastoutput * outputGain;
  Result := lastOutput;

end;

procedure TFlute.controlChange;
var
  norm: my_float;
begin
  norm := Value; // * ONE_OVER_128;
  if (norm < 0) then
    norm := 0.0
  else if (norm > 1.0) then
    norm := 1.0;

  if (number = __SK_JetDelay_) then // 2
    setJetDelay((0.08 + (0.48 * norm)))
  else if (number = __SK_NoiseLevel_) then // 4
    noiseGain := (norm * 0.4)
  else if (number = __SK_ModFrequency_) then // 11
    vibrato.setFrequency(norm * 12.0)
  else if (number = __SK_ModWheel_) then // 1
    vibratoGain := (norm * 0.4)
  else if (number = __SK_AfterTouch_Cont_) then // 128
    adsr.setTarget(norm);
end;

end.
