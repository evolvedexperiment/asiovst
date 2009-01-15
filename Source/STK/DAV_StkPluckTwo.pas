unit DAV_StkPluckTwo;

{
/***************************************************/
/*! \class TPluckTwo
    \brief STK enhanced plucked string model class.

    This class implements an enhanced two-string,
    plucked physical model, a la Jaffe-Smith,
    Smith, and others.

    TPluckTwo is an abstract class, with no excitation
    specified.  Therefore, it can't be directly
    instantiated.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, instrmnt, delayl, delaya, onezero;

type
  TPluckTwo = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Detune the two strings by the given factor.  A value of 1.0 produces unison strings.
    procedure setDetune(detune: MY_FLOAT);

  //! Efficient combined setting of frequency and detuning.
    procedure setFreqAndDetune(frequency, detune: MY_FLOAT);

  //! Set the pluck or "excitation" position along the string (0.0 - 1.0).
    procedure setPluckPosition(position: MY_FLOAT);

  //! Set the base loop gain.
  {
    The actual loop gain is set according to the frequency.
    Because of high-frequency loop filter roll-off, higher
    frequency settings have greater loop gains.
  }
    procedure setBaseLoopGain(aGain: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Virtual (abstract) tick function is implemented by subclasses.
    function tick: MY_FLOAT;

  protected
    delayLine, delayline2: tdelaya;
    combDelay: tdelayl;
    filter, filter2: tonezero;
    length: longint;
    loopGain, baseLoopGain, lastFrequency, lastLength,
    detuning, pluckAmplitude, pluckPosition: MY_FLOAT;
  end;

implementation

constructor TPluckTwo.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  baseLoopGain := 0.995;
  loopGain := 0.999;
  delayLine := TDelayA.Create(srate, (length / 2.0), length);
  delayLine2 := TDelayA.Create(srate, (length / 2.0), length);
  combDelay := TDelayL.Create(srate, (length / 2.0), length);
  filter := TOneZero.Create(srate);
  filter2 := TOneZero.Create(srate);
  pluckAmplitude := 0.3;
  pluckPosition := 0.4;
  detuning := 0.995;
  lastFrequency := lowestFrequency * 2.0;
  lastLength := length * 0.5;
end;

destructor TPluckTwo.Destroy;
begin
  inherited Destroy;
  delayLine.Free;
  delayLine2.Free;
  combDelay.Free;
  filter.Free;
  filter2.Free;
end;

procedure TPluckTwo.Clear;
begin
  delayLine.Clear;
  delayLine2.Clear;
  combDelay.Clear;
  filter.Clear;
  filter2.Clear;
end;

procedure TPluckTwo.setFrequency;
var
  delay: my_float;
begin
  lastFrequency := frequency;
  if (lastFrequency <= 0.0) then
    lastFrequency := 220.0;

  // Delay := length - approximate filter delay.
  lastLength := (srate / lastFrequency);
  delay := (lastLength / detuning) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delayLine.setDelay(delay);

  delay := (lastLength * detuning) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delayLine2.setDelay(delay);

  loopGain := baseLoopGain + (frequency * 0.000005);
  if (loopGain > 1.0) then
    loopGain := 0.99999;
end;

procedure TPluckTwo.setDetune;
begin
  detuning := detune;
  if (detuning <= 0.0) then
    detuning := 0.1;
  delayLine.setDelay((lastLength / detuning) - 0.5);
  delayLine2.setDelay((lastLength * detuning) - 0.5);
end;

procedure TPluckTwo.setFreqAndDetune;
begin
  detuning := detune;
  setFrequency(frequency);
end;

procedure TPluckTwo.setPluckPosition;
begin
  pluckPosition := position;
  if (position < 0.0) then
    pluckPosition := 0.0
  else if (position > 1.0) then
    pluckPosition := 1.0;
end;

procedure TPluckTwo.setBaseLoopGain;
begin
  baseLoopGain := aGain;
  loopGain := baseLoopGain + (lastFrequency * 0.000005);
  if (loopGain > 0.99999) then
    loopGain := 0.99999;
end;

procedure TPluckTwo.noteOff;
begin
  loopGain := (1.0 - amplitude) * 0.5;
end;

function TPluckTwo.tick: MY_FLOAT;
begin
  Result := 0;
end;

end.
