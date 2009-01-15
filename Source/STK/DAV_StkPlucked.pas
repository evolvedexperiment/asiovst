unit DAV_StkPlucked;

{
/***************************************************/
/*! \class TPlucked
    \brief STK TPlucked string model class.

    This class implements a simple TPlucked string
    physical model based on the Karplus-Strong
    algorithm.

    This is a digital waveguide model, making its
    use possibly subject to patents held by
    Stanford University, Yamaha, and others.
    There exist at least two patents, assigned to
    Stanford, bearing the names of Karplus and/or
    Strong.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}

interface

uses stk, instrmnt, delaya, onezero, onepole, noise;

type
  TPlucked = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: MY_FLOAT);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Pluck the string with the given amplitude using the current frequency.
    procedure pluck(amplitude: MY_FLOAT);

  //! Start a note with the given frequency and amplitude.
    procedure noteOn(frequency, amplitude: MY_FLOAT);

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  protected
    delayLine: TDelayA;
    loopFilter: TOneZero;
    pickFilter: TOnePole;
    noise: TNoise;
    length: longint;
    loopGain: my_float;
  end;

implementation

constructor TPlucked.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  loopGain := 0.999;
  delayLine := TDelayA.Create(srate, (length / 2.0), length);
  loopFilter := TOneZero.Create(srate);
  pickFilter := TOnePole.Create(srate);
  noise := TNoise.Create(srate);
  Clear;
end;

destructor TPlucked.Destroy;
begin
  inherited Destroy;
  delayLine.Free;
  loopFilter.Free;
  pickFilter.Free;
  noise.Free;
end;

procedure TPlucked.Clear;
begin
  delayLine.Clear;
  loopFilter.Clear;
  pickFilter.Clear;
end;

procedure TPlucked.setFrequency;
var
  delay, freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  // Delay := length - approximate filter delay.
  delay := (srate / freakency) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delayLine.setDelay(delay);
  loopGain := 0.995 + (freakency * 0.000005);
  if (loopGain >= 1.0) then
    loopGain := 0.99999;
end;

procedure TPlucked.pluck;
var
  gain: my_float;
  i: longint;
begin
  gain := amplitude;
  if (gain > 1.0) then
    gain := 1.0
  else if (gain < 0.0) then
    gain := 0.0;

  pickFilter.setPole(0.999 - (gain * 0.15));
  pickFilter.setGain(gain * 0.5);
  for i := 0 to length - 1 do

  // Fill delay with noise additively with current contents.
    delayLine.tick(0.6 * delayLine.lastOut + pickFilter.tick(noise.tick));
end;

procedure TPlucked.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
end;

procedure TPlucked.noteOff;
begin
  loopGain := 1.0 - amplitude;
  if (loopGain < 0.0) then
    loopGain := 0.0
  else if (loopGain > 1.0) then
    loopGain := 0.99999;
end;

function TPlucked.tick: my_float;
begin
  // Here's the whole inner loop of the instrument!!
  lastOutput := delayLine.tick(loopFilter.tick(
    delayLine.lastOut * loopGain));
  lastOutput := lastoutput * 3;
  Result := lastOutput;
end;

end.
