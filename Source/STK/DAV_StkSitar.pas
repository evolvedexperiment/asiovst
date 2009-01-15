unit DAV_StkSitar;

{
/***************************************************/
/*! \class TSitar
    \brief STK TSitar string model class.

    This class implements a TSitar plucked string
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

uses stk, instrmnt, delaya, onezero, noise, adsr;

type
  TSitar = class(TInstrmnt)
  public
    envelope: TADSR;

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
    noise: TNoise;
    length: longint;
    amGain, delay, targetDelay, loopGain: my_float;
  end;

implementation

constructor TSitar.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  loopGain := 0.999;
  delayLine := TDelayA.Create(srate, (length / 2.0), length);
  delay := length / 2.0;
  targetDelay := delay;

  loopFilter := TOneZero.Create(srate);
  loopFilter.setZero(0.01);

  envelope := TADSR.Create(srate);
  envelope.setAllTimes(0.001, 0.04, 0.0, 0.5);
  noise := TNoise.Create(srate);
  Clear;
end;

destructor TSitar.Destroy;
begin
  inherited Destroy;
  delayLine.Free;
  loopFilter.Free;
  Envelope.Free;
  noise.Free;
end;

procedure TSitar.Clear;
begin
  delayLine.Clear;
  loopFilter.Clear;
end;

procedure TSitar.setFrequency;
var
  freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;

  targetDelay := (srate / freakency);
  delay := targetDelay * (1.0 + (0.05 * noise.tick()));
  delayLine.setDelay(delay);
  loopGain := 0.995 + (freakency * 0.0000005);
  if (loopGain > 0.9995) then
    loopGain := 0.9995;
end;

procedure TSitar.pluck;
begin
  envelope.keyOn;
end;

procedure TSitar.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
  amGain := 0.1 * amplitude;
end;

procedure TSitar.noteOff;
begin
  loopGain := 1.0 - amplitude;
  if (loopGain < 0.0) then
    loopGain := 0.0
  else if (loopGain > 1.0) then
    loopGain := 0.99999;
end;

function TSitar.tick: my_float;
begin
  if (abs(targetDelay - delay) > 0.001) then
   begin
    if (targetDelay < delay) then
      delay := delay * 0.99999
    else
      delay := delay * 1.00001;
    delayLine.setDelay(delay);
   end;
  lastOutput := delayLine.tick(loopFilter.tick(delayLine.lastOut * loopGain) +
    (amGain * envelope.tick * noise.tick));
  Result := lastOutput;
end;

end.
