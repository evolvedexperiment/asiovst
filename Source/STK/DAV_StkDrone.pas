unit DAV_StkDrone;

{
/***************************************************/
/*! \class Drone
    \brief STK "drone" plucked string model.

    This class implements a simple plucked string
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

uses
  DAV_StkCommon, DAV_StkInstrument, DAV_StkDelaya, DAV_StkOneZero, DAV_StkAdsr,
  DAV_StkNoise;

type
  TDrone = class(TInstrmnt)
  public
  //! Class constructor, taking the lowest desired playing frequency.
    constructor Create(sr, lowestFrequency: my_float);

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
    delayLine: tdelaya;
    loopFilter: tonezero;
    envelope: tadsr;
    noise: tnoise;
    length: integer;
    loopGain: MY_FLOAT;
  end;

implementation

constructor TDrone.Create;
begin
  inherited Create(sr);
  length := round(srate / lowestFrequency + 1);
  loopGain := 0.999;
  delayLine := TDelayA.Create(srate, (length / 2.0), length);
  loopFilter := TOneZero.Create(srate);
  noise := TNoise.Create(srate);
  envelope := TADSR.Create(srate);
  envelope.setAllTimes(2.0, 0.5, 0.0, 0.5);
  Clear;
end;

destructor TDrone.Destroy;
begin
  delayLine.Free;
  loopFilter.Free;
  envelope.Free;
  noise.Free;
end;

procedure TDrone.Clear;
begin
  delayLine.Clear;
  loopFilter.Clear;
end;

procedure TDrone.setFrequency;
var
  delay, freakency: my_float;
begin
  freakency := frequency;
  if (frequency <= 0.0) then
    freakency := 220.0;
 // Delay=length - approximate filter delay.
  delay := (srate / freakency) - 0.5;
  if (delay <= 0.0) then
    delay := 0.3
  else if (delay > length) then
    delay := length;
  delayLine.setDelay(delay);
  loopGain := 0.997 + (freakency * 0.000002);
  if (loopGain >= 1.0) then
    loopGain := 0.99999;
end;

procedure TDrone.pluck;
begin
  envelope.keyOn;
end;

procedure TDrone.noteOn;
begin
  setFrequency(frequency);
  pluck(amplitude);
end;

procedure TDrone.noteOff;
begin
  loopGain := 1.0 - amplitude;
  if (loopGain < 0.0) then
    loopGain := 0.0
  else if (loopGain > 1.0) then
    loopGain := 0.99999;
end;

function TDrone.tick: MY_FLOAT;
begin
 // Here's the whole inner loop of the instrument!!
  lastOutput := delayLine.tick(loopFilter.tick(delayLine.lastOut * loopGain) +
    (0.005 * envelope.tick * noise.tick));
  Result := lastOutput;
end;

end.
