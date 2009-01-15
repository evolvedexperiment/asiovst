unit DAV_StkSingWave;

{
/***************************************************/
/*! \class TSingWave
    \brief STK "singing" looped soundfile class.

    This class contains all that is needed to make
    a pitched musical sound, like a simple voice
    or violin.  In general, it will not be used
    alone because of munchkinification effects
    from pitch shifting.  It will be used as an
    excitation source for other instruments.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, waveplayer, modulate, envelope;

type
  TSingWave = class(TStk)
  public
  //! Class constructor taking filename argument.
  {
    An StkError will be thrown if the file is not found, its format is
    unknown, or a read error occurs.
  }
    constructor Create(sr: my_float; fn: string);

  //! Class destructor.
    destructor Destroy;

  //! Reset file to beginning.
    procedure reset;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Set the vibrato frequency in Hz.
    procedure setVibratoRate(aRate: MY_FLOAT);

  //! Set the vibrato gain.
    procedure setVibratoGain(gain: MY_FLOAT);

  //! Set the random-ness amount.
    procedure setRandomGain(gain: MY_FLOAT);

  //! Set the sweep rate.
    procedure setSweepRate(aRate: MY_FLOAT);

  //! Set the gain rate.
    procedure setGainRate(aRate: MY_FLOAT);

  //! Set the gain target value.
    procedure setGainTarget(target: MY_FLOAT);

  //! Start a note.
    procedure noteOn;

  //! Stop a note.
    procedure noteOff;

  //! Return the last output value.
    function lastOut: MY_FLOAT;

  //! Compute one output sample.
    function tick: MY_FLOAT;

  protected

    wave: twaveplayer;
    modulator: tmodulate;
    Envelope: tenvelope;
    pitchEnvelope: tenvelope;
    rate, sweepRate, lastOutput: my_float;
  end;

implementation

constructor TSingWave.Create;
begin
  inherited Create(sr);
  wave := TWavePlayer.Create(srate, fn);
  wave.SetOneShot(False);
  rate := 1.0;
  sweepRate := 0.001;
  modulator := TModulate.Create(srate);
  modulator.setVibratoRate(6.0);
  modulator.setVibratoGain(0.04);
  modulator.setRandomGain(0.005);
  envelope := TEnvelope.Create(srate);
  pitchEnvelope := TEnvelope.Create(srate);
  setFrequency(75.0);
  pitchEnvelope.setRate(1.0);
  tick;
  tick;
  pitchEnvelope.setRate(sweepRate * rate);
end;

destructor TSingWave.Destroy;
begin
  inherited Destroy;
  wave.Free;
  modulator.Free;
  envelope.Free;
  pitchEnvelope.Free;
end;

procedure TSingWave.reset;
begin
  wave.reset;
  lastOutput := 0.0;
end;

procedure TSingWave.setFrequency;
var
  temp: my_float;
begin
  temp := rate;
  rate := frequency / srate;
  temp := temp - rate;
  if (temp < 0) then
    temp := -temp;
  pitchEnvelope.setTarget(rate);
  pitchEnvelope.setRate(sweepRate * temp);
end;

procedure TSingWave.setVibratoRate;
begin
  modulator.setVibratoRate(aRate);
end;

procedure TSingWave.setVibratoGain;
begin
  modulator.setVibratoGain(gain);
end;

procedure TSingWave.setRandomGain;
begin
  modulator.setRandomGain(gain);
end;

procedure TSingWave.setSweepRate;
begin
  sweepRate := aRate;
end;

procedure TSingWave.setGainRate;
begin
  envelope.setRate(aRate);
end;

procedure TSingWave.setGainTarget;
begin
  envelope.setTarget(target);
end;

procedure TSingWave.noteOn;
begin
  envelope.keyOn;
end;

procedure TSingWave.noteOff;
begin
  envelope.keyOff;
end;

function TSingWave.tick: my_float;
var
  newrate: my_float;
begin
  // Set the wave rate.
  newRate := pitchEnvelope.tick;
  newRate := newrate + newRate * modulator.tick;
  wave.setfrequency(newRate);
  lastOutput := wave.tick;
  lastOutput := lastoutput * envelope.tick;
  Result := lastOutput;
end;

function TSingWave.lastOut: my_float;
begin
  Result := lastOutput;
end;

end.
