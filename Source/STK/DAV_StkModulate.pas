unit DAV_StkModulate;

{
/***************************************************/
/*! \class TModulate
    \brief STK periodic/random modulator.

    This class combines random and periodic
    modulations to give a nice, natural human
    modulation function.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, subnoise, onepole, lfo;

type
  TModulate = class(TStk)
  public
  //! Class constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset internal state.
    procedure reset;

  //! Set the periodic (vibrato) rate or frequency in Hz.
    procedure setVibratoRate(aRate: MY_FLOAT);

  //! Set the periodic (vibrato) gain.
    procedure setVibratoGain(aGain: MY_FLOAT);

  //! Set the random modulation gain.
    procedure setRandomGain(aGain: MY_FLOAT);

  //! Compute one output sample.
    function tick: my_float; overload;

  //! Return \e vectorSize outputs in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  //! Return the last computed output value.
    function lastOut: MY_FLOAT;

  protected
    vibrato: TLFO;
    noise: TSubNoise;
    filter: TOnePole;
    vibratoGain, randomGain, lastOutput: my_float;
  end;

implementation

constructor TModulate.Create;
begin
  inherited Create(sr);
  vibrato := TLFO.Create(srate);
  vibrato.setFrequency(6.0);
  vibratoGain := 0.04;

  noise := TSubNoise.Create(srate, 330);
  randomGain := 0.05;

  filter := TOnePole.Create(srate, 0.999);
  filter.setGain(randomGain);
end;

destructor TModulate.Destroy;
begin
  inherited Destroy;
  vibrato.Free;
  noise.Free;
  filter.Free;
end;

procedure TModulate.reset;
begin
  lastOutput := 0.0;
end;

procedure TModulate.setVibratoRate;
begin
  vibrato.setFrequency(aRate);
end;

procedure TModulate.setVibratoGain;
begin
  vibratoGain := aGain;
end;

procedure TModulate.setRandomGain;
begin
  randomGain := aGain;
  filter.setGain(randomGain);
end;

function TModulate.tick: my_float;
begin
  // Compute periodic and random modulations.
  lastOutput := vibratoGain * vibrato.tick;
  lastOutput := lastOutput + filter.tick(noise.tick);
  Result := lastOutput;
end;

function TModulate.tick(vector: pmy_float; vectorSize: longint): pmy_float;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick;
    Inc(p);
   end;
  Result := vector;
end;

function TModulate.lastOut: my_float;
begin
  Result := lastOutput;
end;


end.
