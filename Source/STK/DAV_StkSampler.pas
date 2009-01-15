unit DAV_StkSampler;

{
/***************************************************/
/*! \class TSampler
    \brief STK sampling synthesis abstract base class.

    This instrument contains up to 5 attack waves,
    5 looped waves, and an ADSR envelope.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, waveplayer, instrmnt, adsr, onepole;

type
  TSampler = class(TInstrmnt)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Reset and clear all internal state.
    procedure Clear;

  //! Set instrument parameters for a particular frequency.
    procedure setFrequency(frequency: MY_FLOAT);

  //! Initiate the envelopes with a key-on event and reset the attack waves.
    procedure keyOn;

  //! Signal a key-off event to the envelopes.
    procedure keyOff;

  //! Stop a note with the given amplitude (speed of decay).
    procedure noteOff(amplitude: MY_FLOAT);

  //! Compute one output sample.
    function tick: MY_FLOAT;

  //! Perform the control change specified by \e number and \e value (0.0 - 128.0).
    procedure controlChange(number: integer; Value: MY_FLOAT);

  protected
    ADSR: tadsr;
    attacks: array[0..4] of twaveplayer;
    loops: array[0..4] of twaveplayer;
    filter: tonepole;
    attackGain, loopGain, baseFrequency: my_float;
    loopratios, attackRatios: array[0..4] of MY_FLOAT;
    whichOne: integer;
  end;

implementation


constructor TSampler.Create;
begin
  inherited Create(sr);
  // We don't make the waves here yet, because
  // we don't know what they will be.
  adsr := TADSR.Create(srate);
  baseFrequency := 440.0;
  filter := TOnePole.Create(srate);
  attackGain := 0.25;
  loopGain := 0.25;
  whichOne := 0;
end;

destructor TSampler.Destroy;
begin
  inherited Destroy;
  adsr.Free;
  filter.Free;
end;

procedure TSampler.keyOn;
begin
  adsr.keyOn;
  attacks[0].reset;
end;

procedure TSampler.keyOff;
begin
  adsr.keyOff;
end;

procedure TSampler.noteOff;
begin
  keyOff;
end;

function TSampler.tick: my_float;
begin
  lastOutput := attackGain * attacks[whichOne].tick;
  lastOutput := lastoutput + loopGain * loops[whichOne].tick;
  lastOutput := filter.tick(lastOutput);
  lastOutput := lastoutput * adsr.tick;
  Result := lastOutput;
end;

procedure TSampler.Clear;
begin

end;

procedure TSampler.controlChange(number: integer; Value: MY_FLOAT);
begin

end;

procedure TSampler.setFrequency(frequency: MY_FLOAT);
begin

end;

end.
