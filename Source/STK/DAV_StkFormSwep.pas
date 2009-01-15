unit DAV_StkFormSwep;

{
/***************************************************/
/*! \class TFormSwep
    \brief STK sweepable formant filter class.

    This public BiQuad filter subclass implements
    a formant (resonance) which can be "swept"
    over time from one frequency setting to another.
    It provides methods for controlling the sweep
    rate and target frequency.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, DAV_StkBiquad;

type
  TFormSwep = class(TBiQuad)
  public
  //! Default constructor creates a second-order pass-through filter.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Sets the filter coefficients for a resonance at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  The filter zeros are
    placed at z := 1, z := -1, and the coefficients are then normalized to
    produce a constant unity gain (independent of the filter \e gain
    parameter).  The resulting filter frequency response has a
    resonance at the given \e frequency.  The closer the poles are to
    the unit-circle (\e radius close to one), the narrower the
    resulting resonance width.
  }
    procedure setResonance(aFrequency, aRadius: my_float);

  //! Set both the current and target resonance parameters.
    procedure setStates(aFrequency, aRadius: my_float; aGain: my_float = 1.0);

  //! Set target resonance parameters.
    procedure setTargets(aFrequency, aRadius: my_float; aGain: my_float = 1.0);

  //! Set the sweep rate (between 0.0 - 1.0).
  {
    The formant parameters are varied in increments of the
    sweep rate between their current and target values.
    A sweep rate of 1.0 will produce an immediate change in
    resonance parameters from their current values to the
    target values.  A sweep rate of 0.0 will produce no
    change in resonance parameters.  
  }
    procedure setSweepRate(aRate: my_float);

  //! Set the sweep rate in terms of a time value in seconds.
  {
    This method adjusts the sweep rate based on a
    given time for the formant parameters to reach
    their target values.
 }
    procedure setSweepTime(aTime: my_float);

  //! Input one sample to the filter and return one output.
    function tick(sample: my_float): my_float; overload;

  //! Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: pmy_float; vectorSize: longint): pmy_float; overload;

  protected
    dirty: boolean;
    frequency, radius, startFrequency, startRadius, startGain,
    targetFrequency, targetRadius, targetGain, deltaFrequency,
    deltaRadius, deltaGain, sweepState, sweepRate: my_float;
  end;

implementation

constructor TFormSwep.Create;
begin
  inherited Create(sr);
  frequency := 0;
  radius := 0;
  targetGain := 1;
  targetFrequency := 0;
  targetRadius := 0;
  deltaGain := 0;
  deltaFrequency := 0;
  deltaRadius := 0;
  sweepState := 0;
  sweepRate := 0.002;
  dirty := False;
  Clear;
end;

destructor TFormSwep.Destroy;
begin
  inherited Destroy;
end;

procedure TFormSwep.setResonance;
begin
  dirty := False;
  radius := aRadius;
  frequency := aFrequency;
  inherited setResonance(frequency, radius, True);
end;

procedure TFormSwep.setStates;
begin
  dirty := False;

  if ((frequency <> aFrequency) or (radius <> aRadius)) then
    inherited setResonance(aFrequency, aRadius, True);

  frequency := aFrequency;
  radius := aRadius;
  gain := aGain;
  targetFrequency := aFrequency;
  targetRadius := aRadius;
  targetGain := aGain;
end;

procedure TFormSwep.setTargets;
begin
  dirty := True;
  startFrequency := frequency;
  startRadius := radius;
  startGain := gain;
  targetFrequency := aFrequency;
  targetRadius := aRadius;
  targetGain := aGain;
  deltaFrequency := aFrequency - frequency;
  deltaRadius := aRadius - radius;
  deltaGain := aGain - gain;
  sweepState := 0;
end;

procedure TFormSwep.setSweepRate;
begin
  sweepRate := aRate;
  if (sweepRate > 1.0) then
    sweepRate := 1.0;
  if (sweepRate < 0.0) then
    sweepRate := 0.0;
end;

procedure TFormSwep.setSweepTime;
begin
  sweepRate := 1.0 / (aTime * srate);
  if (sweepRate > 1.0) then
    sweepRate := 1.0;
  if (sweepRate < 0.0) then
    sweepRate := 0.0;
end;

function TFormSwep.tick(sample: my_float): my_float;
begin
  if (dirty) then
   begin
    sweepState := sweepstate + sweepRate;
    if (sweepState >= 1.0) then
     begin
      sweepState := 1.0;
      dirty := False;
      radius := targetRadius;
      frequency := targetFrequency;
      gain := targetGain;
     end
    else
     begin
      radius := startRadius + (deltaRadius * sweepState);
      frequency := startFrequency + (deltaFrequency * sweepState);
      gain := startGain + (deltaGain * sweepState);
     end;
    inherited setResonance(frequency, radius, True);
   end;
  Result := inherited tick(sample);
end;

function TFormSwep.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
var
  i: integer;
  p: pmy_float;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
