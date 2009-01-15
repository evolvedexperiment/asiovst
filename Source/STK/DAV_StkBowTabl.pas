unit DAV_StkBowTabl;

{
/***************************************************/
/*! \class TBowTabl
    \brief STK bowed string table class.

    This class implements a simple bowed string
    non-linear function, as described by Smith (1986).

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses
  DAV_StkCommon, Math;

type
  TBowTabl = class(TStk)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set the table offset value.
  {
    The table offset is a bias which controls the
    symmetry of the friction.  If you want the
    friction to vary with direction, use a non-zero
    value for the offset.  The default value is zero.
  }
    procedure setOffset(aValue: my_float);

  //! Set the table slope value.
  {
   The table slope controls the width of the friction
   pulse, which is related to bow force.
  }
    procedure setSlope(aValue: my_float);

  //! Return the last output value.
    function lastOut: my_float;

  //! Return the function value for \e input.
  {
    The function input represents differential
    string-to-bow velocity.
  }
    function tick(input: my_float): my_float; overload;

  //! Take \e vectorSize inputs and return the corresponding function values in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    offSet, slope, lastOutput: my_float;
  end;

implementation

constructor TBowTabl.Create;
begin
  inherited Create(sr);
  offSet := 0.0;
  slope := 0.1;
end;

destructor TBowTabl.Destroy;
begin
  inherited Destroy;
end;

procedure TBowTabl.setOffset;
begin
  offSet := aValue;
end;

procedure TBowTabl.setSlope;
begin
  slope := aValue;
end;

function TBowTabl.lastOut: my_float;
begin
  Result := lastOutput;
end;

function TBowTabl.tick(input: my_float): my_float;
var
  sample: my_float;
begin
  // The input represents differential string vs. bow velocity.
  sample := input + offSet;  // add bias to input
  sample := sample * slope;          // then scale it
  lastOutput := abs(sample) + 0.75;
  lastOutput := power(lastOutput, -4.0);

  // Set minimum friction to 0.0
  //if (lastOutput < 0.0 ) lastOutput := 0.0;
  // Set maximum friction to 1.0.
  if (lastOutput > 1.0) then
    lastOutput := 1.0;

  Result := lastOutput;
end;

function TBowTabl.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
