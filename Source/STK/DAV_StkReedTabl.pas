unit DAV_StkReedTabl;

{
/***************************************************/
/*! \class ReedTabl
    \brief STK reed table class.

    This class implements a simple one breakpoint,
    non-linear reed function, as described by
    Smith (1986).  This function is based on a
    memoryless non-linear spring model of the reed
    (the reed mass is ignored) which saturates when
    the reed collides with the mouthpiece facing.

    See McIntyre, Schumacher, & Woodhouse (1983),
    Smith (1986), Hirschman, Cook, Scavone, and
    others for more information.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk;

type
  TReedTabl = class(TStk)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Set the table offset value.
  {
    The table offset roughly corresponds to the size
    of the initial reed tip opening (a greater offset
    represents a smaller opening).
  }
    procedure setOffset(aValue: MY_FLOAT);

  //! Set the table slope value.
  {
   The table slope roughly corresponds to the reed
   stiffness (a greater slope represents a harder
   reed).
  }
    procedure setSlope(aValue: MY_FLOAT);

  //! Return the last output value.
    function lastOut: MY_FLOAT;

  //! Return the function value for \e input.
  {
    The function input represents the differential
    pressure across the reeds.
  }
    function tick(input: MY_FLOAT): MY_FLOAT; overload;

  //! Take \e vectorSize inputs and return the corresponding function values in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;

  protected
    offSet, slope, lastOutput: my_float;
  end;

implementation

constructor TReedTabl.Create;
begin
  inherited Create(sr);
  offSet := 0.6;  // Offset is a bias, related to reed rest position.
  slope := -0.8;  // Slope corresponds loosely to reed stiffness.
end;

destructor TReedTabl.Destroy;
begin
  inherited Destroy;
end;

procedure TReedTabl.setOffset;
begin
  offSet := aValue;
end;

procedure TReedTabl.setSlope;
begin
  slope := aValue;
end;

function TReedTabl.lastOut: my_float;
begin
  Result := lastOutput;
end;

function TReedTabl.tick(input: MY_FLOAT): MY_FLOAT;
begin
  // The input is differential pressure across the reed.
  lastOutput := offSet + (slope * input);

  // If output is > 1, the reed has slammed shut and the
  // reflection function value saturates at 1.0.
  if (lastOutput > 1.0) then
    lastOutput := 1.0;

  // This is nearly impossible in a physical system, but
  // a reflection function value of -1.0 corresponds to
  // an open end (and no discontinuity in bore profile).
  if (lastOutput < -1.0) then
    lastOutput := -1.0;
  Result := lastOutput;
end;

function TReedTabl.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
