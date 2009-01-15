unit DAV_StkJetTabl;

{
/***************************************************/
/*! \class TJetTabl
    \brief STK jet table class.

    This class implements a flue jet non-linear
    function, computed by a polynomial calculation.
    Contrary to the name, this is not a "table".

    Consult Fletcher and Rossing, Karjalainen,
    Cook, and others for more information.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk;

type
  TJetTabl = class(TStk)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Return the last output value.
    function lastOut: my_float;

  //! Return the function value for \e input.
    function tick(input: MY_FLOAT): MY_FLOAT; overload;

  //! Take \e vectorSize inputs and return the corresponding function values in \e vector.
    function tick(vector: pmy_float; vectorSize: longint): pmy_float; overload;

  protected
    lastOutput: MY_FLOAT;

  end;

implementation

constructor TJetTabl.Create;
begin
  inherited Create(sr);
  lastOutput := 0.0;
end;

destructor TJetTabl.Destroy;
begin
  inherited Destroy;
end;

function TJetTabl.lastOut: MY_FLOAT;
begin
  Result := lastOutput;
end;

function TJetTabl.tick(input: MY_FLOAT): MY_FLOAT;
begin
  // Perform "table lookup" using a polynomial
  // calculation (x^3 - x), which approximates
  // the jet sigmoid behavior.
  lastOutput := input * (input * input - 1.0);

  // Saturate at +/- 1.0.
  if (lastOutput > 1.0) then
    lastOutput := 1.0;
  if (lastOutput < -1.0) then
    lastOutput := -1.0;
  Result := lastOutput;
end;

function TJetTabl.tick(vector: pmy_float; vectorSize: longint): pmy_float;
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

