unit DAV_StkNoise;

{
/***************************************************/
/*! \class Noise
    \brief STK noise generator.

    Generic random number generation using the
    C rand() function.  The quality of the rand()
    function varies from one OS to another.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk;

type
  TNoise = class(TStk)
  public
  //! Default constructor.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Return a random number between -1.0 and 1.0 using rand().
    function tick: my_float; overload;

  //! Return \e vectorSize random numbers between -1.0 and 1.0 in \e vector.
    function tick(vector: pmy_float; vectorSize: longint): pmy_float; overload;

  //! Return the last computed value.
    function lastOut: my_float;

  protected
    lastOutput: my_float;

  end;

implementation

constructor TNoise.Create;
begin
  inherited Create(sr);
  lastOutput := 0.0;
end;

destructor TNoise.Destroy;
begin
  inherited Destroy;
end;

function TNoise.tick: my_float;
begin
  lastOutput := (2.0 * random) - 1;
  Result := lastOutput;
end;

function TNoise.tick(vector: pmy_float; vectorSize: longint): pmy_float;
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

function TNoise.lastOut;
begin
  Result := lastOutput;
end;

end.
