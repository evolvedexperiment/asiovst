unit DAV_StkOneZero;

{
/***************************************************/
/*! \class OneZero
    \brief STK one-zero filter class.

    This protected Filter subclass implements
    a one-zero digital filter.  A method is
    provided for setting the zero position
    along the real axis of the z-plane while
    maintaining a constant filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, filter;

type
  TOneZero = class(TFilter)
  public
  //! Default constructor creates a first-order low-pass filter.
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor which sets the pole position during instantiation.
    constructor Create(sr, theZero: MY_FLOAT); overload;

  //! Class destructor.
    destructor Destroy;

  //! Clears the internal state of the filter.
    procedure Clear;

  //! Set the b[0] coefficient value.
    procedure setB0(b0: MY_FLOAT);

  //! Set the b[1] coefficient value.
    procedure setB1(b1: MY_FLOAT);

  //! Set the zero position in the z-plane.
  {
    This method sets the zero position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive zero value produces a high-pass filter, while a
    negative zero value produces a low-pass filter.  This method does
    not affect the filter \e gain value.
  }
    procedure setZero(theZero: MY_FLOAT);

  //! Set the filter gain.
  {
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   }
    procedure setGain(theGain: MY_FLOAT);

  //! Return the current filter gain.
    function getGain: MY_FLOAT;

  //! Return the last computed output value.
    function lastOut: MY_FLOAT;

  //! Input one sample to the filter and return one output.
    function tick(sample: MY_FLOAT): MY_FLOAT; overload;

  //! Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;
  end;

implementation

constructor TOneZero.Create(sr: my_float);
var
  b: array[0..1] of my_float;
  a: my_float;
begin
  inherited Create(sr);
  A := 1.0;
  B[0] := 0.5;
  B[1] := 0.5;
  inherited setCoefficients(2, @B, 1, @A);
end;

constructor TOneZero.Create(sr, theZero: MY_FLOAT);
var
  b: array[0..1] of my_float;
  a: my_float;
begin
  inherited Create(sr);
  A := 1.0;
  // Normalize coefficients for unity gain.
  if (theZero > 0.0) then
    B[0] := 1.0 / (1.0 + theZero)
  else
    B[0] := 1.0 / (1.0 - theZero);

  B[1] := -theZero * B[0];
  inherited setCoefficients(2, @B, 1, @A);
end;

destructor TOneZero.Destroy;
begin
  inherited Destroy;
end;

procedure TOneZero.Clear;
begin
  inherited Clear;
end;

procedure TOneZero.setB0(b0: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(b, 0);
  p^ := b0;
end;

procedure TOneZero.setB1(b1: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(b, 1);
  p^ := b1;
end;

procedure TOneZero.setZero;
var
  p: pmy_float;
begin
  // Normalize coefficients for unity gain.
  if (theZero > 0.0) then
    b^ := 1.0 / (1.0 + theZero)
  else
    b^ := 1.0 / (1.0 - theZero);
  p := pindex(b, 1);
  p^ := -theZero * b^;
end;

procedure TOneZero.setGain;
begin
  inherited setGain(theGain);
end;

function TOneZero.getGain;
begin
  Result := inherited getGain;
end;

function TOneZero.lastOut;
begin
  Result := inherited lastOut;
end;

function TOneZero.tick(sample: MY_FLOAT): MY_FLOAT;
var
  p: pmy_float;
begin
  inputs^ := gain * sample;

  outputs^ := index(b, 1) * index(inputs, 1) + b^ * inputs^;
  p := pindex(inputs, 1);
  p^ := inputs^;

  Result := outputs^;
end;

function TOneZero.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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

