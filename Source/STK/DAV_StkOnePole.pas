unit DAV_StkOnePole;

{
/***************************************************/
/*! \class OnePole
    \brief STK one-pole filter class.

    This protected Filter subclass implements
    a one-pole digital filter.  A method is
    provided for setting the pole position along
    the real axis of the z-plane while maintaining
    a constant peak filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, filter;

type
  TOnePole = class(TFilter)
  public
  //! Default constructor creates a first-order low-pass filter.
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor which sets the pole position during instantiation.
    constructor Create(sr, thePole: MY_FLOAT); overload;

  //! Class destructor.
    destructor Destroy;

  //! Clears the internal state of the filter.
    procedure Clear;

  //! Set the b[0] coefficient value.
    procedure setB0(b0: MY_FLOAT);

  //! Set the a[1] coefficient value.
    procedure setA1(a1: MY_FLOAT);

  //! Set the pole position in the z-plane.
  {
    This method sets the pole position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive pole value produces a low-pass filter, while a negative
    pole value produces a high-pass filter.  This method does not
    affect the filter \e gain value.
  }
    procedure setPole(thePole: MY_FLOAT);

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

constructor TOnePole.Create(sr: my_float);
var
  a: array[0..1] of my_float;
  b: my_float;
begin
  inherited Create(sr);
  B := 0.1;
  A[0] := 1.0;
  A[1] := -0.9;
  inherited setCoefficients(1, @B, 2, @A);
end;

constructor TOnePole.Create(sr, thePole: MY_FLOAT);
var
  a: array[0..1] of my_float;
  b: my_float;
begin
  inherited Create(sr);
  A[0] := 1.0;
  A[1] := -0.9;
  // Normalize coefficients for peak unity gain.
  if (thePole > 0.0) then
    B := (1.0 - thePole)
  else
    B := (1.0 + thePole);

  A[1] := -thePole;
  inherited setCoefficients(1, @B, 2, @A);
end;

destructor TOnePole.Destroy;
begin
  inherited Destroy;
end;

procedure TOnePole.Clear;
begin
  inherited Clear;
end;

procedure TOnePole.setB0(b0: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(b, 0);
  p^ := b0;
end;

procedure TOnePole.setA1(a1: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(a, 1);
  p^ := a1;
end;

procedure TOnePole.setPole;
var
  p: pmy_float;
begin
  // Normalize coefficients for peak unity gain.
  if (thePole > 0.0) then
    b^ := (1.0 - thePole)
  else
    b^ := (1.0 + thePole);
  p := pindex(a, 1);
  p^ := -thePole;
end;

procedure TOnePole.setGain;
begin
  inherited setGain(theGain);
end;

function TOnePole.getGain;
begin
  Result := inherited getGain;
end;

function TOnePole.lastOut;
begin
  Result := inherited lastOut;
end;

function TOnePole.tick(sample: MY_FLOAT): MY_FLOAT;
var
  p: pmy_float;
begin
  inputs^ := gain * sample;
  outputs^ := b^ * inputs^ - index(a, 1) * index(outputs, 1);
  p := outputs;
  Inc(p);
  p^ := outputs^;
  Result := outputs^;
end;

function TOnePole.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
