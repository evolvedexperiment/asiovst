unit DAV_StkTwoZero;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK two-zero filter class.

  This protected Filter subclass implements a two-zero digital filter. A method
  is provided for creating a "notch" in the frequency response while
  maintaining a constant filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Stk, DAV_StkFilter;

type
  TTwoZero = class(TFilter)
  public
    // Default constructor creates a second-order pass-through filter.
    constructor Create(sr: my_float);

    // Class destructor.
    destructor Destroy;

    // Clears the internal states of the filter.
    procedure Clear;

    // Set the b[0] coefficient value.
    procedure setB0(b0: my_float);

    // Set the a[1] coefficient value.
    procedure setB1(b1: my_float);

    // Set the a[2] coefficient value.
    procedure setB2(B2: my_float);

    // Sets the filter coefficients for a "notch" at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate zeros with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  The coefficients are then
    normalized to produce a maximum filter gain of one (independent of
    the filter \e gain parameter).  The resulting filter frequency
    response has a "notch" or anti-resonance at the given \e
    frequency.  The closer the zeros are to the unit-circle (\e radius
    close to or equal to one), the narrower the resulting notch width.
  }
    procedure setNotch(frequency, radius: my_float);

    // Set the filter gain.
  {
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   }
    procedure setGain(theGain: my_float);

    // Return the current filter gain.
    function getGain: my_float;

    // Return the last computed output value.
    function lastOut: my_float;

    // Input one sample to the filter and return one output.
    function tick(sample: my_float): my_float; overload;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: pmy_float; vectorSize: longint): Pmy_float; overload;
  end;

implementation

constructor TTwoZero.Create;
var
  a: my_float;
  b: array[0..2] of my_float;
begin
  inherited Create(sr);
  A := 1.0;
  B[0] := 1;
  B[1] := 0;
  B[2] := 0;
  inherited setCoefficients(3, @B, 1, @A);
end;

destructor TTwoZero.Destroy;
begin
  inherited Destroy;
end;

procedure TTwoZero.Clear;
begin
  inherited Clear;
end;

procedure TTwoZero.setB0;
begin
  b^ := b0;
end;

procedure TTwoZero.setB1;
var
  p: pmy_float;
begin
  p := b;
  Inc(p);
  p^ := b1;
end;

procedure TTwoZero.setB2;
var
  p: pmy_float;
begin
  p := b;
  Inc(p);
  Inc(p);
  p^ := b2;
end;

procedure TTwoZero.setNotch;
var
  p: pmy_float;
begin
  p := pindex(b, 2);
  p^ := radius * radius;
  Dec(p);
  p^ := -2.0 * radius * cos(TWO_PI * frequency / srate);

  // Normalize the filter gain.
  if (index(b, 1) > 0.0) then // Maximum at z = 0.
    b^ := 1.0 / (1.0 + index(b, 1) + index(b, 2))
  else            // Maximum at z = -1.
    b^ := 1.0 / (1.0 - index(b, 1) + index(b, 2));
  p := pindex(b, 1);
  p^ := p^ * b^;
  Inc(p);
  p^ := p^ * b^;
end;

procedure TTwoZero.setGain;
begin
  inherited setGain(theGain);
end;

function TTwoZero.getGain: my_float;
begin
  Result := inherited getGain;
end;

function TTwoZero.lastOut: my_float;
begin
  Result := inherited lastOut;
end;

function TTwoZero.tick(sample: my_float): my_float;
var
  p: pmy_float;
begin
  inputs^ := gain * sample;

  outputs^ := index(b, 2) * index(inputs, 2) + index(b, 1) * index(inputs, 1) + b^ * inputs^;
  p := pindex(inputs, 2);
  p^ := index(inputs, 1);
  Dec(p);
  p^ := inputs^;

  Result := outputs^;
end;

function TTwoZero.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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

