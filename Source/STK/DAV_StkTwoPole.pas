unit DAV_StkTwoPole;

{
/***************************************************/
/*! \class TTwoPole
    \brief STK two-pole filter class.

    This protected Filter subclass implements
    a two-pole digital filter.  A method is
    provided for creating a resonance in the
    frequency response while maintaining a nearly
    constant filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, filter;

type
  TTwoPole = class(TFilter)
  public
  //! Default constructor creates a second-order pass-through filter.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Clears the internal states of the filter.
    procedure Clear;

  //! Set the b[0] coefficient value.
    procedure setB0(b0: my_float);

  //! Set the a[1] coefficient value.
    procedure setA1(a1: my_float);

  //! Set the a[2] coefficient value.
    procedure setA2(a2: my_float);

  //! Sets the filter coefficients for a resonance at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  If \e normalize is true,
    the coefficients are then normalized to produce unity gain at \e
    frequency (the actual maximum filter gain tends to be slightly
    greater than unity when \e radius is not close to one).  The
    resulting filter frequency response has a resonance at the given
    \e frequency.  The closer the poles are to the unit-circle (\e
    radius close to one), the narrower the resulting resonance width.
    An unstable filter will result for \e radius >= 1.0.  For a better
    resonance filter, use a BiQuad filter. \sa BiQuad filter class
  }
    procedure setResonance(frequency, radius: my_float; normalize: boolean = False);

  //! Set the filter gain.
  {
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   }
    procedure setGain(theGain: my_float);

  //! Return the current filter gain.
    function getGain: my_float;

  //! Return the last computed output value.
    function lastOut: my_float;

  //! Input one sample to the filter and return one output.
    function tick(sample: my_float): my_float; overload;

  //! Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: pmy_float; vectorSize: longint): Pmy_float; overload;
  end;

implementation

constructor TTwoPole.Create;
var
  b: my_float;
  a: array[0..2] of my_float;
begin
  inherited Create(sr);
  B := 1.0;
  A[0] := 1;
  A[1] := 0;
  A[2] := 0;
  inherited setCoefficients(1, @B, 3, @A);
end;

destructor TTwoPole.Destroy;
begin
  inherited Destroy;
end;

procedure TTwoPole.Clear;
begin
  inherited Clear;
end;

procedure TTwoPole.setB0;
begin
  b^ := b0;
end;

procedure TTwoPole.setA1;
var
  p: pmy_float;
begin
  p := a;
  Inc(p);
  p^ := a1;
end;

procedure TTwoPole.setA2;
var
  p: pmy_float;
begin
  p := a;
  Inc(p);
  Inc(p);
  p^ := a2;
end;

procedure TTwoPole.setResonance;
var
  p: pmy_float;
  real, imag: my_float;
begin
  p := pindex(a, 2);
  p^ := radius * radius;
  Dec(p);
  p^ := -2.0 * radius * cos(TWO_PI * frequency / srate);

  if (normalize) then
   begin
    // Normalize the filter gain ... not terribly efficient.
    real := 1 - radius + (index(a, 2) - radius) *
      cos(TWO_PI * 2 * frequency / srate);
    imag := (index(a, 2) - radius) * sin(TWO_PI * 2 * frequency / srate);
    b^ := sqrt(real * real + imag * imag);
   end;
end;

procedure TTwoPole.setGain;
begin
  inherited setGain(theGain);
end;

function TTwoPole.getGain: my_float;
begin
  Result := inherited getGain;
end;

function TTwoPole.lastOut: my_float;
begin
  Result := inherited lastOut;
end;

function TTwoPole.tick(sample: my_float): my_float;
var
  p: pmy_float;
begin
  inputs^ := gain * sample;
  outputs^ := b^ * inputs^ - index(a, 2) * index(outputs, 2) -
    index(a, 1) * index(outputs, 1);
  p := pindex(outputs, 2);
  p^ := index(outputs, 1);
  Dec(p);
  p^ := outputs^;
  Result := outputs^;
end;

function TTwoPole.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
