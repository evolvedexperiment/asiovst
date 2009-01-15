unit DAV_StkBiQuad;

{
/***************************************************/
/*! \class TBiQuad
    \brief STK TBiQuad (two-pole, two-zero) filter class.

    This protected Filter subclass implements a
    two-pole, two-zero digital filter.  A method
    is provided for creating a resonance in the
    frequency response while maintaining a constant
    filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses DAV_StkCommon, DAV_StkFilter;

type
  TBiQuad = class(TFilter)
  public
  //! Default constructor creates a second-order pass-through filter.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Clears all internal states of the filter.
    procedure Clear;

  //! Set the b[0] coefficient value.
    procedure setB0(b0: MY_FLOAT);

  //! Set the b[1] coefficient value.
    procedure setB1(b1: MY_FLOAT);

  //! Set the b[2] coefficient value.
    procedure setB2(b2: MY_FLOAT);

  //! Set the a[1] coefficient value.
    procedure setA1(a1: MY_FLOAT);

  //! Set the a[2] coefficient value.
    procedure setA2(a2: MY_FLOAT);

  //! Sets the filter coefficients for a resonance at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  If \e normalize is true,
    the filter zeros are placed at z := 1, z := -1, and the coefficients
    are then normalized to produce a constant unity peak gain
    (independent of the filter \e gain parameter).  The resulting
    filter frequency response has a resonance at the given \e
    frequency.  The closer the poles are to the unit-circle (\e radius
    close to one), the narrower the resulting resonance width.
  }
    procedure setResonance(frequency, radius: my_float; normalize: boolean = False);

  //! Set the filter coefficients for a notch at \e frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate zeros with the given \e frequency (in Hz)
    and \e radius from the z-plane origin.  No filter normalization
    is attempted.
  }
    procedure setNotch(frequency, radius: my_float);

  //! Sets the filter zeroes for equal resonance gain.
  {
    When using the filter as a resonator, zeroes places at z := 1, z
    := -1 will result in a constant gain at resonance of 1 / (1 - R),
    where R is the pole radius setting.
  }
    procedure setEqualGainZeroes;

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

constructor TBiQuad.Create;
var
  a, b: array[0..2] of my_float;
begin
  inherited Create(sr);
  b[0] := 1;
  b[1] := 0;
  b[2] := 0;
  a[0] := 1;
  a[1] := 0;
  a[2] := 0;
  inherited setCoefficients(3, @B, 3, @A);
end;

destructor TBiQuad.Destroy;
begin
  inherited Destroy;
end;

procedure TBiQuad.Clear;
begin
  inherited Clear;
end;

procedure TBiQuad.setB0(b0: MY_FLOAT);
begin
  b^ := b0;
end;

procedure TBiQuad.setB1(b1: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(b, 1);
  p^ := b1;
end;

procedure TBiQuad.setB2(b2: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(b, 2);
  p^ := b2;
end;

procedure TBiQuad.setA1(a1: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(a, 1);
  p^ := a1;
end;

procedure TBiQuad.setA2(a2: MY_FLOAT);
var
  p: pmy_float;
begin
  p := pindex(a, 2);
  p^ := a2;
end;

procedure TBiQuad.setResonance;
var
  p: pmy_float;
begin
  p := pindex(a, 2);
  p^ := radius * radius;
  Dec(p);
  p^ := -2.0 * radius * cos(TWO_PI * frequency / srate);
  if (normalize) then
   begin
   // Use zeros at +- 1 and normalize the filter peak gain.
    p := b;
    p^ := 0.5 - 0.5 * index(a, 2);
    Inc(p);
    p^ := 0.0;
    Inc(p);
    p^ := -b^;
   end;
end;

procedure TBiQuad.setNotch;
var
  p: pmy_float;
begin
  p := pindex(b, 2);
 // This method does not attempt to normalize the filter gain.
  p^ := radius * radius;
  Dec(p);
  p^ := -2.0 * radius * cos(TWO_PI * frequency / srate);
end;

procedure TBiQuad.setEqualGainZeroes;
var
  p: pmy_float;
begin
  p := b;
  p^ := 1.0;
  Inc(p);
  p^ := 0.0;
  Inc(p);
  p^ := -1.0;
end;

procedure TBiQuad.setGain;
begin
  inherited setGain(theGain);
end;

function TBiQuad.getGain;
begin
  Result := inherited getGain;
end;

function TBiQuad.lastOut;
begin
  Result := inherited lastOut;
end;

function TBiQuad.tick(sample: MY_FLOAT): MY_FLOAT;
var
  p: pmy_float;
begin
  inputs^ := gain * sample;
  outputs^ := b^ * inputs^ + index(b, 1) * index(inputs, 1) +
    index(b, 2) * index(inputs, 2);
  outputs^ := outputs^ - (index(a, 2) * index(outputs, 2) +
    index(a, 1) * index(outputs, 1));
  p := pindex(inputs, 2);
  p^ := index(inputs, 1);
  Dec(p);
  p^ := inputs^;
  p := pindex(outputs, 2);
  p^ := index(outputs, 1);
  Dec(p);
  p^ := outputs^;
  Result := outputs^;
end;

function TBiQuad.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
