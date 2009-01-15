unit DAV_StkPoleZero;

{
/***************************************************/
/*! \class TPoleZero
    \brief STK one-pole, one-zero filter class.

    This protected Filter subclass implements
    a one-pole, one-zero digital filter.  A
    method is provided for creating an allpass
    filter with a given coefficient.  Another
    method is provided to create a DC blocking filter.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}
interface

uses stk, filter;

type
  TPoleZero = class(TFilter)
  public
  //! Default constructor creates a first-order pass-through filter.
    constructor Create(sr: my_float);

  //! Class destructor.
    destructor Destroy;

  //! Clears the internal states of the filter.
    procedure Clear;

  //! Set the b[0] coefficient value.
    procedure setB0(b0: MY_FLOAT);

  //! Set the b[1] coefficient value.
    procedure setB1(b1: MY_FLOAT);

  //! Set the a[1] coefficient value.
    procedure setA1(a1: MY_FLOAT);

  //! Set the filter for allpass behavior using \e coefficient.
  {
    This method uses \e coefficient to create an allpass filter,
    which has unity gain at all frequencies.  Note that the \e
    coefficient magnitude must be less than one to maintain stability.
  }
    procedure setAllpass(coefficient: MY_FLOAT);

  //! Create a DC blocking filter with the given pole position in the z-plane.
  {
    This method sets the given pole position, together with a zero
    at z=1, to create a DC blocking filter.  \e thePole should be
    close to one to minimize low-frequency attenuation.
  }
    procedure setBlockZero(thePole: MY_FLOAT = 0.99);

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

constructor TPoleZero.Create;
var
  a, b: array[0..1] of my_float;
begin
  inherited Create(sr);
  // Default setting for pass-through.
  b[0] := 1;
  b[1] := 0;
  a[0] := 1;
  a[1] := 0;
  inherited setCoefficients(2, @B, 2, @A);
end;

destructor TPoleZero.Destroy;
begin
  inherited Destroy;
end;

procedure TPoleZero.Clear;
begin
  inherited Clear;
end;

procedure TPoleZero.setB0;
var
  p: pmy_float;
begin
  p := pindex(b, 0);
  p^ := b0;
end;

procedure TPoleZero.setB1;
var
  p: pmy_float;
begin
  p := pindex(b, 1);
  p^ := b1;
end;

procedure TPoleZero.setA1;
var
  p: pmy_float;
begin
  p := pindex(a, 1);
  p^ := a1;
end;

procedure TPoleZero.setAllpass;
var
  p: pmy_float;
begin
  p := b;
  p^ := coefficient;
  Inc(p);
  p^ := 1.0;
  p := a;
  p^ := 1.0; // just in case
  Inc(p);
  p^ := coefficient;
end;

procedure TPoleZero.setBlockZero;//0.99
var
  p: pmy_float;
begin
  p := b;
  p^ := 1.0;
  Inc(p);
  p^ := -1.0;
  p := a;
  p^ := 1.0; // just in case
  Inc(p);
  p^ := -thePole;
end;

procedure TPoleZero.setGain;
begin
  inherited setGain(theGain);
end;

function TPoleZero.getGain: my_float;
begin
  Result := inherited getGain;
end;

function TPoleZero.lastOut: my_float;
begin
  Result := inherited lastOut;
end;

function TPoleZero.tick(sample: my_float): my_float;
var
  p: pmy_float;
begin
  inputs^ := gain * sample;
  outputs^ := b^ * inputs^ + index(b, 1) * index(inputs, 1) -
    index(a, 1) * index(outputs, 1);
  p := pindex(inputs, 1);
  p^ := inputs^;
  p := pindex(outputs, 1);
  p^ := outputs^;
  Result := outputs^;
end;

function TPoleZero.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
