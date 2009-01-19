unit DAV_StkOnePole;

// based on DAV_Stk by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ DAV_Stk one-pole DAV_StkFilter class.

  This protected DAV_StkFilter subclass implements a one-pole digital DAV_StkFilter. A method
  is provided for setting the pole position along the real axis of the z-plane
  while maintaining a constant peak DAV_StkFilter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses DAV_Stk, DAV_StkFilter;

type
  TStkOnePole = class(TStkFilter)
  public
    // Default constructor creates a first-order low-pass DAV_StkFilter.
    constructor Create(sr: Single); overload;

    // Overloaded constructor which sets the pole position during instantiation.
    constructor Create(sr, thePole: Single); overload;

    // Class destructor.
    destructor Destroy;

    // Clears the internal state of the DAV_StkFilter.
    procedure Clear;

    // Set the b[0] coefficient value.
    procedure setB0(b0: Single);

    // Set the a[1] coefficient value.
    procedure setA1(a1: Single);

    // Set the pole position in the z-plane.
  {
    This method sets the pole position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive pole value produces a low-pass DAV_StkFilter, while a negative
    pole value produces a high-pass DAV_StkFilter.  This method does not
    affect the DAV_StkFilter \e gain value.
  }
    procedure setPole(thePole: Single);

    // Set the DAV_StkFilter gain.
  {
    The gain is applied at the DAV_StkFilter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   }
    procedure setGain(theGain: Single);

    // Return the current DAV_StkFilter gain.
    function getGain: Single;

    // Return the last computed output value.
    function lastOut: Single;

    // Input one sample to the DAV_StkFilter and return one output.
    function tick(sample: Single): Single; overload;

    // Input \e vectorSize samples to the DAV_StkFilter and return an equal number of outputs in \e vector.
    function tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT; overload;
  end;

implementation

constructor TStkOnePole.Create(sr: Single);
var
  a: array[0..1] of Single;
  b: Single;
begin
  inherited Create(sr);
  B := 0.1;
  A[0] := 1.0;
  A[1] := -0.9;
  inherited setCoefficients(1, @B, 2, @A);
end;

constructor TStkOnePole.Create(sr, thePole: Single);
var
  a: array[0..1] of Single;
  b: Single;
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

destructor TStkOnePole.Destroy;
begin
  inherited Destroy;
end;

procedure TStkOnePole.Clear;
begin
  inherited Clear;
end;

procedure TStkOnePole.setB0(b0: Single);
var
  p: pmy_float;
begin
  p := pindex(b, 0);
  p^ := b0;
end;

procedure TStkOnePole.setA1(a1: Single);
var
  p: pmy_float;
begin
  p := pindex(a, 1);
  p^ := a1;
end;

procedure TStkOnePole.setPole;
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

procedure TStkOnePole.setGain;
begin
  inherited setGain(theGain);
end;

function TStkOnePole.getGain;
begin
  Result := inherited getGain;
end;

function TStkOnePole.lastOut;
begin
  Result := inherited lastOut;
end;

function TStkOnePole.tick(sample: Single): Single;
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

function TStkOnePole.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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
