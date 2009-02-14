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
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkTwoZero = class(TStkFilter)
  public
    // Default constructor creates a second-order pass-through filter.
    constructor Create(const SampleRate: Single); override;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal states of the filter.
    procedure Clear; override;

    // Set the b[0] coefficient value.
    procedure SetB0(const Value: Single);

    // Set the a[1] coefficient value.
    procedure SetB1(const Value: Single);

    // Set the a[2] coefficient value.
    procedure SetB2(const Value: Single);

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
    procedure SetNotch(const Frequency, Radius: Single);

    // Input one sample to the filter and return one output.
    function Tick(const sample: Single): Single; overload; override;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function tick(vector: pSingle; vectorSize: longint): PSingle; overload;
  end;

implementation

constructor TStkTwoZero.Create(const SampleRate: Single);
var
  a: Single;
  b: array[0..2] of Single;
begin
  inherited Create(SampleRate);
  A := 1.0;
  B[0] := 1;
  B[1] := 0;
  B[2] := 0;
  inherited setCoefficients(3, @B, 1, @A);
end;

destructor TStkTwoZero.Destroy;
begin
  inherited Destroy;
end;

procedure TStkTwoZero.Clear;
begin
  inherited Clear;
end;

procedure TStkTwoZero.setB0(const Value: Single);
begin
  FB^[0] := Value;
end;

procedure TStkTwoZero.setB1(const Value: Single);
var
  Temp : PDAV2SingleArray;
begin
 Temp := @FB^;
 Temp^[1] := Value;
end;

procedure TStkTwoZero.setB2(const Value: Single);
var
  Temp : PDAV4SingleArray;
begin
 Temp := @FB^;
 Temp^[2] := Value;
end;

procedure TStkTwoZero.SetNotch(const Frequency, Radius: Single);
var
  Temp : PDAV4SingleArray;
begin
 Temp     := @FB^;
 Temp^[2] := Sqr(Radius);
 Temp^[1] := -2.0 * Radius * Cos(CTwoPI32 * Frequency / SampleRate);

  // Normalize the filter gain.
  if Temp^[1] > 0.0
   then Temp^[0] := 1.0 / (1.0 + Temp^[1] + Temp^[2])  // Maximum at z = 0.
   else Temp^[0] := 1.0 / (1.0 - Temp^[1] + Temp^[2]); // Maximum at z = -1.
  Temp^[1] := Temp^[1] * Temp^[0];
  Temp^[2] := Temp^[2] * Temp^[0];
end;

function TStkTwoZero.Tick(const Sample: Single): Single;
var
  p: pSingle;
begin
  inputs^ := gain * sample;

  outputs^ := index(b, 2) * index(inputs, 2) + index(b, 1) * index(inputs, 1) + b^ * inputs^;
  p := pindex(inputs, 2);
  p^ := index(inputs, 1);
  Dec(p);
  p^ := inputs^;

  Result := outputs^;
end;

function TStkTwoZero.tick(vector: PSingle; vectorSize: longint): PSingle;
var
  i: integer;
  p: pSingle;
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

