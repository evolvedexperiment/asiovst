unit DAV_StkBiQuad;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK TStkBiQuad (two-pole, two-zero) filter class.

  This protected Filter subclass implements a two-pole, two-zero digital
  filter. A method is provided for creating a resonance in the frequency
  response while maintaining a constant filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkBiQuad = class(TStkFilter)
  public
    // Default constructor creates a second-order pass-through filter.
    constructor Create(SampleRate: Single);

    // Class destructor.
    destructor Destroy;

    // Clears all internal states of the filter.
    procedure Clear;

    // Set the b[0] coefficient value.
    procedure SetB0(b0: Single);

    // Set the b[1] coefficient value.
    procedure SetB1(b1: Single);

    // Set the b[2] coefficient value.
    procedure setB2(b2: Single);

    // Set the a[1] coefficient value.
    procedure setA1(a1: Single);

    // Set the a[2] coefficient value.
    procedure setA2(a2: Single);

    // Sets the filter coefficients for a resonance at \e Frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate poles with the given \e Frequency (in Hz)
    and \e Radius from the z-plane origin.  If \e Normalize is true,
    the filter zeros are placed at z := 1, z := -1, and the coefficients
    are then normalized to produce a constant unity peak gain
    (independent of the filter \e gain parameter).  The resulting
    filter Frequency response has a resonance at the given \e
    Frequency.  The closer the poles are to the unit-circle (\e Radius
    close to one), the narrower the resulting resonance width.
  }
    procedure SetResonance(const Frequency, Radius: Single; const Normalize: Boolean = False);

    // Set the filter coefficients for a notch at \e Frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate zeros with the given \e Frequency (in Hz)
    and \e Radius from the z-plane origin.  No filter normalization
    is attempted.
  }
    procedure SetNotch(const Frequency, Radius: Single);

    // Sets the filter zeroes for equal resonance gain.
  {
    When using the filter as a resonator, zeroes places at z := 1, z
    := -1 will result in a constant gain at resonance of 1 / (1 - R),
    where R is the pole Radius setting.
  }
    procedure SetEqualGainZeroes;

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    function Tick(vector: PSingle; vectorSize: longint): PSingle; overload;
  end;

implementation

constructor TStkBiQuad.Create;
var
  a, b: array[0..2] of Single;
begin
  inherited Create(SampleRate);
  b[0] := 1;
  b[1] := 0;
  b[2] := 0;
  a[0] := 1;
  a[1] := 0;
  a[2] := 0;
  inherited setCoefficients(3, @B, 3, @A);
end;

destructor TStkBiQuad.Destroy;
begin
  inherited Destroy;
end;

procedure TStkBiQuad.Clear;
begin
  inherited Clear;
end;

procedure TStkBiQuad.SetB0(b0: Single);
begin
  FB^[0] := b0;
end;

procedure TStkBiQuad.SetB1(b1: Single);
begin
 PDav4SingleArray(FB)^[1] := b1;
end;

procedure TStkBiQuad.setB2(b2: Single);
begin
 PDav4SingleArray(FB)^[2] := b2;
end;

procedure TStkBiQuad.SetA1(a1: Single);
begin
 PDav4SingleArray(FA)^[1] := a1;
end;

procedure TStkBiQuad.setA2(a2: Single);
begin
 PDav4SingleArray(FA)^[2] := a2;
end;

procedure TStkBiQuad.SetResonance(const Frequency, Radius: Single; const Normalize: Boolean = False);
begin
  PDav4SingleArray(FA)^[2] := Radius * Radius;
  PDav4SingleArray(FA)^[1] := -2.0 * Radius * cos(2 * Pi * Frequency * FSampleRateInv);
  if (Normalize) then
   begin
    // Use zeros at +- 1 and Normalize the filter peak gain.
    PDav4SingleArray(FB)^[0] := 0.5 - 0.5 * PDav4SingleArray(FA)^[2];
    PDav4SingleArray(FB)^[1] := 0.0;
    PDav4SingleArray(FB)^[2] := -PDav4SingleArray(FB)^[0];
   end;
end;

procedure TStkBiQuad.SetNotch(const Frequency, Radius: Single);
begin
  // This method does not attempt to Normalize the filter gain.
  PDav4SingleArray(FA)^[2] := Radius * Radius;
  PDav4SingleArray(FA)^[1] := -2.0 * Radius * cos(2 * Pi * Frequency * FSampleRateInv);
end;

procedure TStkBiQuad.SetEqualGainZeroes;
begin
  PDav4SingleArray(FB)^[0] := 1.0;
  PDav4SingleArray(FB)^[1] := 0.0;
  PDav4SingleArray(FB)^[2] := -1.0;
end;

function TStkBiQuad.Tick(const Sample: Single): Single;
var
  p: PSingle;
begin
  FInputs^[0] := FGain * Sample;
  FOutputs^[0] := FB^[0] * FInputs^[0] +
    PDav4SingleArray(FB)^[1] * PDav4SingleArray(FInputs)^[1] +
    PDav4SingleArray(FB)^[2] * PDav4SingleArray(FInputs)^[2];
  FOutputs^[0] := FOutputs^[0] - (index(a, 2) * index(outputs, 2) +
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

function TStkBiQuad.Tick(vector: PSingle; vectorSize: longint): PSingle;
var
  i: integer;
  p: PSingle;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := Tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

end.
