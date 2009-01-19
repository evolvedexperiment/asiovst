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
  DAV_StkCommon, DAV_StkFilter;

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
    procedure SetResonance(Frequency, Radius: Single; Normalize: Boolean = False);

    // Set the filter coefficients for a notch at \e Frequency (in Hz).
  {
    This method determines the filter coefficients corresponding to
    two complex-conjugate zeros with the given \e Frequency (in Hz)
    and \e Radius from the z-plane origin.  No filter normalization
    is attempted.
  }
    procedure SetNotch(Frequency, Radius: Single);

    // Sets the filter zeroes for equal resonance gain.
  {
    When using the filter as a resonator, zeroes places at z := 1, z
    := -1 will result in a constant gain at resonance of 1 / (1 - R),
    where R is the pole Radius setting.
  }
    procedure SetEqualGainZeroes;

    // Set the filter gain.
  {
    The gain is applied at the filter input and does not affect the
    coefficient values.  The default gain value is 1.0.
   }
    procedure SetGain(theGain: Single);

    // Return the current filter gain.
    function GetGain: Single;

    // Return the last computed output value.
    function lastOut: Single;

    // Input one sample to the filter and return one output.
    function Tick(sample: Single): Single; overload;

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
  b^ := b0;
end;

procedure TStkBiQuad.SetB1(b1: Single);
var
  p: PSingle;
begin
  p := pindex(b, 1);
  p^ := b1;
end;

procedure TStkBiQuad.setB2(b2: Single);
var
  p: PSingle;
begin
  p := pindex(b, 2);
  p^ := b2;
end;

procedure TStkBiQuad.setA1(a1: Single);
var
  p: PSingle;
begin
  p := pindex(a, 1);
  p^ := a1;
end;

procedure TStkBiQuad.setA2(a2: Single);
var
  p: PSingle;
begin
  p := pindex(a, 2);
  p^ := a2;
end;

procedure TStkBiQuad.SetResonance;
var
  p: PSingle;
begin
  p := pindex(a, 2);
  p^ := Radius * Radius;
  Dec(p);
  p^ := -2.0 * Radius * cos(TWO_PI * Frequency / srate);
  if (Normalize) then
   begin
   // Use zeros at +- 1 and Normalize the filter peak gain.
    p := b;
    p^ := 0.5 - 0.5 * index(a, 2);
    Inc(p);
    p^ := 0.0;
    Inc(p);
    p^ := -b^;
   end;
end;

procedure TStkBiQuad.SetNotch;
var
  p: PSingle;
begin
  p := pindex(b, 2);
 // This method does not attempt to Normalize the filter gain.
  p^ := Radius * Radius;
  Dec(p);
  p^ := -2.0 * Radius * cos(TWO_PI * Frequency / srate);
end;

procedure TStkBiQuad.SetEqualGainZeroes;
var
  p: PSingle;
begin
  p := b;
  p^ := 1.0;
  Inc(p);
  p^ := 0.0;
  Inc(p);
  p^ := -1.0;
end;

procedure TStkBiQuad.SetGain;
begin
  inherited SetGain(theGain);
end;

function TStkBiQuad.GetGain;
begin
  Result := inherited GetGain;
end;

function TStkBiQuad.lastOut;
begin
  Result := inherited lastOut;
end;

function TStkBiQuad.Tick(sample: Single): Single;
var
  p: PSingle;
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
