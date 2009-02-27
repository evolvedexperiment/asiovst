unit DAV_StkOneZero;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{ STK one-zero filter class.

  This protected Filter subclass implements a one-zero digital filter. A method
  is provided for setting the zero position along the real axis of the z-plane
  while maintaining a constant filter gain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_Common, DAV_StkCommon, DAV_StkFilter;

type
  TStkOneZero = class(TStkFilter)
  public
    // Default constructor creates a first-order low-pass filter.
    constructor Create(const SampleRate: Single); overload; override;

    // Overloaded constructor which sets the pole position during instantiation.
    constructor Create(const SampleRate, theZero: Single); overload; virtual;

    // Class destructor.
    destructor Destroy; override;

    // Clears the internal state of the filter.
    procedure Clear; override; 

    // Set the b[0] coefficient value.
    procedure setB0(b0: Single);

    // Set the b[1] coefficient value.
    procedure setB1(b1: Single);

    // Set the zero position in the z-plane.
  {
    This method sets the zero position along the real-axis of the
    z-plane and normalizes the coefficients for a maximum gain of one.
    A positive zero value produces a high-pass filter, while a
    negative zero value produces a low-pass filter.  This method does
    not affect the filter \e gain value.
  }
    procedure setZero(theZero: Single);

    // Input one sample to the filter and return one output.
    function Tick(const Sample: Single): Single; overload; override;

    // Input \e vectorSize samples to the filter and return an equal number of outputs in \e vector.
    procedure Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer); overload;
  end;

implementation

constructor TStkOneZero.Create(const SampleRate: Single);
var
  b: array[0..1] of Single;
  a: Single;
begin
  inherited Create(SampleRate);
  A := 1.0;
  B[0] := 0.5;
  B[1] := 0.5;
  inherited setCoefficients(2, @B, 1, @A);
end;

constructor TStkOneZero.Create(const SampleRate, theZero: Single);
var
  b: array[0..1] of Single;
  a: Single;
begin
  inherited Create(SampleRate);
  A := 1.0;
  // Normalize coefficients for unity gain.
  if (theZero > 0.0) then
    B[0] := 1.0 / (1.0 + theZero)
  else
    B[0] := 1.0 / (1.0 - theZero);

  B[1] := -theZero * B[0];
  inherited setCoefficients(2, @B, 1, @A);
end;

destructor TStkOneZero.Destroy;
begin
  inherited Destroy;
end;

procedure TStkOneZero.Clear;
begin
  inherited Clear;
end;

procedure TStkOneZero.setB0(b0: Single);
begin
 FB^[0] := b0;
end;

procedure TStkOneZero.setB1(b1: Single);
begin
 PDAV4SingleArray(FB)^[1] := b1;
end;

procedure TStkOneZero.setZero;
begin
  // Normalize coefficients for unity gain.
  if (theZero > 0.0)
   then FB^[0] := 1.0 / (1.0 + theZero)
   else FB^[0] := 1.0 / (1.0 - theZero);
  PDAV4SingleArray(FB)^[1] := -theZero * FB^[0];
end;

function TStkOneZero.Tick(const sample: Single): Single;
begin
  FInputs^[0] := Gain * sample;

  FOutputs^[0] := PDAV4SingleArray(FB)^[1] * PDAV4SingleArray(FInputs)^[1] +
    FB^[0] * FInputs^[0];
  PDAV4SingleArray(FInputs)^[1] := FInputs^[0];

  Result := FOutputs^[0];
end;

procedure TStkOneZero.Tick(const Data: PDAVSingleFixedArray; const SampleFrames: Integer);
var
  Sample: integer;
begin
  for Sample := 0 to SampleFrames - 1
   do Data^[Sample] := Tick(Data^[Sample]);
end;

end.

