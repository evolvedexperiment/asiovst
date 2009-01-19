unit DAV_StkFilter;

// based on STK by Perry R. Cook and Gary P. Scavone, 1995 - 2002.

{  STK filter class.

   This class implements FA generic structure which can be used to create FA wide
   range of filters. It can function independently or be subclassed to provide
   more specific controls based on FA particular filter type.

   In particular, this class implements the standard difference equation:

   FA[0]*y[n] := FB[0]*x[n] + ... + FB[FnumB]*x[n-FnumB] -
               FA[1]*y[n-1] - ... - FA[FnumA]*y[n-FnumA]

   If FA[0] is not equal to 1, the filter coeffcients
   are normalized by FA[0].

   The \e FGain parameter is applied at the filter
   input and does not affect the coefficient values.
   The default FGain value is 1.0.  This structure
   results in one extra multiply per computed sample,
   but allows easy control of the overall filter FGain.
}

interface

{$I ..\DAV_Compiler.inc}

uses
  DAV_StkCommon;

type
  TStkFilter = class(TStk)
  protected
    FGain: Single;
    FnumB, FnumA: Integer;
    FA, FB, FOutputs, FInputs: PSingle;
  public
    // Default constructor creates FA zero-order pass-through "filter".
    constructor Create(SampleRate: Single); overload;

    // Overloaded constructor which takes filter coefficients.
  {
    An StkError can be thrown if either \e FnumB or \e FnumA is less than
    one, or if the FA[0] coefficient is equal to zero.
  }
    constructor Create
      (SampleRate: Single; nmb: Integer; bCoefficients: PSingle; nma: Integer;
      aCoefficients: PSingle); overload;

    // Class destructor.
    destructor Destroy;

    // Clears all internal states of the filter.
    procedure Clear;

    // Set filter coefficients.
  {
    An StkError can be thrown if either \e FnumB or \e FnumA is less than
    one, or if the FA[0] coefficient is equal to zero.  If FA[0] is not
    equal to 1, the filter coeffcients are normalized by FA[0].
  }
    procedure setCoefficients(nmb: Integer; bCoefficients: PSingle;
      nma: Integer; aCoefficients: PSingle);

    // Set numerator coefficients.
  {
    An StkError can be thrown if \e FnumB is less than one.  Any
    previously set denominator coefficients are left unaffected.
    Note that the default constructor sets the single denominator
    coefficient FA[0] to 1.0.
  }
    procedure setNumerator(nmb: Integer; bCoefficients: PSingle);

    // Set denominator coefficients.
  {
    An StkError can be thrown if \e FnumA is less than one or if the
    FA[0] coefficient is equal to zero.  Previously set numerator
    coefficients are unaffected unless FA[0] is not equal to 1, in
    which case all coeffcients are normalized by FA[0].  Note that the
    default constructor sets the single numerator coefficient FB[0]
    to 1.0.
  }
    procedure setDenominator(nma: Integer; aCoefficients: PSingle);

    // Set the filter FGain.
  {
    The FGain is applied at the filter input and does not affect the
    coefficient values.  The default FGain value is 1.0.
   }
    procedure setGain(theGain: Single);

    // Return the current filter FGain.
    function getGain: Single;

    // Return the last computed output value.
    function lastOut: Single;

    // Input one sample to the filter and return one output.
    function tick(sample: Single): Single; overload;

    // Input \e vectorSize samples to the filter and return an equal number of FOutputs in \e vector.
    function tick(vector: PSingle; vectorSize: longint): PSingle; overload;

  end;

implementation

{ TStkFilter }

procedure TStkFilter.Clear;
var
  i: Integer;
  p: PSingle;
begin
  for i := 0 to FnumB - 1 do
   begin
    p := PSingle(longint(FInputs) + i * sizeof(PSingle));
    p^ := 0;
   end;
  for i := 0 to FnumA - 1 do
   begin
    p := PSingle(longint(FOutputs) + i * sizeof(PSingle));
    p^ := 0;
   end;
end;

constructor TStkFilter.Create(SampleRate: Single);
begin
  inherited Create(SampleRate);
   // The default constructor should setup for pass-through.
  FGain := 1.0;
  FnumB := 1;
  FnumA := 1;
  getmem(FB, FnumB * sizeof(Single));
  FB^ := 1.0;
  getmem(FA, FnumA * sizeof(Single));
  FA^ := 1.0;

  getmem(FInputs, FnumB * sizeof(Single));
  getmem(FOutputs, FnumA * sizeof(Single));
  Clear;
end;

constructor TStkFilter.Create(SampleRate: Single; nmb: Integer;
  bCoefficients: PSingle; nma: Integer; aCoefficients: PSingle);
begin
  inherited Create(SampleRate);
  // Check the arguments.
  if (aCoefficients^ = 0) or (FnumB < 1) or (FnumA < 1) then
    exit;

  FGain := 1.0;
  FnumB := nmb;
  FnumA := nma;
  getmem(FB, FnumB * sizeof(Single));
  getmem(FA, FnumA * sizeof(Single));
  getmem(FInputs, FnumB * sizeof(Single));
  getmem(FOutputs, FnumA * sizeof(Single));
  Clear;
  setCoefficients(FnumB, bCoefficients, FnumA, aCoefficients);
end;

destructor TStkFilter.Destroy;
begin
  inherited Destroy;
  freemem(FB);
  freemem(FA);
  freemem(FInputs);
  freemem(FOutputs);
end;

function TStkFilter.getGain: Single;
begin
  Result := FGain;
end;

function TStkFilter.lastOut: Single;
begin
  Result := FOutputs^;
end;

procedure TStkFilter.setCoefficients(nmb: Integer; bCoefficients: PSingle;
  nma: Integer; aCoefficients: PSingle);
var
  i: Integer;
  p, q: PSingle;
begin
  // Check the arguments.
  if (aCoefficients^ = 0.0) or (FnumB < 1) or (FnumA < 1) then
    exit;

  if (nmb <> FnumB) then
   begin
    freemem(FB);
    freemem(FInputs);
    FnumB := nmb;
    getmem(FB, FnumB * sizeof(Single));
    getmem(FInputs, FnumB * sizeof(Single));
    for i := 0 to FnumB - 1 do
     begin
      p := PSingle(longint(FInputs) + i * 4);
      p^ := 0;
     end;
   end;

  if (FnumA <> nmA) then
   begin
    freemem(FA);
    freemem(FOutputs);
    FnumA := nma;
    getmem(FA, FnumA * sizeof(Single));
    getmem(FOutputs, FnumA * sizeof(Single));
    for i := 0 to FnumB - 1 do
     begin
      p := PSingle(longint(FOutputs) + i * sizeof(Single));
      p^ := 0;
     end;
   end;

  p := FB;
  q := bCoefficients;
  for i := 0 to FnumB - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;

  p := FA;
  q := aCoefficients;
  for i := 0 to FnumA - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;

  // scale coefficients by FA[0] if necessary
  if (FA^ <> 1.0) then
   begin
    p := FA;
    for i := 0 to FnumA - 1 do
     begin
      p^ := p^ / FA^;
      Inc(p);
     end;
    p := FB;
    for i := 0 to FnumB - 1 do
     begin
      p^ := p^ / FA^;
      Inc(p);
     end;
   end;

end;

procedure TStkFilter.setDenominator(nma: Integer; aCoefficients: PSingle);
var
  i: Integer;
  p, q: PSingle;
begin
  // Check the arguments.
  if (FnumA < 1) or (aCoefficients^ = 0.0) then
    exit;

  if (FnumA <> FnumA) then
   begin
    freemem(FA);
    freemem(FOutputs);
    FnumA := nma;

    getmem(FA, FnumA * sizeof(Single));
    getmem(FOutputs, FnumA * sizeof(Single));
    for i := 0 to FnumA - 1 do
     begin
      p := PSingle(longint(FOutputs) + i * sizeof(PSingle));
      p^ := 0;
     end;
   end;

  p := FA;
  q := aCoefficients;
  for i := 0 to FnumA - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;

  // scale coefficients by FA[0] if necessary
  if (FA^ <> 1.0) then
   begin
    p := FA;
    for i := 0 to FnumA - 1 do
     begin
      p^ := p^ / FA^;
      Inc(p);
     end;
    p := FB;
    for i := 0 to FnumB - 1 do
     begin
      p^ := p^ / FA^;
      Inc(p);
     end;
   end;
end;

procedure TStkFilter.setGain(theGain: Single);
begin
  FGain := theGain;
end;

procedure TStkFilter.setNumerator(nmb: Integer; bCoefficients: PSingle);
var
  i: Integer;
  p, q: PSingle;
begin

  // Check the arguments.
  if (FnumB < 1) then
    exit;

  if (FnumB <> FnumB) then
   begin
    freemem(FB);
    freemem(FInputs);
    FnumB := nmb;
    getmem(FB, FnumB * sizeof(Single));
    getmem(FInputs, FnumB * sizeof(Single));
    for i := 0 to FnumB - 1 do
     begin
      p := PSingle(longint(FInputs) + i * 4);
      p^ := 0;
     end;
   end;

  p := FB;
  q := bCoefficients;
  for i := 0 to FnumB - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;
end;

function TStkFilter.tick(vector: PSingle; vectorSize: longint): PSingle;
var
  i: Integer;
  p: PSingle;
begin
  p := vector;
  for i := 0 to vectorSize - 1 do
   begin
    p^ := tick(p^);
    Inc(p);
   end;
  Result := vector;
end;

function TStkFilter.tick(sample: Single): Single;
var
  i: Integer;
  p, q: PSingle;
begin
  FOutputs^ := 0.0;
  FInputs^ := FGain * sample;
  for i := FnumB - 1 downto 1 do
   begin
    p := PSingle(longint(FB) + sizeof(Single) * i);
    q := PSingle(longint(FInputs) + sizeof(Single) * i);
    FOutputs^ := FOutputs^ + p^ * q^;
    p := PSingle(longint(FInputs) + sizeof(Single) * (i - 1));
    q^ := p^;
   end;
  FOutputs^ := FOutputs^ + FB^ * FInputs^;

  for i := FnumA - 1 downto 1 do
   begin
    p := PSingle(longint(FA) + sizeof(Single) * i);
    q := PSingle(longint(FOutputs) + sizeof(Single) * i);
    FOutputs^ := FOutputs^ - p^ * q^;
    p := PSingle(longint(FOutputs) + sizeof(Single) * (i - 1));
    q^ := p^;
   end;
  Result := FOutputs^;
end;

end.
