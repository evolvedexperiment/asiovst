unit DAV_StkFilter;

interface

uses
  DAV_StkCommon;

{
/***************************************************/
/*! \class Filter
    \brief STK filter class.

    This class implements a generic structure which
    can be used to create a wide range of filters.
    It can function independently or be subclassed
    to provide more specific controls based on a
    particular filter type.

    In particular, this class implements the standard
    difference equation:

    a[0]*y[n] := b[0]*x[n] + ... + b[nb]*x[n-nb] -
                a[1]*y[n-1] - ... - a[na]*y[n-na]

    If a[0] is not equal to 1, the filter coeffcients
    are normalized by a[0].

    The \e gain parameter is applied at the filter
    input and does not affect the coefficient values.
    The default gain value is 1.0.  This structure
    results in one extra multiply per computed sample,
    but allows easy control of the overall filter gain.

    by Perry R. Cook and Gary P. Scavone, 1995 - 2002.
*/
/***************************************************/
}

type
  TFilter = class(TStk)
  public
  //! Default constructor creates a zero-order pass-through "filter".
    constructor Create(sr: my_float); overload;

  //! Overloaded constructor which takes filter coefficients.
  {
    An StkError can be thrown if either \e nb or \e na is less than
    one, or if the a[0] coefficient is equal to zero.
  }
    constructor Create
      (sr: my_float; nmb: integer; bCoefficients: PMY_FLOAT; nma: integer;
      aCoefficients: PMY_FLOAT); overload;

  //! Class destructor.
    destructor Destroy;

  //! Clears all internal states of the filter.
    procedure Clear;

  //! Set filter coefficients.
  {
    An StkError can be thrown if either \e nb or \e na is less than
    one, or if the a[0] coefficient is equal to zero.  If a[0] is not
    equal to 1, the filter coeffcients are normalized by a[0].
  }
    procedure setCoefficients(nmb: integer; bCoefficients: PMY_FLOAT;
      nma: integer; aCoefficients: PMY_FLOAT);

  //! Set numerator coefficients.
  {
    An StkError can be thrown if \e nb is less than one.  Any
    previously set denominator coefficients are left unaffected.
    Note that the default constructor sets the single denominator
    coefficient a[0] to 1.0.
  }
    procedure setNumerator(nmb: integer; bCoefficients: PMY_FLOAT);

  //! Set denominator coefficients.
  {
    An StkError can be thrown if \e na is less than one or if the
    a[0] coefficient is equal to zero.  Previously set numerator
    coefficients are unaffected unless a[0] is not equal to 1, in
    which case all coeffcients are normalized by a[0].  Note that the
    default constructor sets the single numerator coefficient b[0]
    to 1.0.
  }
    procedure setDenominator(nma: integer; aCoefficients: PMY_FLOAT);

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

  protected
    gain: MY_FLOAT;
    nB, nA: integer;
    a, b, outputs, inputs: PMY_FLOAT;
  end;

implementation

{ TFilter }

procedure TFilter.Clear;
var
  i: integer;
  p: pmy_float;
begin
  for i := 0 to nB - 1 do
   begin
    p := pmy_float(longint(inputs) + i * sizeof(pmy_float));
    p^ := 0;
   end;
  for i := 0 to nA - 1 do
   begin
    p := pmy_float(longint(outputs) + i * sizeof(pmy_float));
    p^ := 0;
   end;
end;

constructor TFilter.Create(sr: my_float);
begin
  inherited Create(sr);
   // The default constructor should setup for pass-through.
  gain := 1.0;
  nB := 1;
  nA := 1;
  getmem(b, nb * sizeof(my_float));
  b^ := 1.0;
  getmem(a, na * sizeof(my_float));
  a^ := 1.0;

  getmem(inputs, nb * sizeof(my_float));
  getmem(outputs, na * sizeof(my_float));
  Clear;
end;

constructor TFilter.Create(sr: my_float; nmb: integer;
  bCoefficients: PMY_FLOAT; nma: integer; aCoefficients: PMY_FLOAT);
begin
  inherited Create(sr);
  // Check the arguments.
  if (aCoefficients^ = 0) or (nb < 1) or (na < 1) then
    exit;

  gain := 1.0;
  nB := nmb;
  nA := nma;
  getmem(b, nb * sizeof(my_float));
  getmem(a, na * sizeof(my_float));
  getmem(inputs, nb * sizeof(my_float));
  getmem(outputs, na * sizeof(my_float));
  Clear;
  setCoefficients(nB, bCoefficients, nA, aCoefficients);
end;

destructor TFilter.Destroy;
begin
  inherited Destroy;
  freemem(b);
  freemem(a);
  freemem(inputs);
  freemem(outputs);
end;

function TFilter.getGain: MY_FLOAT;
begin
  Result := gain;
end;

function TFilter.lastOut: MY_FLOAT;
begin
  Result := outputs^;
end;

procedure TFilter.setCoefficients(nmb: integer; bCoefficients: PMY_FLOAT;
  nma: integer; aCoefficients: PMY_FLOAT);
var
  i: integer;
  p, q: pmy_float;
begin
  // Check the arguments.
  if (aCoefficients^ = 0.0) or (nb < 1) or (na < 1) then
    exit;

  if (nmb <> nB) then
   begin
    freemem(b);
    freemem(inputs);
    nB := nmb;
    getmem(b, nb * sizeof(my_float));
    getmem(inputs, nb * sizeof(my_float));
    for i := 0 to nB - 1 do
     begin
      p := pmy_float(longint(inputs) + i * 4);
      p^ := 0;
     end;
   end;

  if (na <> nmA) then
   begin
    freemem(a);
    freemem(outputs);
    na := nma;
    getmem(a, na * sizeof(my_float));
    getmem(outputs, na * sizeof(my_float));
    for i := 0 to nB - 1 do
     begin
      p := pmy_float(longint(outputs) + i * sizeof(my_float));
      p^ := 0;
     end;
   end;

  p := b;
  q := bCoefficients;
  for i := 0 to nB - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;

  p := a;
  q := aCoefficients;
  for i := 0 to nA - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;

  // scale coefficients by a[0] if necessary
  if (a^ <> 1.0) then
   begin
    p := a;
    for i := 0 to nA - 1 do
     begin
      p^ := p^ / a^;
      Inc(p);
     end;
    p := b;
    for i := 0 to nB - 1 do
     begin
      p^ := p^ / a^;
      Inc(p);
     end;
   end;

end;

procedure TFilter.setDenominator(nma: integer; aCoefficients: PMY_FLOAT);
var
  i: integer;
  p, q: pmy_float;
begin
  // Check the arguments.
  if (na < 1) or (aCoefficients^ = 0.0) then
    exit;

  if (na <> nA) then
   begin
    freemem(a);
    freemem(outputs);
    nA := nma;

    getmem(a, na * sizeof(my_float));
    getmem(outputs, na * sizeof(my_float));
    for i := 0 to nA - 1 do
     begin
      p := pmy_float(longint(outputs) + i * sizeof(pmy_float));
      p^ := 0;
     end;
   end;

  p := a;
  q := aCoefficients;
  for i := 0 to nA - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;

  // scale coefficients by a[0] if necessary
  if (a^ <> 1.0) then
   begin
    p := a;
    for i := 0 to nA - 1 do
     begin
      p^ := p^ / a^;
      Inc(p);
     end;
    p := b;
    for i := 0 to nB - 1 do
     begin
      p^ := p^ / a^;
      Inc(p);
     end;
   end;
end;

procedure TFilter.setGain(theGain: MY_FLOAT);
begin
  gain := theGain;
end;

procedure TFilter.setNumerator(nmb: integer; bCoefficients: PMY_FLOAT);
var
  i: integer;
  p, q: pmy_float;
begin

  // Check the arguments.
  if (nb < 1) then
    exit;

  if (nb <> nB) then
   begin
    freemem(b);
    freemem(inputs);
    nB := nmb;
    getmem(b, nb * sizeof(my_float));
    getmem(inputs, nb * sizeof(my_float));
    for i := 0 to nB - 1 do
     begin
      p := pmy_float(longint(inputs) + i * 4);
      p^ := 0;
     end;
   end;

  p := b;
  q := bCoefficients;
  for i := 0 to nB - 1 do
   begin
    p^ := q^;
    Inc(p);
    Inc(q);
   end;
end;

function TFilter.tick(vector: PMY_FLOAT; vectorSize: longint): PMY_FLOAT;
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

function TFilter.tick(sample: MY_FLOAT): MY_FLOAT;
var
  i: integer;
  p, q: pmy_float;
begin
  outputs^ := 0.0;
  inputs^ := gain * sample;
  for i := nB - 1 downto 1 do
   begin
    p := pmy_float(longint(b) + sizeof(my_float) * i);
    q := pmy_float(longint(inputs) + sizeof(my_float) * i);
    outputs^ := outputs^ + p^ * q^;
    p := pmy_float(longint(inputs) + sizeof(my_float) * (i - 1));
    q^ := p^;
   end;
  outputs^ := outputs^ + b^ * inputs^;

  for i := nA - 1 downto 1 do
   begin
    p := pmy_float(longint(a) + sizeof(my_float) * i);
    q := pmy_float(longint(outputs) + sizeof(my_float) * i);
    outputs^ := outputs^ - p^ * q^;
    p := pmy_float(longint(outputs) + sizeof(my_float) * (i - 1));
    q^ := p^;
   end;
  Result := outputs^;
end;

end.
