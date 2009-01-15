unit DAV_Approximations;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, {$IFDEF FPC} LCLIntf; {$DEFINE PUREPASCAL}{$ELSE}
  Windows {$IFDEF UseNativeTypes}, Types{$ENDIF};{$ENDIF}

  {$IFNDEF FPC}
  function FastExp(const Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}

  function FastRoot(i: Single; n: Integer): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastIntPower(i: Single; n: Integer): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastPower(base, exp: Double) : Double; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2Laurent(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2Continous5(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2MinError5(Value: Single): Single; {$IFDEF useinlining} inline; {$ENDIF}
  function FastLog2(const Value: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function FastFloorln2(const Value: Single): Integer; {$IFDEF useinlining} inline; {$ENDIF}
  function FastArctan(const Value: Single): Single; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function FastArctan(const Value: Double): Double; overload; {$IFDEF useinlining} inline; {$ENDIF}
  function FastSin(const Value: Single): Single; overload;
  function FastSin(const Value: Double): Double; overload;
  function FastCos(const Value: Single): Single; overload;
  function FastCos(const Value: Double): Double; overload;
  function FastArcTan2(const Y, X: Extended): Extended;
  function FastTan(const Value: Extended): Extended;
  function FastCoTan(const Value: Extended): Extended;
  function FastLog10(const Value: Extended): Extended;
  {$ENDIF}

  function FastTanhOpt3(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt3(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  function FastTanhOpt3asm(Input: Single): Single; assembler; overload;
  function FastTanhOpt4asm(Input: Single): Single; assembler; overload;
  function FastTanhOpt5asm(Input: Single): Single; assembler; overload;
  function FastTanhOpt6asm(Input: Single): Single; assembler; overload;
  function FastTanhOpt7asm(Input: Single): Single; assembler; overload;
  function FastTanhOpt3asm(Input: Double): Double; assembler; overload;
  function FastTanhOpt4asm(Input: Double): Double; assembler; overload;
  function FastTanhOpt5asm(Input: Double): Double; assembler; overload;
  function FastTanhOpt6asm(Input: Double): Double; assembler; overload;
  function FastTanhOpt7asm(Input: Double): Double; assembler; overload;

  function Tanh2a(const Input: Single): Single;
  function Tanh2b(const Input: Single): Single;
  function Tanh2c(const Input: Single): Single;
  function Tanh2d(const Input: Single): Single;

var
  ln10, ln2, ln22, ln2Rez : Double;

const
  CMinusOneThird : Double = -1/3;
  CTwo           : Double = 2;
  CMinusTwoThird : Double = -2/3;
  CDenorm32      : Single = 1E-24;
  CDenorm64      : Double = 1E-34;
  CTwoPI         : Double = 2 * Pi;
  CFourPI        : Double = 4 * Pi;
  CHalf32        : Single = 0.5;
  CHalf64        : Double = 0.5;
  CTwenty64      : Double = 20;

const
  kMaxLong  = $7FFFFFFF;
  kMinLong  = -$7FFFFFFF-1;
  kMaxInt64 = 9223372036854775807;
  kMinInt64 = -9223372036854775807-1;
  kMaxFloat = 3.40282346638528860e+38;

implementation

uses
  Math, SysUtils;

{$IFNDEF FPC}
{$WARNINGS OFF}
function FastArcTan2(const Y, X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result := ArcTan2(Y,X);
{$ELSE}
asm
 fld Y
 fld X
 fpatan
{$ENDIF}
end;

function FastTan(const Value: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result := Tan(X);
{$ELSE}
asm
 fld Value
 fptan
 fstp st(0)
{$ENDIF}
end;

function FastCoTan(const Value: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result := CoTan(X);
{$ELSE}
asm
 fld Value
 fptan
 fdivrp
{$ENDIF}
end;

function FastLog10(const Value: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 result := Log10(X);
{$ELSE}
asm
 fldlg2
 fld Value
 fyl2x
{$ENDIF}
end;
{$ENDIF}

function FastExp(const Value: Single): Single;
begin
 Result := Exp(Value * ln2);
end;

function FastSin(const Value: Single): Single;
const
  sin1 : Double = 7.61e-03;
  sin2 : Double = -1.6605e-01;
{$IFDEF PUREPASCAL}
var Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * sin1) * Asqr + sin2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld Value.Single
 fmul Value.Single
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fld1
 faddp
 fmul Value
{$ENDIF}
end;

function FastSin(const Value: Double): Double;
const
  sin1 : Double = 7.61e-03;
  sin2 : Double = -1.6605e-01;
{$IFDEF PUREPASCAL}
var Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * sin1) * Asqr + sin2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld Value.Double
 fmul Value.Double
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fld1
 faddp
 fmul Value
{$ENDIF}
end;

function FastCos(const Value: Single): Single;
const
  sin1 : Double =  3.705e-02;
  sin2 : Double = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * sin1) * Asqr + sin2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld Value.Single
 fmul Value.Single
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fld1
 faddp
 fmul Value
{$ENDIF}
end;

function FastCos(const Value: Double): Double;
const
  sin1 : Double =  3.705e-02;
  sin2 : Double = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * sin1) * Asqr + sin2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld Value.Double
 fmul Value.Double
 fld sin1.Double
 fmul st(0),st(1)
 fld sin2.Double
 faddp st(1),st(0)
 fmulp st(1),st(0)
 fld1
 faddp
 fmul Value
{$ENDIF}
end;

function FastArcTan(const Value: Single): Single;
var
  VSqr : Double;
begin
 VSqr   := sqr(Value);
 Result := ((((0.0208351 * VSqr - 0.085133) * VSqr + 0.180141) * VSqr - 0.3302995) * VSqr + 0.999866) * Value;
end;

function FastArcTan(const Value: Double): Double;
var
  VSqr : Double;
begin
 VSqr   := sqr(Value);
 Result := ((((0.0208351 * VSqr - 0.085133) * VSqr + 0.180141) * VSqr - 0.3302995) * VSqr + 0.999866) * Value;
end;

function FastFloorLn2(const Value: Single): Integer;
begin
 Result := (((Integer((@Value)^) and $7F800000) shr 23) - $7F);
end;

function FastLog2(const Value: Single): Single;
begin
 Result := (((Integer((@Value)^) and $7F800000) shr 23) - $7F) +
             (Integer((@Value)^) and $007FFFFF) / $800000;
end;

function FastLog2Laurent(Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Value;
begin
 log2   := ((x shr 23) and $FF) - $80;
 x      := x and (not ($FF shl 23)) + $7F shl 23;
 Value  := ((CMinusOneThird * Value) + CTwo) * Value + CMinusTwoThird;
 Result := Value + log2;
end;

function FastLog2MinError5(Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Value;
begin
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Value := ((( - 8.18038640187952054E-2 *
          Value + 6.46216635143615381E-1) *
          Value - 2.12293700635511007) *
          Value + 4.07217052527789480) *
          Value - 1.51355930430330177;
 Result := Value + log2;
end;

function FastLog2Continous5(Value: Single): Single;
var
  log2 : Integer;
  x    : Integer absolute Value;
begin
 log2 := ((x shr 23) and $FF) - $80;
 x := x and (not ($FF shl 23)) + $7F shl 23;
 Value := ((( - 8.21343513178931783E-2 *
          Value + 6.49732456739820052E-1) *
          Value - 2.13417801862571777) *
          Value + 4.08642207062728868) *
          Value - 1.51984215742349793;
 Result := Value + log2;
end;

function FastIntPower(i: Single; n: Integer): Single;
var
  l : Integer;
begin
 l := Integer((@i)^);
 l := (l - $3F800000) shl (n-1) + $3F800000;
 Result:=Single((@l)^);
end;

function FastPower(base, exp : Double): Double;
begin
 Result := Power(base, exp);
end;

function FastRoot(i: Single; n: Integer): Single;
var
  l : Integer;
begin
 l := Integer((@i)^);
 l := (l - $3F800000) shr (n-1) + $3F800000;
 Result:=Single((@l)^);
end;

function Tanh2a(const Input: Single): Single;
var
  a, b: Single;
begin
 a := abs(Input);
 b := 12 + a * (6 + a * (3 + a));
 Result := (Input * b) / (a * b + 24);
end;

function Tanh2b(const Input: Single): Single;
var
  a, b: Single;
begin
 a := abs(Input);
 b := (6 + a * (3 + a));
 Result := (Input * b) / (a * b + 12);
end;

function Tanh2c(const Input: Single): Single;
{$IFDEF PUREPASCAL}
var
  a, b: Single;
begin
 a := abs(Input);
 b := 3 + a;
 Result := (Input * b) / (a * b + 6 );
{$ELSE}
const
  c3: Single = 3;
  c6: Single = 6;
asm
 fld Input.Single;
 fabs
 fld c3
 fadd st(0),st(1)
 fld st(0)
 fmul Input.Single
 fxch st(2)
 fmulp
 fadd c6.Single
 fdiv
{$ENDIF}
end;

function Tanh2d(const Input: Single): Single;
{$IFDEF PUREPASCAL}
begin
 Result := Input / (abs(Input) + 3);
{$ELSE}
const c3 : Single = 3;
asm
 fld Input.Single;
 fld Input.Single;
 fabs
 fadd c3
 fdiv
{$ENDIF}
end;

function FastTanhOpt3(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt4(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.89690305801668457 + a *
     ( 1.89047619399687661 + a *
     (-1.35205169119085666 + a *
       1.74656303770202670));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt5(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt6(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.98516470896867081 + a *
     ( 1.21020234045009012 + a *
     (-0.22720155259481389 + a *
     ( 1.89719615102030725 + a *
     (-1.07161642656874956 + a *
     ( 0.40487405571569546)))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt7(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.00518193411912860 + a *
     ( 0.91005085146116016 + a *
     ( 1.14542500876429276 + a *
     (-0.76509890972158046 + a *
     ( 1.34808969964882519 + a *
     (-0.60147655894944263 + a *
     ( 0.15264109378548973))))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt3(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt4(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.89690305801668457 + a *
     ( 1.89047619399687661 + a *
     (-1.35205169119085666 + a *
       1.74656303770202670));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt5(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.03971379878158321 + a *
     ( 0.54953758170495126 + a *
     ( 2.13184139104070569 + a *
     (-1.46060069227128242 + a *
     ( 0.91996358346770157))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt6(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  0.98516470896867081 + a *
     ( 1.21020234045009012 + a *
     (-0.22720155259481389 + a *
     ( 1.89719615102030725 + a *
     (-1.07161642656874956 + a *
     ( 0.40487405571569546)))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt7(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b :=  1.00518193411912860 + a *
     ( 0.91005085146116016 + a *
     ( 1.14542500876429276 + a *
     (-0.76509890972158046 + a *
     ( 1.34808969964882519 + a *
     (-0.60147655894944263 + a *
     ( 0.15264109378548973))))));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt3asm(Input: Single): Single; assembler;
const
  c0 : Double =  2.66559097474027817;
  c1 : Double = -0.54699348440059470;
  c2 : Double =  1.26175667589988239;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: b := c2 + a * (c1 + a * c0), a, Input
 fXch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt4asm(Input: Single): Single; assembler;
const
  c0 : Double =  1.74656303770202670;
  c1 : Double = -1.35205169119085666;
  c2 : Double =  1.89047619399687661;
  c3 : Double =  0.89690305801668457;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt5asm(Input: Single): Single; assembler;
const
  c0 : Double =  0.91996358346770157;
  c1 : Double = -1.46060069227128242;
  c2 : Double =  2.13184139104070569;
  c3 : Double =  0.54953758170495126;
  c4 : Double =  1.03971379878158321;
asm
 fld Input.Single      // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt6asm(Input: Single): Single; assembler;
const
  c0 : Double =  0.40487405571569546;
  c1 : Double = -1.07161642656874956;
  c2 : Double =  1.89719615102030725;
  c3 : Double = -0.22720155259481389;
  c4 : Double =  1.21020234045009012;
  c5 : Double =  0.98516470896867081;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt7asm(Input: Single): Single; assembler;
const
  c0 : Double =  0.152641093785489734;
  c1 : Double = -0.60147655894944263;
  c2 : Double =  1.34808969964882519;
  c3 : Double = -0.765098909721580456;
  c4 : Double =  1.14542500876429276;
  c5 : Double =  0.91005085146116016;
  c6 : Double =  1.00518193411912860;
asm
 fld Input.Single  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fmul st(0), st(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fadd c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fxch st(2)        // exchange b and x, Stack: x, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt3asm(Input: Double): Double; assembler;
const
  c0 : Double =  2.66559097474027817;
  c1 : Double = -0.54699348440059470;
  c2 : Double =  1.26175667589988239;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: b := c2 + a * (c1 + a * c0), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt4asm(Input: Double): Double; assembler;
const
  c0 : Double =  1.74656303770202670;
  c1 : Double = -1.35205169119085666;
  c2 : Double =  1.89047619399687661;
  c3 : Double =  0.89690305801668457;
asm
 fld Input.Double      // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: b := c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt5asm(Input: Double): Double; assembler;
const
  c4 : Double =  0.91996358346770157;
  c3 : Double = -1.46060069227128242;
  c2 : Double =  2.13184139104070569;
  c1 : Double =  0.54953758170495126;
  c0 : Double =  1.03971379878158321;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt6asm(Input: Double): Double; assembler;
const
  c0 : Double =  0.40487405571569546;
  c1 : Double = -1.07161642656874956;
  c2 : Double =  1.89719615102030725;
  c3 : Double = -0.22720155259481389;
  c4 : Double =  1.21020234045009012;
  c5 : Double =  0.98516470896867081;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: b := c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

function FastTanhOpt7asm(Input: Double): Double; assembler;
const
  c0 : Double =  0.152641093785489734;
  c1 : Double = -0.60147655894944263;
  c2 : Double =  1.34808969964882519;
  c3 : Double = -0.765098909721580456;
  c4 : Double =  1.14542500876429276;
  c5 : Double =  0.91005085146116016;
  c6 : Double =  1.00518193411912860;
asm
 fld Input.Double  // Load Input
 fld st(0)         // Copy Input
 fabs              // Stack: abs(Input), Input
 fld c0            // Load c0 as working value, abs(Input) => a
 fmul st(0), st(1) // Stack: a * c0, a, Input
 fadd c1           // Stack: c1 + a * c0, a, Input
 fmul st(0), st(1) // Stack: a * (c1 + a * c0), a, Input
 fadd c2           // Stack: c2 + a * (c1 + a * c0), a, Input
 fmul st(0), st(1) // Stack: a * (c2 + a * (c1 + a * c0)), a, Input
 fadd c3           // Stack: c3 + a * (c2 + a * (c1 + a * c0)), a, Input
 fmul st(0), st(1) // Stack: a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fadd c4           // Stack: b := c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))), a, Input
 fmul st(0), st(1) // Stack: a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fadd c5           // Stack: c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0)))), a, Input
 fmul st(0), st(1) // Stack: a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fadd c6           // Stack: b := c6 + a * (c5 + a * (c4 + a * (c3 + a * (c2 + a * (c1 + a * c0))))), a, Input
 fxch st(2)        // exchange b and Input, Stack: Input, a, b
 fmul st(0), st(2) // Stack: b * Input, a, b
 fxch st(2)        // exchange b * Input and Input, Stack: b, a, b * Input
 fmulp             // Stack: b * a, b * Input
 fld1              // Stack: 1, b * a, b * Input
 faddp             // Stack: 1 + b * a, b * Input
 fdivp             // Stack: (b * Input) / (1 + b * a)
end;

procedure InitConstants;
{$IFDEF PUREPASCAL}
begin
 ln2    := ln(2);
 ln22   := ln2 * CHalf32;
 ln2Rez := 1 / ln2;
 ln10   := ln(10);
end;
{$ELSE}
begin
 ln2    := ln(2); // ToDo: use ASM here!
 ln22   := ln2 * CHalf32;
 ln2Rez := 1 / ln2;
 ln10   := ln(10);
end;
{$ENDIF}

initialization
 InitConstants;

end.
