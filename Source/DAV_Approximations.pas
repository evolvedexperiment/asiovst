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

  { Trigonomic Approximations }

  // 3-Term: Accurate to about 3.2 decimal digits over the range [0, pi/2].
  function FastCosPart3Term(const Value: Single): Single; overload;
  function FastCosPart3Term(const Value: Double): Double; overload;
  function FastCos3Term(const Value: Single): Single; overload;
  function FastCos3Term(const Value: Double): Double; overload;
  function FastSin3Term(const Value: Single): Single; overload;
  function FastSin3Term(const Value: Double): Double; overload; 

  // 4-Term: Accurate to about 5.2 decimal digits over the range [0, pi/2].
  function FastCosPart4Term(const Value: Single): Single; overload;
  function FastCosPart4Term(const Value: Double): Double; overload;
  function FastCos4Term(const Value: Single): Single; overload;
  function FastCos4Term(const Value: Double): Double; overload;
  function FastSin4Term(const Value: Single): Single; overload;
  function FastSin4Term(const Value: Double): Double; overload;

  // 5-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].
  function FastCosPart5Term(const Value: Single): Single; overload;
  function FastCosPart5Term(const Value: Double): Double; overload;
  function FastCos5Term(const Value: Single): Single; overload;
  function FastCos5Term(const Value: Double): Double; overload;
  function FastSin5Term(const Value: Single): Single; overload;
  function FastSin5Term(const Value: Double): Double; overload;

  // 7-Term: Accurate to about 12.1 decimal digits over the range [0, pi/2].
  function FastCosPart7Term(const Value: Single): Single; overload;
  function FastCosPart7Term(const Value: Double): Double; overload;
  function FastCos7Term(const Value: Single): Single; overload;
  function FastCos7Term(const Value: Double): Double; overload;
  function FastSin7Term(const Value: Single): Single; overload;
  function FastSin7Term(const Value: Double): Double; overload;

  function FastSinLike(const Value: Single): Single; overload;
  function FastSinLike(const Value: Double): Double; overload;
  function FastCosLike(const Value: Single): Single; overload;
  function FastCosLike(const Value: Double): Double; overload;
  function FastArcTan2(const Y, X: Extended): Extended;
  function FastTan(const Value: Extended): Extended;
  function FastCoTan(const Value: Extended): Extended;
  function FastLog10(const Value: Extended): Extended;
  {$ENDIF}


  { TanH Approximations }

  function FastTanhOpt3Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7Term(const Input: Single): Single; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt3Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt4Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt5Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt6Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;
  function FastTanhOpt7Term(const Input: Double): Double; {$IFDEF useinlining} inline; {$ENDIF} overload;

  function FastTanhOpt3TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt4TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt5TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt6TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt7TermFPU(const Input: Single): Single; assembler; overload;
  function FastTanhOpt3TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt4TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt5TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt6TermFPU(const Input: Double): Double; assembler; overload;
  function FastTanhOpt7TermFPU(const Input: Double): Double; assembler; overload;

  function Tanh2Like4Term(const Input: Single): Single;
  function Tanh2Like3Term(const Input: Single): Single;
  function Tanh2Like2Term(const Input: Single): Single;
  function Tanh2Like1Term(const Input: Single): Single;

var
  ln10, ln2, ln22, ln2Rez : Double;

const
  CMinusOneThird : Double = -1/3;
  CMinusTwoThird : Double = -2/3;
  CTwo32         : Single = 2;
  CTwo64         : Double = 2;
  CTwoDivPi32    : Single = 2.0 / Pi;
  CTwoDivPi64    : Double = 2.0 / Pi;
  CPiHalf32      : Single = Pi * 0.5;
  CPiHalf64      : Double = Pi * 0.5;
  CThreeHalfPi32 : Single = 1.5 * pi;  // pi times 3/2, used in tan routines
  CThreeHalfPi64 : Double = 1.5 * pi;  // pi times 3/2, used in tan routines
  CFourDivPi32   : Single = 4.0 / Pi;  // 4 / pi, used in tan routines
  CFourDivPi64   : Double = 4.0 / Pi;  // 4 / pi, used in tan routines
  CFourthPi32    : Single = Pi * 0.25; // pi / 4.0, used in tan routines
  CFourthPi64    : Double = Pi * 0.25; // pi / 4.0, used in tan routines
  CSixthPi32     : Single = Pi / 6.0;  // pi/6.0, used in atan routines
  CSixthPi64     : Double = Pi / 6.0;  // pi/6.0, used in atan routines
  CTwelfthPi32   : Single = Pi / 12.0; // pi/12.0, used in atan routines
  CTwelfthPi64   : Double = Pi / 12.0; // pi/12.0, used in atan routines
(*
  CTansixthpi=tan(sixthpi);    // tan(pi/6), used in atan routines
  CTanTwelfthPi = tan(twelfthpi);  // tan(pi/12), used in atan routines
*)

const
  CArcTanLike32 : Array [0..4] of Single = (0.0208351, -0.085133, 0.180141,
    -0.3302995, 0.999866);
  CArcTanLike64 : Array [0..4] of Single = (0.0208351, -0.085133, 0.180141,
    -0.3302995, 0.999866);
  CCos3Term : array [0..2] of Single = (0.99940307, -0.49558072, 0.03679168);
  CCos4Term : array [0..3] of Single = (0.9999932946, -0.4999124376,
    0.0414877472, -0.0012712095);
  CCos5Term : array [0..4] of Double = (0.999999953464, -0.4999999053455,
    0.0416635846769, -0.0013853704264, 0.000023233);
  CCos7Term : array [0..6] of Double = (0.99999999999925182,
   -0.49999999997024012, 4.1666666473384543E-2, -1.388888418000423E-3,
    2.48010406484558E-5, -2.752469638432E-7, 1.9907856854E-9);

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


{ Trigonomic Approximations }

type
  TQuadrant = 0..3;


//  3-Term: Accurate to about 3.2 decimal digits over the range [0, pi/2].

function FastCosPart3Term(const Value: Single): Single;
begin
 result := sqr(Value);
 result := (CCos3Term[0] + result * (CCos3Term[1] + CCos3Term[2] * result));
end;

function FastCosPart3Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
 result := sqr(Value);
 result := (CCos3Term[0] + result * (CCos3Term[1] + CCos3Term[2] * result));
end;

function FastCos3Term(const Value: Single): Single; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi32));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi32)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart3Term(result);
   1 : result := -FastCosPart3Term(Pi - result);
   2 : result := -FastCosPart3Term(result - Pi);
   3 : result :=  FastCosPart3Term(CTwoPI32 - result);
  end;
end;

function FastCos3Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi64));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi64)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart3Term(result);
   1 : result := -FastCosPart3Term(Pi - result);
   2 : result := -FastCosPart3Term(result - Pi);
   3 : result :=  FastCosPart3Term(CTwoPI64 - result);
  end;
end;

function FastSin3Term(const Value: Single): Single; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
  result := FastCos3Term(CPiHalf32 - Value);
end;

function FastSin3Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
  result := FastCos3Term(CPiHalf64 - Value);
end;


//  4-Term: Accurate to about 5.2 decimal digits over the range [0, pi/2].

function FastCosPart4Term(const Value: Single): Single;
begin
 result := sqr(Value);
 result := CCos4Term[0] + result * (CCos4Term[1] + result * (CCos4Term[2] + CCos4Term[3] * result));
end;

function FastCosPart4Term(const Value: Double): Double;
begin
 result := sqr(Value);
 result := CCos4Term[0] + result * (CCos4Term[1] + result * (CCos4Term[2] + CCos4Term[3] * result));
end;

function FastCos4Term(const Value: Single): Single;
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi32));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi32)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart4Term(result);
   1 : result := -FastCosPart4Term(Pi - result);
   2 : result := -FastCosPart4Term(result - Pi);
   3 : result :=  FastCosPart4Term(CTwoPI32 - result);
  end;
end;

function FastCos4Term(const Value: Double): Double;
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi64));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi64)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart4Term(result);
   1 : result := -FastCosPart4Term(Pi - result);
   2 : result := -FastCosPart4Term(result - Pi);
   3 : result :=  FastCosPart4Term(CTwoPI64 - result);
  end;
end;

function FastSin4Term(const Value: Single): Single;
begin
  result := FastCos4Term(CPiHalf32 - Value);
end;

function FastSin4Term(const Value: Double): Double;
begin
  result := FastCos4Term(CPiHalf64 - Value);
end;


//  5-Term: Accurate to about 7.3 decimal digits over the range [0, pi/2].

function FastCosPart5Term(const Value: Single): Single;
begin
 result := sqr(Value);
 result := CCos5Term[0] + result * (CCos5Term[1] + result * (CCos5Term[2] + result * (CCos5Term[3] + CCos5Term[4] * result)));
end;

function FastCosPart5Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
 result := sqr(Value);
 result := CCos5Term[0] + result * (CCos5Term[1] + result * (CCos5Term[2] + result * (CCos5Term[3] + CCos5Term[4] * result)));
end;

function FastCos5Term(const Value: Single): Single; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi32));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi32)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart5Term(result);
   1 : result := -FastCosPart5Term(Pi - result);
   2 : result := -FastCosPart5Term(result - Pi);
   3 : result :=  FastCosPart5Term(CTwoPI32 - result);
  end;
end;

function FastCos5Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi64));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi64)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart5Term(result);
   1 : result := -FastCosPart5Term(Pi - result);
   2 : result := -FastCosPart5Term(result - Pi);
   3 : result :=  FastCosPart5Term(CTwoPI64 - result);
  end;
end;

function FastSin5Term(const Value: Single): Single; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
  result := FastCos5Term(CPiHalf32 - Value);
end;

function FastSin5Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
  result := FastCos5Term(CPiHalf64 - Value);
end;


//  7-Term: Accurate to about 12.1 decimal digits over the range [0, pi/2].

function FastCosPart7Term(const Value: Single): Single;
begin
 result := sqr(Value);
 result := CCos7Term[0] + result *
          (CCos7Term[1] + result *
          (CCos7Term[2] + result *
          (CCos7Term[3] + result *
          (CCos7Term[4] + result *
          (CCos7Term[5] + CCos7Term[6] * result)))));
end;

function FastCosPart7Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
 result := sqr(Value);
 result := CCos7Term[0] + result *
          (CCos7Term[1] + result *
          (CCos7Term[2] + result *
          (CCos7Term[3] + result *
          (CCos7Term[4] + result *
          (CCos7Term[5] + CCos5Term[6] * result)))));
end;

function FastCos7Term(const Value: Single): Single; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi32));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi32)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart7Term(result);
   1 : result := -FastCosPart7Term(Pi - result);
   2 : result := -FastCosPart7Term(result - Pi);
   3 : result :=  FastCosPart7Term(CTwoPI32 - result);
  end;
end;

function FastCos7Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
var
  Quadrant : TQuadrant;
begin
  result := abs(FastMod(Value, CTwoPi64));          // Get rid of values > 2* pi
  Quadrant := TQuadrant(round(result * CTwoDivPi64)); // Get quadrant # (0 to 3) we're in
  case Quadrant of
   0 : result :=  FastCosPart7Term(result);
   1 : result := -FastCosPart7Term(Pi - result);
   2 : result := -FastCosPart7Term(result - Pi);
   3 : result :=  FastCosPart7Term(CTwoPI64 - result);
  end;
end;

function FastSin7Term(const Value: Single): Single; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
  result := FastCos7Term(CPiHalf32 - Value);
end;

function FastSin7Term(const Value: Double): Double; //  Accurate to about 3.2 decimal digits over the range [0, pi/2].
begin
  result := FastCos7Term(CPiHalf64 - Value);
end;




function FastSinLike(const Value: Single): Single;
const
  C1 : Single = 7.61E-03;
  C2 : Single = -1.6605E-01;
{$IFDEF PUREPASCAL}
var Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0),  st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastSinLike(const Value: Double): Double;
const
  C1 : Double = 7.61E-03;
  C2 : Double = -1.6605E-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0), st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastCosLike(const Value: Single): Single;
const
  C1 : Single =  3.705e-02;
  C2 : Single = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Single;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0), st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastCosLike(const Value: Double): Double;
const
  C1 : Double =  3.705e-02;
  C2 : Double = -4.967e-01;
{$IFDEF PUREPASCAL}
var
  Asqr : Double;
begin
 Asqr   := sqr(Value);
 result := (((Asqr * C1) * Asqr + C2 * Asqr) + 1) * Value;
{$ELSE}
asm
 fld   Value
 fmul  Value
 fld   C1
 fmul  st(0), st(1)
 fld   C2
 faddp st(1), st(0)
 fmulp st(1), st(0)
 fld1
 faddp
 fmul  Value
{$ENDIF}
end;

function FastArcTan(const Value: Single): Single;
var
  VSqr : Single;
begin
 VSqr   := sqr(Value);
 Result := ((((CArcTanLike32[0]  * VSqr +
               CArcTanLike32[1]) * VSqr +
               CArcTanLike32[2]) * VSqr +
               CArcTanLike32[3]) * VSqr +
               CArcTanLike32[4]) * Value;
end;

function FastArcTan(const Value: Double): Double;
var
  VSqr : Single;
begin
 VSqr   := sqr(Value);
 Result := ((((CArcTanLike64[0]  * VSqr +
               CArcTanLike64[1]) * VSqr +
               CArcTanLike64[2]) * VSqr +
               CArcTanLike64[3]) * VSqr +
               CArcTanLike64[4]) * Value;
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
 Value  := ((CMinusOneThird * Value) + CTwo32) * Value + CMinusTwoThird;
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
  l : Integer absolute i;
begin
 Result := (l - $3F800000) shr (n-1) + $3F800000;
end;

function FastPower(base, exp : Double): Double;
begin
 Result := Power(base, exp);
end;

function FastRoot(i: Single; n: Integer): Single;
var
  l : Integer absolute i;
begin
 Result := (l - $3F800000) shr (n-1) + $3F800000;
end;

function FastTanhOpt3Term(const Input: Single): Single;
var
  a, b : Double;
begin
 a := abs(Input);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt4Term(const Input: Single): Single;
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

function FastTanhOpt5Term(const Input: Single): Single;
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

function FastTanhOpt6Term(const Input: Single): Single;
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

function FastTanhOpt7Term(const Input: Single): Single;
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

function FastTanhOpt3Term(const Input: Double): Double;
var
  a, b : Double;
begin
 a := abs(Input);
 b := 1.26175667589988239 + a *
    (-0.54699348440059470 + a *
    ( 2.66559097474027817));
 Result := (b * Input) / (b * a + 1);
end;

function FastTanhOpt4Term(const Input: Double): Double;
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

function FastTanhOpt5Term(const Input: Double): Double;
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

function FastTanhOpt6Term(const Input: Double): Double;
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

function FastTanhOpt7Term(const Input: Double): Double;
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

function FastTanhOpt3TermFPU(const Input: Single): Single; assembler;
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

function FastTanhOpt4TermFPU(const Input: Single): Single; assembler;
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

function FastTanhOpt5TermFPU(const Input: Single): Single; assembler;
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

function FastTanhOpt6TermFPU(const Input: Single): Single; assembler;
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

function FastTanhOpt7TermFPU(const Input: Single): Single; assembler;
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

function FastTanhOpt3TermFPU(const Input: Double): Double; assembler;
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

function FastTanhOpt4TermFPU(const Input: Double): Double; assembler;
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

function FastTanhOpt5TermFPU(const Input: Double): Double; assembler;
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

function FastTanhOpt6TermFPU(const Input: Double): Double; assembler;
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

function FastTanhOpt7TermFPU(const Input: Double): Double; assembler;
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

function Tanh2Like4Term(const Input: Single): Single;
var
  a, b: Single;
begin
 a := abs(Input);
 b := 12 + a * (6 + a * (3 + a));
 Result := (Input * b) / (a * b + 24);
end;

function Tanh2Like3Term(const Input: Single): Single;
var
  a, b: Single;
begin
 a := abs(Input);
 b := (6 + a * (3 + a));
 Result := (Input * b) / (a * b + 12);
end;

function Tanh2Like2Term(const Input: Single): Single;
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

function Tanh2Like1Term(const Input: Single): Single;
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
