unit DAV_EllipticFunctions;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex;

function DoubleFactorial(n: Integer): Integer;
function LegendreDenominator(n: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function LegendrePolynomial(n: Integer; x: Double): Double;
function IncompleteEllipticIntegral1stKind(k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function IncompleteEllipticIntegral2ndKind(k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function IncompleteEllipticIntegral3rdKind(n, k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral1stKind(k: TComplexDouble; Steps: Integer = 30): TComplexDouble; overload;
function CompleteEllipticIntegral1stKind(k: Double; Steps: Integer = 30): Double; overload;
function CompleteEllipticIntegral2ndKind(k: Double; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral3rdKind(n, k: Double; Steps: Integer = 1000): Double;
function NomeQk(k: Double): Double; overload;
function NomeQm(m: Double): Double; overload;
function NomeQk(k: TComplexDouble): TComplexDouble; overload;
function NomeQm(m: TComplexDouble): TComplexDouble; overload;
function Theta00(q: Double): Double; overload;
function Theta01(q: Double): Double; overload;
function Theta10(q: Double): Double; overload;
function Theta11(q: Double): Double; overload;
function Theta00(z, q: Double): Double; overload;
function Theta01(z, q: Double): Double; overload;
function Theta10(z, q: Double): Double; overload;
function Theta11(z, q: Double): Double; overload;
function Theta00(q: TComplexDouble): TComplexDouble; overload;
function Theta01(q: TComplexDouble): TComplexDouble; overload;
function Theta10(q: TComplexDouble): TComplexDouble; overload;
function Theta11(q: TComplexDouble): TComplexDouble; overload;
function Theta00(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta01(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta10(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta11(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta1(z, q: Double): Double; overload;
function Theta2(z, q: Double): Double; overload;
function Theta3(z, q: Double): Double; overload;
function Theta4(z, q: Double): Double; overload;
function Theta1(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta2(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta3(z, q:  TComplexDouble): TComplexDouble; overload;
function Theta4(z, q:  TComplexDouble): TComplexDouble; overload;

function Sn(z, k: Double): Double; overload;
function Cn(z, k: Double): Double; overload;
function Dn(z, k: Double): Double; overload;
procedure JacobiEllipticFunctions(z, k: Double; out sn, cn, dn: Double);

function Ns(z, k: Double): Double; overload;
function Nc(z, k: Double): Double; overload;
function Nd(z, k: Double): Double; overload;
function Sd(z, k: Double): Double; overload;
function Sc(z, k: Double): Double; overload;
function Cd(z, k: Double): Double; overload;
function Cs(z, k: Double): Double; overload;
function Dc(z, k: Double): Double; overload;
function Ds(z, k: Double): Double; overload;

function Sn(z, k: TComplexDouble): TComplexDouble; overload;
function Cn(z, k: TComplexDouble): TComplexDouble; overload;
function Dn(z, k: TComplexDouble): TComplexDouble; overload;
function Ns(z, k: TComplexDouble): TComplexDouble; overload;
function Nc(z, k: TComplexDouble): TComplexDouble; overload;
function Nd(z, k: TComplexDouble): TComplexDouble; overload;
function Sd(z, k: TComplexDouble): TComplexDouble; overload;
function Sc(z, k: TComplexDouble): TComplexDouble; overload;
function Cd(z, k: TComplexDouble): TComplexDouble; overload;
function Cs(z, k: TComplexDouble): TComplexDouble; overload;
function Dc(z, k: TComplexDouble): TComplexDouble; overload;
function Ds(z, k: TComplexDouble): TComplexDouble; overload;

implementation

uses
  Math, SysUtils;


function DoubleFactorial(n: Integer): Integer;
begin
 if n < 0
  then Result := 0
  else Result := Factorial(2 * n) div Factorial(n) div (1 shl n);
end;

function LegendreDenominator(n: Integer): Integer;
var
  Step : Integer;
  Sum  : Integer;
begin
 if n < 0
  then Result := 0
  else
   begin
    Sum := 0;
    for Step := 1 to n
     do Sum := Sum + n div (1 shl Step);
    Result := 1 shl Sum;
   end;
end;

function LegendrePolynomial(n: Integer; x: Double): Double;
begin
 case n of
  0 : Result := 1;
  1 : Result := x;
  2 : Result := 0.5 * (3 * Sqr(x) - 1);
  3 : Result := 0.5 * (5 * Sqr(x) * x - 3 * x);
  4 : Result := 0.125 * (35 * Sqr(Sqr(x)) - 30 * Sqr(x) + 3);
  5 : Result := 0.125 * (63 * Sqr(Sqr(x)) * x - 70 * Sqr(x) * x + 15 * x);
  else raise Exception.Create('Not yet defined!');
 end;
end;

function IncompleteEllipticIntegral1stKind(k: Double; Phi: Double = 0.5 * Pi;
  Steps: Integer = 1000): Double;
var
  i     : Integer;
  Scale : Double;
  Cmplx : TComplexDouble;
  Pos   : TComplexDouble;
begin
 Result := 0;
 Scale  := 1 / Steps;
 Pos.Re := 1;
 Pos.Im := 0;
 GetSinCos(Scale * Phi, Cmplx.Im, Cmplx.Re);
 for i := 0 to Steps do
  begin
   Result := Result + Scale / sqrt(1 - Sqr(k * Pos.Im));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := Phi * Result;
end;

function IncompleteEllipticIntegral2ndKind(k: Double; Phi: Double = 0.5 * Pi;
  Steps: Integer = 1000): Double;
var
  i     : Integer;
  Scale : Double;
  Cmplx : TComplexDouble;
  Pos   : TComplexDouble;
begin
 Result := 0;
 Scale  := 1 / Steps;
 Pos.Re := 1;
 Pos.Im := 0;
 GetSinCos(Scale * Phi, Cmplx.Im, Cmplx.Re);
 for i := 0 to Steps do
  begin
   Result := Result + Scale * sqrt(1 - Sqr(k * Pos.Im));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := Phi * Result;
end;

function IncompleteEllipticIntegral3rdKind(n, k: Double; Phi: Double = 0.5 * Pi;
  Steps: Integer = 1000): Double;
var
  i     : Integer;
  Scale : Double;
  Cmplx : TComplexDouble;
  Pos   : TComplexDouble;
begin
 Result := 0;
 Scale  := 1 / Steps;
 Pos.Re := 1;
 Pos.Im := 0;
 GetSinCos(Scale * Phi, Cmplx.Im, Cmplx.Re);
 for i := 0 to Steps do
  begin
   Result := Result + Scale / ((1 - n * Sqr(Pos.Im)) * sqrt(1 - Sqr(k * Pos.Im)));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := Phi * Result;
end;

////////////////////////////////////////////////////////////////////////////////

function CompleteEllipticIntegral1stKind(k: Double; Steps: Integer = 30): Double;
var
  Factor : Double;
  Temp   : Double;
  Step   : Integer;

 function CalculateFactor(Step: Integer): Single;
 var
   n : Integer;
 begin
  Result := 1;
  for n := 1 to Step do Result := Result * ((n - 0.5) / n);
  Result := Sqr(Result);
 end;

begin
 // zero & first step at once
 Factor    := 0.25;
 Temp      := k;
 Result := 1 + Factor * Temp;

 // second step
 Temp := Temp * k;
 Factor := 0.140625;
 Result := Result + Factor * Temp;

 // n-th step
 for Step := 3 to Steps do
  begin
   Temp := Temp * k;
   Factor  := CalculateFactor(Step);
   Result := Result + Factor * Temp;
  end;
 Result := 0.5 * Pi * Result;
end;

function CompleteEllipticIntegral1stKind(k: TComplexDouble; Steps: Integer = 30): TComplexDouble; overload;
var
  Factor : Double;
  Temp   : TComplexDouble;
  Step   : Integer;

 function CalculateFactor(Step: Integer): Single;
 var
   n : Integer;
 begin
  Result := 1;
  for n := 1 to Step do Result := Result * ((n - 0.5) / n);
  Result := Sqr(Result);
 end;

begin
 // zero & first step at once
 Factor    := 0.25;
 Temp      := k;
 Result.Re := 1 + Factor * Temp.Re;
 Result.Im := 0 + Factor * Temp.Im;

 // second step
 ComplexMultiplyInplace(Temp, k);
 Factor    := 0.140625;
 Result.Re := Result.Re + Factor * Temp.Re;
 Result.Im := Result.Im + Factor * Temp.Im;

 // n-th step
 for Step := 3 to 1000 do
  begin
   ComplexMultiplyInplace(Temp, k);
   Factor  := CalculateFactor(Step);
   Result.Re := Result.Re + Factor * Temp.Re;
   Result.Im := Result.Im + Factor * Temp.Im;
  end;
 Result.Re := 0.5 * Pi * Result.Re;
 Result.Im := 0.5 * Pi * Result.Im;
end;

////////////////////////////////////////////////////////////////////////////////

function CompleteEllipticIntegral2ndKind(k: Double; Steps: Integer = 1000): Double;
var
  i     : Integer;
  Scale : Double;
  Cmplx : TComplexDouble;
  Pos   : TComplexDouble;
begin
 Result := 0;
 Scale  := 1 / Steps;
 Pos.Re := 1;
 Pos.Im := 0;
 GetSinCos(Scale * 0.5 * Pi, Cmplx.Im, Cmplx.Re);
 for i := 0 to Steps do
  begin
   Result := Result + Scale * sqrt(1 - Sqr(k * Pos.Im));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := 0.5 * Pi * Result;
end;

function CompleteEllipticIntegral3rdKind(n, k: Double; Steps: Integer = 1000): Double;
var
  i     : Integer;
  Scale : Double;
  Cmplx : TComplexDouble;
  Pos   : TComplexDouble;
begin
 Result := 0;
 Scale  := 1 / Steps;
 Pos.Re := 1;
 Pos.Im := 0;
 GetSinCos(Scale * 0.5 * Pi, Cmplx.Im, Cmplx.Re);
 for i := 0 to Steps do
  begin
   Result := Result + Scale / ((1 - n * Sqr(Pos.Im)) * sqrt(1 - Sqr(k * Pos.Im)));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := 0.5 * Pi * Result;
end;

////////////////////////////////////////////////////////////////////////////////

function NomeQk(k: Double): Double;
begin
 Assert(k <= 1);
 Assert(k >= 0);
 Result := exp(-Pi * CompleteEllipticIntegral1stKind(sqrt(1 - Sqr(k))) /
                     CompleteEllipticIntegral1stKind(k));
end;

function NomeQm(m: Double): Double;
begin
 Result := exp(-Pi * CompleteEllipticIntegral1stKind(1 - m) / CompleteEllipticIntegral1stKind(m));
end;

function NomeQk(k: TComplexDouble): TComplexDouble; overload;
begin
 raise Exception.Create('Yet todo!');
(*
 Result := Pi * CompleteEllipticIntegral1stKind(sqrt(1 - Sqr(k.Re))) / CompleteEllipticIntegral1stKind(k.Re));

 Result.Re := Pi * Result.Re;
 Result.Im := Pi * Result.Im;
 Result := ComplexExp(Result);
*)
end;

function NomeQm(m: TComplexDouble): TComplexDouble; overload;
begin
 raise Exception.Create('Yet todo!');
end;

////////////////////////////////////////////////////////////////////////////////

function Theta00(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta01(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);
  if Step mod 2 = 1 then Current := -Current;

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta10(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 0;
 repeat
  Current := IntPower(q, Step * (Step + 1)) * cos((2 * Step + 1) * z);

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 2 * Power(Q, 0.25) * Result;
end;

function Theta11(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 0;
 repeat
  Current := IntPower(q, Step * (Step + 1)) * sin((2 * Step + 1) * z);
  if Step mod 2 = 1 then Current := -Current;

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := -2 * Power(Q, 0.25) * Result;
end;

////////////////////////////////////////////////////////////////////////////////

function Theta00(q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, Sqr(Step));

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta01(q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, Sqr(Step));
  if Step mod 2 = 1 then Current := -Current;

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta10(q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 0;
 repeat
  Current := IntPower(q, Step * (Step + 1));

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 2 * Power(Q, 0.25) * Result;
end;

function Theta11(q: Double): Double;
begin
 Assert(q < 1);
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function Theta00(q: TComplexDouble): TComplexDouble;
var
  Epsilon : Double;
  Step    : Integer;
  Current : TComplexDouble;
begin
 Assert(q.Im >= 0);
 Result.Re := 0;
 Result.Im := 0;
 Step := 1;
 repeat
  Current.Re := Pi * (Sqr(Step) * q.Im);
  Current.Im := Pi * (Sqr(Step) * q.Re);

  Current := ComplexExp(Current);

  // evaluate epsilon
  Epsilon := max(Current.Re, Current.Im);

  // add current value to Result
  Result.Re := Result.Re + Current.Re;
  Result.Im := Result.Im + Current.Im;

  Inc(Step);
 until Epsilon < 1E-10;

 Result.Re := 1 + 2 * Result.Re;
 Result.Im := 2 * Result.Im;
end;

function Theta01(q: TComplexDouble): TComplexDouble;
var
  Temp : TComplexDouble;
begin
 Temp.Re := 0.5;
 Temp.Im := 0;
 Result := Theta00(Temp, q);
end;

function Theta10(q: TComplexDouble): TComplexDouble;
var
  Temp : TComplexDouble;
begin
 Temp.Re := 0.5 * q.Re;
 Temp.Im := 0.5 * q.Im;
 Result := Theta00(Temp, q);
 Temp.Im := Pi * (q.Im - 0.25 * Temp.Im);
 Temp.Re := Pi * (q.Re - 0.25 * Temp.Re);
 ComplexMultiplyInplace(Result, ComplexExp(Temp.Im, Temp.Re));
end;

function Theta11(q: TComplexDouble): TComplexDouble;
var
  Temp : TComplexDouble;
begin
 Temp.Re := 0.5;
 Temp.Im := 0;
 Result := Theta10(Temp, q);
end;

////////////////////////////////////////////////////////////////////////////////

function Theta00(z, q:  TComplexDouble): TComplexDouble;
var
  Epsilon : Double;
  Step    : Integer;
  Current : TComplexDouble;
begin
 Assert(q.Im >= 0);
 Result.Re := 0;
 Result.Im := 0;
 Step := 1;
 repeat
  Current.Re := Pi * (Sqr(Step) * q.Im + 2 * Step * z.Im);
  Current.Im := Pi * (Sqr(Step) * q.Re + 2 * Step * z.Re);

  Current := ComplexExp(Current);

  // evaluate epsilon
  Epsilon := max(Current.Re, Current.Im);

  // add current value to Result
  Result.Re := Result.Re + Current.Re;
  Result.Im := Result.Im + Current.Im;

  Inc(Step);
 until Epsilon < 1E-10;

 Result.Re := 1 + 2 * Result.Re;
 Result.Im := 2 * Result.Im;
end;

function Theta01(z, q:  TComplexDouble): TComplexDouble;
begin
 z.Re := z.Re + 0.5;
 Result := Theta00(z, q);
end;

function Theta10(z, q:  TComplexDouble): TComplexDouble;
begin
 z.Re := z.Re + 0.5 * q.Re;
 z.Im := z.Im + 0.5 * q.Im;
 Result := Theta00(z, q);
 z.Im := Pi * (q.Im - 0.25 * z.Im);
 z.Re := Pi * (q.Re - 0.25 * z.Re);
 ComplexMultiplyInplace(Result, ComplexExp(z.Im, z.Re));
end;

function Theta11(z, q:  TComplexDouble): TComplexDouble;
begin
 z.Re := z.Re + 0.5;
 Result := Theta10(z, q);
end;

////////////////////////////////////////////////////////////////////////////////

function Theta1(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 0;
 repeat
  Current := IntPower(q, Step * (Step + 1)) * sin((2 * Step + 1) * z);
  if Step mod 2 = 1 then Current := -Current;

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 2 * Power(Q, 0.25) * Result;
end;

function Theta2(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 0;
 repeat
  Current := IntPower(q, Step * (Step + 1)) * cos((2 * Step + 1) * z);

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 2 * Power(Q, 0.25) * Result;
end;

function Theta3(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 1);
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta4(z, q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Assert(q < 0);
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, Sqr(Step)) * cos(2 * Step * z);
  if Step mod 2 = 1 then Current := -Current;

  // add current value to Result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

////////////////////////////////////////////////////////////////////////////////

function Theta1(z, q:  TComplexDouble): TComplexDouble;
begin
 Result := Theta11(z, q);
 Result.Re := -Result.Re;
 Result.Im := -Result.Im;
end;

function Theta2(z, q:  TComplexDouble): TComplexDouble;
begin
 Result := Theta10(z, q);
end;

function Theta3(z, q:  TComplexDouble): TComplexDouble;
begin
 Result := Theta00(z, q);
end;

function Theta4(z, q:  TComplexDouble): TComplexDouble;
begin
 Result := Theta01(z, q);
end;

////////////////////////////////////////////////////////////////////////////////

function Sn(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);
 Result := -(Theta00(Theta) * Theta11(z, Theta)) /
            (Theta10(Theta) * Theta01(z, Theta));
end;

function Cn(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);
 Result := (Theta01(Theta) * Theta10(z, Theta)) /
           (Theta10(Theta) * Theta01(z, Theta));
end;

function Dn(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);
 Result := (Theta01(Theta) * Theta00(z, Theta)) /
           (Theta00(Theta) * Theta01(z, Theta));
end;

procedure JacobiEllipticFunctions(z, k: Double; out sn, cn, dn: Double);
var
  Theta : Double;
  Ts    : array [0..1, 0..2] of Double;
begin
 Theta  := NomeQk(k);

 Ts[0, 0] := Theta00(Theta);
 Ts[0, 1] := Theta01(Theta);
 Ts[0, 2] := Theta10(Theta);

 Ts[1, 0] := Theta01(z, Theta);
 Ts[1, 1] := Theta10(z, Theta);
 Ts[1, 2] := Theta11(z, Theta);


 Sn := -(Ts[0, 0] * Ts[1, 2]) / (Ts[0, 2] * Ts[1, 0]);

 Cn :=  (Ts[0, 1] * Ts[1, 1]) / (Ts[0, 2] * Ts[1, 0]);

 Dn :=  (Ts[0, 1] * Theta00(z, Theta)) / (Ts[0, 0] * Ts[1, 0]);
end;

////////////////////////////////////////////////////////////////////////////////

function Ns(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);
 Result := -(Theta10(Theta) * Theta01(z, Theta)) /
            (Theta00(Theta) * Theta11(z, Theta));
end;

function Nc(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);
 Result := (Theta10(Theta) * Theta01(z, Theta)) /
           (Theta01(Theta) * Theta10(z, Theta));
end;

function Nd(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);
 Result := (Theta00(Theta) * Theta01(z, Theta)) /
           (Theta01(Theta) * Theta00(z, Theta));
end;

function Sd(z, k: Double): Double;
var
  Theta  : Double;
begin
 Theta  := NomeQk(k);

 Result := -Sqr(Theta00(Theta)) * (Theta11(z, Theta)) /
   (Theta00(z, Theta) * Theta01(Theta) * Theta10(Theta));
end;

function Sc(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);

 Result := -(Theta00(Theta) * Theta11(z, Theta)) /
   (Theta01(Theta) * Theta10(z, Theta));
end;

function Cd(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);

 Result := (Theta00(Theta) * Theta10(z, Theta)) /
   (Theta10(Theta) * Theta00(z, Theta));
end;

function Cs(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);

 Result := -(Theta01(Theta) * Theta10(z, Theta)) /
   (Theta00(Theta) * Theta11(z, Theta));
end;

function Dc(z, k: Double): Double;
var
  Theta : Double;
begin
 Theta  := NomeQk(k);

 Result := (Theta10(Theta) * Theta00(z, Theta)) /
   (Theta00(Theta) * Theta10(z, Theta));
end;

function Ds(z, k: Double): Double;
var
  Theta  : Double;
begin
 Theta  := NomeQk(k);

 Result := -(Theta01(Theta) * Theta10(Theta) * Theta00(z, Theta)) *
   Sqr(Theta00(Theta)) / (Theta11(z, Theta));
end;

////////////////////////////////////////////////////////////////////////////////

function Sn(z, k: TComplexDouble): TComplexDouble;
var
  Theta : TComplexDouble;
begin
 Theta  := NomeQk(k);
 Result := ComplexDivide(ComplexMultiply(Theta00(Theta), Theta11(z, Theta)),
   ComplexMultiply(Theta10(Theta), Theta01(z, Theta)));
 Result.Re := -Result.Re;
 Result.Im := -Result.Im;
end;

function Cn(z, k: TComplexDouble): TComplexDouble; overload;
var
  Theta : TComplexDouble;
begin
 Theta  := NomeQk(k);
 Result := ComplexDivide(ComplexMultiply(Theta01(Theta), Theta10(z, Theta)),
   ComplexMultiply(Theta10(Theta), Theta01(z, Theta)));
end;

function Dn(z, k: TComplexDouble): TComplexDouble; overload;
var
  Theta : TComplexDouble;
begin
 Theta  := NomeQk(k);
 Result := ComplexDivide(ComplexMultiply(Theta01(Theta), Theta00(z, Theta)),
   ComplexMultiply(Theta00(Theta), Theta01(z, Theta)));
end;

////////////////////////////////////////////////////////////////////////////////

function Ns(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexReciprocal(Dn(z, k));
end;

function Nc(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexReciprocal(Dn(z, k));
end;

function Nd(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexReciprocal(Dn(z, k));
end;

function Sd(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexDivide(Sn(z, k), Dn(z, k));
end;

function Sc(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexDivide(Sn(z, k), Cn(z, k));
end;

function Cd(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexDivide(Cn(z, k), Dn(z, k));
end;

function Cs(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexDivide(Cn(z, k), Sn(z, k));
end;

function Dc(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexDivide(Dn(z, k), Cn(z, k));
end;

function Ds(z, k: TComplexDouble): TComplexDouble;
begin
 Result := ComplexDivide(Dn(z, k), Sn(z, k));
end;

end.
