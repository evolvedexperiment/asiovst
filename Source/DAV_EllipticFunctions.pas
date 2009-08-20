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
function Theta00(z: Double; q: Double): Double; overload;
function Theta01(z: Double; q: Double): Double; overload;
function Theta10(z: Double; q: Double): Double; overload;
function Theta11(z: Double; q: Double): Double; overload;
function Theta00(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta01(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta10(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta11(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta1(z: Double; q: Double): Double; overload;
function Theta2(z: Double; q: Double): Double; overload;
function Theta3(z: Double; q: Double): Double; overload;
function Theta4(z: Double; q: Double): Double; overload;
function Theta1(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta2(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta3(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Theta4(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble; overload;
function Sn(z: Double; k: Double): Double;
function Cn(z: Double; k: Double): Double;
function Dn(z: Double; k: Double): Double;
function Ns(z: Double; k: Double): Double;
function Nc(z: Double; k: Double): Double;
function Nd(z: Double; k: Double): Double;
function Sd(z: Double; k: Double): Double;
function Sc(z: Double; k: Double): Double;
function Cd(z: Double; k: Double): Double;
function Cs(z: Double; k: Double): Double;
function Dc(z: Double; k: Double): Double;
function Ds(z: Double; k: Double): Double;

implementation

uses
  Math;


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
  then result := 0
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
  0 : result := 1;
  1 : result := x;
  2 : result := 0.5 * (3 * sqr(x) - 1);
  3 : result := 0.5 * (5 * sqr(x) * x - 3 * x);
  4 : result := 0.125 * (35 * sqr(sqr(x)) - 30 * sqr(x) + 3);
  5 : result := 0.125 * (63 * sqr(sqr(x)) * x - 70 * sqr(x) * x + 15 * x);
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
   Result := Result + Scale / sqrt(1 - sqr(k * Pos.Im));

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
   Result := Result + Scale * sqrt(1 - sqr(k * Pos.Im));

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
   Result := Result + Scale / ((1 - n * sqr(Pos.Im)) * sqrt(1 - sqr(k * Pos.Im)));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := Phi * Result;
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
  Result := sqr(Result);
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
  Result := sqr(Result);
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
   Result := Result + Scale * sqrt(1 - sqr(k * Pos.Im));

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
   Result := Result + Scale / ((1 - n * sqr(Pos.Im)) * sqrt(1 - sqr(k * Pos.Im)));

   ComplexMultiplyInplace(Pos, Cmplx);
  end;

 Result := 0.5 * Pi * Result;
end;

////////////////////////////////////////////////////////////////////////////////

function NomeQk(k: Double): Double;
begin
 result := exp(-Pi * CompleteEllipticIntegral1stKind(sqrt(1 - sqr(k))) / CompleteEllipticIntegral1stKind(k));
end;

function NomeQm(m: Double): Double;
begin
 result := exp(-Pi * CompleteEllipticIntegral1stKind(1 - m) / CompleteEllipticIntegral1stKind(m));
end;

function NomeQk(k: TComplexDouble): TComplexDouble; overload;
begin
 raise Exception.Create('Yet todo!');
(*
 Result := Pi * CompleteEllipticIntegral1stKind(sqrt(1 - sqr(k.Re))) / CompleteEllipticIntegral1stKind(k.Re));

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

function Theta00(z: Double; Theta: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Result := 0;
 Step := 1;
 repeat
  Current := Power(q, sqr(Step)) * cos(2 * Step * z);

  // add current value to result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta01(z: Double; Theta: Double): Double;
begin

end;

function Theta10(z: Double; Theta: Double): Double;
begin

end;

function Theta11(z: Double; Theta: Double): Double;
begin

end;

////////////////////////////////////////////////////////////////////////////////

function Theta00(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
var
  Epsilon : Double;
  Step    : Integer;
  Current : TComplexDouble;
begin
 assert(Theta.Im >= 0);
 Result.Re := 0;
 Result.Im := 0;
 Step := 1;
 repeat
  Current.Re := Pi * (sqr(Step) * Theta.Im + 2 * Step * z.Im);
  Current.Im := Pi * (sqr(Step) * Theta.Re + 2 * Step * z.Re);

  Current := ComplexExp(Current);

  // evaluate epsilon
  Epsilon := max(Current.Re, Current.Im);

  // add current value to result
  Result.Re := Result.Re + Current.Re;
  Result.Im := Result.Im + Current.Im;

  Inc(Step);
 until Epsilon < 1E-10;

 Result.Re := 1 + 2 * Result.Re;
 Result.Im := 2 * Result.Im;
end;

function Theta01(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 z.Re := z.Re + 0.5;
 Result := Theta00(z, Theta);
end;

function Theta10(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 z.Re := z.Re + 0.5 * Theta.Re;
 z.Im := z.Im + 0.5 * Theta.Im;
 Result := Theta00(z, Theta);
 z.Im := Pi * (Theta.Im - 0.25 * z.Im);
 z.Re := Pi * (Theta.Re - 0.25 * z.Re);
 ComplexMultiplyInplace(Result, ComplexExp(z.Im, z.Re));
end;

function Theta11(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 z.Re := z.Re + 0.5;
 Result := Theta10(z, Theta);
end;

function Theta1(z: Double; q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Result := 0;
 Step := 0;
 repeat
  Current := Power(q, Step * (Step + 1)) * sin((2 * Step + 1) * z);
  if Step mod 2 = 1 then Current := -Current;

  // add current value to result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 2 * Power(Q, 0.25) * Result;
end;

function Theta2(z: Double; q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Result := 0;
 Step := 0;
 repeat
  Current := Power(q, Step * (Step + 1)) * cos((2 * Step + 1) * z);

  // add current value to result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 2 * Power(Q, 0.25) * Result;
end;

function Theta3(z: Double; q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Result := 0;
 Step := 1;
 repeat
  Current := Power(q, sqr(Step)) * cos(2 * Step * z);

  // add current value to result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta4(z: Double; q: Double): Double;
var
  Step    : Integer;
  Current : Double;
begin
 Result := 0;
 Step := 1;
 repeat
  Current := IntPower(q, sqr(Step)) * cos(2 * Step * z);
  if Step mod 2 = 1 then Current := -Current;

  // add current value to result
  Result := Result + Current;

  Inc(Step);
 until Current < 1E-10;

 Result := 1 + 2 * Result;
end;

function Theta1(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 Result := Theta11(z, Theta);
 Result.Re := -Result.Re;
 Result.Im := -Result.Im;
end;

function Theta2(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 Result := Theta10(z, Theta);
end;

function Theta3(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 Result := Theta00(z, Theta);
end;

function Theta4(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
begin
 Result := Theta01(z, Theta);
end;

function Sn(z: TComplexDouble; k: Double): TComplexDouble;
var
  Kk   : Double;
begin
 {$IFDEF UseThetaFunctions}
 Kk := 1 / (2 * CompleteEllipticIntegral1stKind(k));

 z.Re := z.Re * Kk;
 z.Im := z.Im * Kk;

// Result := ComplexDivide(Theta2(z, Theta),  Theta1(z, Theta))
 Result.Re := Result.Re / sqrt(k);
 Result.Im := Result.Im / sqrt(k);
 {$ENDIF}

 {$IFDEF UseLambert}
 Kk[0] := CompleteEllipticIntegral1stKind(k);
 Kk[1] := CompleteEllipticIntegral1stKind(sqrt(1 - sqr(k)));

 Q := Exp(-Pi * Kk[0] / Kk[1]);

 for Step := 0 to 1000 do
  begin

  end;


 Result.Re := Result.Re * 2 * Pi / (Kk * abs(k));
 Result.Im := Result.Im * 2 * Pi / (Kk * abs(k));
 {$ENDIF}
end;

function Cn(z: TComplexDouble; k: Double): TComplexDouble;
begin

end;

function Dn(z: TComplexDouble; k: Double): TComplexDouble;
begin

end;


function Ns(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexReciprocal(Dn(z, k));
end;

function Nc(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexReciprocal(Dn(z, k));
end;

function Nd(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexReciprocal(Dn(z, k));
end;

function Sd(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexDivide(Sn(z, k), Dn(z, k));
end;

function Sc(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexDivide(Sn(z, k), Cn(z, k));
end;

function Cd(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexDivide(Cn(z, k), Dn(z, k));
end;

function Cs(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexDivide(Cn(z, k), Sn(z, k));
end;

function Dc(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexDivide(Dn(z, k), Cn(z, k));
end;

function Ds(z: TComplexDouble; k: Double): TComplexDouble;
begin
 result := ComplexDivide(Dn(z, k), Sn(z, k));
end;

end.
