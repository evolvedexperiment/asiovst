unit DAV_EllipticFunctions;

interface

{$I DAV_Compiler.inc}

uses
  DAV_Common, DAV_Complex;

function IncompleteEllipticIntegral1stKind(k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function IncompleteEllipticIntegral2ndKind(k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function IncompleteEllipticIntegral3rdKind(n, k: Double; Phi: Double = 0.5 * Pi; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral1stKind(k: Double; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral2ndKind(k: Double; Steps: Integer = 1000): Double;
function CompleteEllipticIntegral3rdKind(n, k: Double; Steps: Integer = 1000): Double;
function NomeQ(m: Double): Double;
function Theta00(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta01(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta10(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta11(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta1(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta2(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta3(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Theta4(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
function Sn(z: TComplexDouble; k: Double): TComplexDouble;

implementation

uses
  Math;

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

function CompleteEllipticIntegral1stKind(k: Double; Steps: Integer = 1000): Double;
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
   Result := Result + Scale / sqrt(1 - sqr(k * Pos.Im));

   ComplexMultiplyInplace(Pos, Cmplx);
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

function NomeQ(m: Double): Double;
begin
 result := exp(-Pi * CompleteEllipticIntegral1stKind(1 - m) / CompleteEllipticIntegral1stKind(m));
end;

(*
function Theta1(z: TComplexDouble; q: TComplexDouble): TComplexDouble;
var
  Epsilon : Double;
  Step    : Integer;
begin
 Result := 0;
 Step := 0;
 repeat
//  Power(q, Step + 0.5) / (1 - Power(q, 2 * Step + 1)) * sin(2 *
  Inc(Step);
 until Epsilon < 1E-10;

 Result := 2 * Power(q, 1/4) * result;
end;
*)

function Theta00(z: TComplexDouble; Theta: TComplexDouble): TComplexDouble;
var
  Epsilon : Double;
  Step    : Integer;
  Current : TComplexDouble;
begin
 assert(Theta.Im > 0);
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
  Kk : Double;
begin
 Kk := 1 / (2 * CompleteEllipticIntegral1stKind(k));
 z.Re := z.Re * Kk;
 z.Im := z.Im * Kk;

 Result := ComplexDivide(Theta2(z, Theta),  Theta1(z, Theta))
 Result.Re := Result.Re / sqrt(k);
 Result.Im := Result.Im / sqrt(k);
end;

end.
