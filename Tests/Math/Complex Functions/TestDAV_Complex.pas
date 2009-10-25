unit TestDAV_Complex;
{

  Delphi DUnit Testfall
  ----------------------
  Diese Unit enthält ein Codegerüst einer Testfallklasse, das vom Testfall-Experten
  erzeugt wurde. Ändern Sie den erzeugten Code, damit die Methoden aus der
  getesteten Unit korrekt eingerichtet und aufgerufen werden.

}

interface

uses
  TestFramework, Windows, Classes, DAV_Complex;
type
  // Test methods for class complex functions

  TestComplexFunctions = class(TTestCase)
  published
    procedure TestComplexAdd32;
    procedure TestComplexAdd64;
    procedure TestComplexSubtract32;
    procedure TestComplexSubtract64;
    procedure TestComplexMultiply32;
    procedure TestComplexMultiply64;
    procedure TestComplexDivide32;
    procedure TestComplexDivide64;
    procedure TestComplexSqr32;
    procedure TestComplexSqr64;
    procedure TestComplexSqrt32;
    procedure TestComplexSqrt64;
    procedure TestComplexExp32;
    procedure TestComplexExp64;
    procedure TestComplexLn32;
    procedure TestComplexLn64;
    procedure TestComplexSin32;
    procedure TestComplexSin64;
    procedure TestComplexCos32;
    procedure TestComplexCos64;
    procedure TestComplexTan32;
    procedure TestComplexTan64;
    procedure TestComplexTanh32;
    procedure TestComplexTanh64;
    procedure TestComplexArcSin32;
    procedure TestComplexArcSin64;
    procedure TestComplexArcCos32;
    procedure TestComplexArcCos64;
    procedure TestComplexArcTan32;
    procedure TestComplexArcTan64;
    procedure TestComplexArcTanh32;
    procedure TestComplexArcTanh64;
  end;

implementation

uses
  Math, DAV_Common;

const
  CEpsilon32 = 1E-5;
  CEpsilon64 = 1E-9;

procedure TestComplexFunctions.TestComplexAdd32;
var
  A, B   : TComplexSingle;
  Result : TComplexSingle;
begin
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexAdd(A, B);
 CheckTrue(Result.Re = 3);
 CheckTrue(Result.Im = 3);

 Result := ComplexAdd(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 3);
 CheckTrue(Result.Im = 3);
end;

procedure TestComplexFunctions.TestComplexAdd64;
var
  A, B   : TComplexDouble;
  Result : TComplexDouble;
begin
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexAdd(A, B);
 CheckTrue(Result.Re = 3);
 CheckTrue(Result.Im = 3);

 Result := ComplexAdd(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 3);
 CheckTrue(Result.Im = 3);
end;

procedure TestComplexFunctions.TestComplexSubtract32;
var
  A, B   : TComplexSingle;
  Result : TComplexSingle;
begin
 A.Re := 5;
 A.Im := 3;
 B.Re := 4;
 B.Im := 2;

 Result := ComplexSubtract(A, B);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 1);

 Result := ComplexSubtract(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 1);
end;

procedure TestComplexFunctions.TestComplexSubtract64;
var
  A, B   : TComplexDouble;
  Result : TComplexDouble;
begin
 A.Re := 5;
 A.Im := 3;
 B.Re := 4;
 B.Im := 2;

 Result := ComplexSubtract(A, B);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 1);

 Result := ComplexSubtract(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 1);
end;

procedure TestComplexFunctions.TestComplexMultiply32;
var
  A, B   : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexMultiply(A, B);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im :=  2;

 Result := ComplexMultiply(A, B);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 1;

 Result := ComplexMultiply(A, B);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 3);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 3);
end;

procedure TestComplexFunctions.TestComplexMultiply64;
var
  A, B   : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexMultiply(A, B);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im :=  2;

 Result := ComplexMultiply(A, B);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 1;

 Result := ComplexMultiply(A, B);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 3);

 Result := ComplexMultiply(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 3);
end;

procedure TestComplexFunctions.TestComplexDivide32;
var
  A, B   : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexDivide(A, B);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im := -2;

 Result := ComplexDivide(A, B);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexDivide(A, B);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexDivide64;
var
  A, B   : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 A.Re := -1;
 A.Im :=  0;
 B.Re := -2;
 B.Im :=  0;

 Result := ComplexDivide(A, B);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 // imaginary case
 A.Re :=  0;
 A.Im := -1;
 B.Re :=  0;
 B.Im := -2;

 Result := ComplexDivide(A, B);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 // complex case
 A.Re := 1;
 A.Im := 1;
 B.Re := 2;
 B.Im := 2;

 Result := ComplexDivide(A, B);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);

 Result := ComplexDivide(A.Re, A.Im, B.Re, B.Im);
 CheckTrue(Result.Re = 0.5);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexExp32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 // complex case
 Value.Re := 1;
 Value.Im := Pi;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon32);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon32);
 CheckTrue(Result.Im < CEpsilon32);
end;

procedure TestComplexFunctions.TestComplexExp64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re - exp(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

 // complex case
 Value.Re := 1;
 Value.Im := Pi;

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexExp(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
end;

procedure TestComplexFunctions.TestComplexLn32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := exp(1);
 Value.Im := 0;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re - 1) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re - 1) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexLn64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := exp(1);
 Value.Im := 0;

 Result := ComplexLn(Value);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 0);

 Result := ComplexLn(Value);
 CheckTrue(Result.Re = 1);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexLn(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSin32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSin64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re - sin(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqr32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 2;
 Value.Im := 0;

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqr64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 2;
 Value.Im := 0;

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexSqr(Value);
 CheckTrue(abs(Result.Re - 4) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqrt32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 4;
 Value.Im := 0;

 Result := ComplexSqrt(Value);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 Result := ComplexSqrt(Value);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexSqrt64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 4;
 Value.Im := 0;

 Result := ComplexSqrt(Value);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

 Result := ComplexSqrt(Value);
 CheckTrue(Result.Re = 2);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re := -exp(1);
 Value.Im :=  Pi;

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexSin(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexCos32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re - Cos(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re - Cos(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexCos64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re - Cos(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re - Cos(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTan32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTan64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - Tan(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTanh32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexTanh64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);

(*
 // complex case
 Value.Re :=  1;
 Value.Im := Pi;

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);

 Result := ComplexCos(Value);
 CheckTrue(abs(Result.Re + exp(1)) < CEpsilon64);
 CheckTrue(Result.Im < CEpsilon32);
*)
end;

procedure TestComplexFunctions.TestComplexArcCos32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcCos(Value);
 CheckTrue(abs(Result.Re - ArcCos(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcCos64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcCos(Value);
 CheckTrue(abs(Result.Re - ArcCos(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcSin32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcSin(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcSin64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcSin(Value);
 CheckTrue(abs(Result.Re - Tanh(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcTan32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexArcTan(Value);
 CheckTrue(abs(Result.Re - ArcTan(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcTan64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTan(Value);
 CheckTrue(abs(Result.Re - ArcTan(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcTanh32;
var
  Value  : TComplexSingle;
  Result : TComplexSingle;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - ArcTanh(1)) < CEpsilon32);
 CheckTrue(Result.Im = 0);
end;

procedure TestComplexFunctions.TestComplexArcTanh64;
var
  Value  : TComplexDouble;
  Result : TComplexDouble;
begin
 // real case
 Value.Re := 1;
 Value.Im := 0;

 Result := ComplexTanh(Value);
 CheckTrue(abs(Result.Re - ArcTanh(1)) < CEpsilon64);
 CheckTrue(Result.Im = 0);
end;

initialization
  RegisterTest(TestComplexFunctions.Suite);

end.
