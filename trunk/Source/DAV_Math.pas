unit DAV_Math;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Tobias Fleischer and                //
//  Christian-W. Budde                                                        //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2003-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{$IFDEF Darwin}
  {$DEFINE PUREPASCAL} // for OSX use pure pascal code
{$ENDIF}

uses
  DAV_Types;

{ Compatibility }

{$IFDEF DELPHI5}
function Sign(const AValue: Single): Single; overload;
function Sign(const AValue: Double): Double; overload;
{$ENDIF}

{ Math }

function ModZeroBesselI0(Value: Double): Double;
function ModZeroBessel(Value: Double): Double;
function ChebyshevPolynomial(Order, Value : Double): Double;

function RandomGauss: Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function FastRandom: Single;

function Factorial(const Order: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Factorial(const Order: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Factorial(const Order: Integer): Int64; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFNDEF FPC}
function Tanh(const X: Extended): Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
{$ENDIF}
function Tanh(const X: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Tanh(const X: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

{$IFNDEF FPC}
procedure GetSinCos(const Frequency: Extended; out SinValue, CosValue : Extended); overload;
{$ENDIF}
procedure GetSinCos(const Frequency: Double; out SinValue, CosValue : Double); overload;
procedure GetSinCos(const Frequency: Single; out SinValue, CosValue : Single); overload;

function IsPowerOf2(const Value: Integer): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function NextPowerOf2(Value: Integer): Integer; {$IFDEF Purepascal} {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} {$ENDIF}
function PrevPowerOf2(Value: Integer): Integer; {$IFDEF Purepascal} {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} {$ENDIF}
function RoundToPowerOf2(const Value: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function TruncToPowerOf2(const Value: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function ExtendToPowerOf2(const Value: Integer): Integer; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function TruncLog2(Value : Extended): Integer; overload;
function TruncLog2(Value : Integer): Integer; overload;
function CeilLog2(Value : Extended): Integer; overload;
function CeilLog2(Value : Integer): Integer; overload;
function Power2(const X: Extended): Extended;

function IsNan32(const Value: Single): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function IsNan64(const Value: Double): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function Sigmoid(const Input: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Sigmoid(const Input: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Sinc(const Input: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Sinc(const Input: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function EvaluatePolynomial(Coefficients: array of Single; Input: Single): Single; overload;
function EvaluatePolynomial(Coefficients: array of Double; Input: Double): Double; overload;
function EvaluateRational(Nominator, Denominator: array of Single; Input: Single): Double; overload;
function EvaluateRational(Nominator, Denominator: array of Double; Input: Double): Double; overload;

function EvaluatePolynomialRoot1(A, B : Single): Single; overload;
function EvaluatePolynomialRoot1(A, B : Double): Double; overload;
//procedure EvaluatePolynomialRoot2(Coef : PVector; Z : PCompVector); // overload;

procedure Balance(A : PDAVSingleFixedMatrix; LowIndex, HighIndex : Integer;
  out IndexLow, IndexHigh: Integer; Scale: PDAVSingleFixedArray); overload;
procedure Balance(A : PDAVDoubleFixedMatrix; LowIndex, HighIndex : Integer;
  out IndexLow, IndexHigh: Integer; Scale: PDAVDoubleFixedArray); overload;


function RadToDeg(const Radians: Extended): Extended;  { Degrees := Radians * 180 / PI }
function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
function SafeAngle(Angle: Single): Single;
function SolveForX(X, Z: Longint): Longint;
function SolveForY(Y, Z: Longint): Longint;

const
  CTwoMulTwo2Neg32   : Single = ((2.0 / $10000) / $10000);  // 2^-32
  CMinusOneSixteenth : Single = -0.0625;

var
  ln10, ln2, ln22, ln2Rez : Double;
  RandSeed: Longint = 0;

implementation

uses
  Math, SysUtils;

{ Compatibility }

{$IFDEF DELPHI5}
function Sign(const AValue: Single): Single;
begin
 if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000)
  then Result := 0 else
 if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000)
  then Result := -1 else Result := 1;
end;

function Sign(const AValue: Double): Double;
begin
 if ((PInt64(@AValue)^ and $7FFFFFFFFFFFFFFF) = $0000000000000000)
  then Result := 0 else
 if ((PInt64(@AValue)^ and $8000000000000000) = $8000000000000000)
  then Result := -1 else Result := 1;
end;
{$ENDIF}


{ Math }

function ModZeroBesselI0(Value: Double): Double;
const
  P : Array [0..6] of Double = (1.0, 3.5156229, 3.0899424, 1.2067429,
                                0.2659732, 0.360768e-1, 0.45813e-2);
  Q : Array [0..8] of Double = (0.39894228, 0.1328592e-1, 0.225319e-2,
                               -0.157565e-2, 0.916281e-2,  -0.2057706e-1,
                                0.2635537e-1, -0.1647633e-1, 0.392377e-2);
var
  Y, AX, BX: Double;
begin
 if Abs(Value) < 3.75 then
  begin
   Y := Sqr(Value / 3.75);
   Result := P[0] + Y * (P[1] + Y * (P[2] + Y * (P[3] + Y * (P[4] + Y * (P[5] + Y * P[6])))))
  end
 else
  begin
   AX := Abs(Value);
   Y := 3.75 / AX;
   BX := Exp(AX) / Sqrt(AX);
   AX := Q[0] + Y * (Q[1] + Y * (Q[2] + Y * (Q[3] + Y * (Q[4] + Y * (Q[5] + Y * (Q[6] + Y * (Q[7] + Y * Q[8])))))));
   Result := AX * BX
  end
end;

function ModZeroBessel(Value: Double): Double;
var
  h : Double;
  i : LongInt;
begin
 Result := 0;
 h := Value * 0.5;
 for i := 0 to 31
  do Result := Result + Power(2, IntPower(h, i));
end;

function ChebyshevPolynomial(Order, Value : Double): Double;
begin
 if Abs(Value) <= 1
  then Result := Cos(Order * ArcCos(Value))
  else Result := Cosh(Order * ArcCosh(Value));
end;

function FastRandom: Single;
{$IFDEF PUREPASCAL}
begin
 Result := 2 * Random - 1;
end;
{$ELSE}
asm
 IMUL  EDX, RandSeed, 08088405H
 INC   EDX
 MOV   RandSeed, EDX
 FLD   CTwoMulTwo2Neg32
 PUSH  0
 PUSH  EDX
 FILD  qword ptr [ESP]
 ADD   ESP, 8
 FMULP ST(1), ST(0)
 FLD1
 FSUBP ST, ST
end;
{$ENDIF}

function RandomGauss: Extended;
var
  U1, S2: Extended;
begin
  repeat
    U1 := FastRandom;
    S2 := Sqr(U1) + Sqr(FastRandom);
  until S2 < 1;
  Result := Sqrt(CMinusOneSixteenth * Ln(S2) / S2) * U1;
end;

function Factorial(const Order : Single): Single;
var
  i : Integer;
begin
 Result := 1;
 for i := 2 to Round(Order)
  do Result := Result * i;
end;

function Factorial(const Order : Double): Double;
var
  i : Integer;
begin
 Result := 1;
 for i := 2 to Round(Order)
  do Result := Result * i;
end;

function Factorial(const Order : Integer): Int64;
var
  i : Integer;
begin
 Result := 1;
 for i := 2 to Order
  do Result := Result * i;
end;

{$IFNDEF FPC}
function Tanh(const X: Extended): Extended;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;
{$ENDIF}

function Tanh(const X: Double): Double;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;

function Tanh(const X: Single): Single;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;

{$IFNDEF FPC}
procedure GetSinCos(const Frequency: Extended; out SinValue, CosValue : Extended);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
end;
{$ELSE}
asm
  fld Frequency;
  fsincos
  fstp    tbyte ptr [edx]    // Cos
  fstp    tbyte ptr [eax]    // Sin
end;
{$ENDIF}
{$ENDIF}

procedure GetSinCos(const Frequency: Double; out SinValue, CosValue : Double);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
end;
{$ELSE}
asm
 fld Frequency.Double;
 fsincos
 fstp [CosValue].Double;
 fstp [SinValue].Double;
end;
{$ENDIF}

procedure GetSinCos(const Frequency: Single; out SinValue, CosValue : Single);
{$IFDEF PUREPASCAL}
begin
 SinValue := Sin(Frequency);
 CosValue := Cos(Frequency);
end;
{$ELSE}
asm
 fld Frequency;
 fsincos
 fstp [CosValue].Single;
 fstp [SinValue].Single;
end;
{$ENDIF}

function IsPowerOf2(const Value: Integer): Boolean;
//returns true when X = 1,2,4,8,16 etc.
begin
  Result := Value and (Value - 1) = 0;
end;

function PrevPowerOf2(Value: Integer): Integer;
//returns X rounded down to the power of two
{$IFNDEF TARGET_x86}
begin
  Result := 1;
  while Value shr 1 > 0 do
    Result := Result shl 1;
{$ELSE}
asm
 bsr ecx, eax
 shr eax, cl
 shl eax, cl
{$ENDIF}
end;

function NextPowerOf2(Value: Integer): Integer;
//returns X rounded up to the power of two, i.e. 5 -> 8, 7 -> 8, 15 -> 16
{$IFDEF PUREPASCAL}
begin
  Result := 2;
  while Value shr 1 > 0 do
    Result := Result shl 1;
{$ELSE}
asm
 dec eax
 jle @1
 bsr ecx, eax
 mov eax, 2
 shl eax, cl
 ret
@1:
 mov eax, 1
{$ENDIF}
end;

function RoundToPowerOf2(const Value: Integer): Integer;
begin
 Result := round(Log2(Value));
 Result := (Value shr (Result - 1)) shl (Result - 1);
end;

function TruncToPowerOf2(const Value: Integer): Integer;
begin
 result := 1;
 while result <= value do result := result shl 1;
 result := result shr 1;
end;

function ExtendToPowerOf2(const Value: Integer): Integer;
begin
 result := 1;
 while result < value do result := result shl 1;
end;

function TruncLog2(Value : Extended): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value));
end;
{$ELSE}
asm
 fld Value.Extended
 fxtract
 fstp st(0)
 fistp result.Integer
end;
{$ENDIF}

function TruncLog2(Value : Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value));
end;
{$ELSE}
var
  temp : Integer;
asm
 mov temp, Value;
 fild temp.Integer
 fxtract
 fstp st(0)
 fistp result.Integer
end;
{$ENDIF}

function CeilLog2(Value : Extended): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value) + 1);
end;
{$ELSE}
asm
 fld   Value.Extended
 fld1
 fsubp st, st
 fxtract
 fstp  st(0)
 fld1
 faddp st(1), st(0)
 fistp result.Integer
end;
{$ENDIF}

function CeilLog2(Value : Integer): Integer;
{$IFDEF PUREPASCAL}
begin
 result := round(log2(Value) + 1);
end;
{$ELSE}
var
  temp : Integer;
asm
 dec Value
 mov temp, Value;
 fild temp.Integer
 fxtract
 fstp st(0)
 fistp result.Integer
 inc result
end;
{$ENDIF}

function Power2(const X: Extended): Extended;
{$IFDEF PUREPASCAL}
begin
 Result := Power(2, X);
{$ELSE}
asm
 FLD     X
 FLD     ST(0)       { i := round(y);     }
 FRNDINT
 FSUB    ST(1), ST   { f := y - i;        }
 FXCH    ST(1)       { z := 2**f          }
 F2XM1
 FLD1
 FADD
 FSCALE              { Result := z * 2**i }
 FSTP    ST(1)
 {$ENDIF}
end;

// IsNan

function IsNan32(const Value: Single): Boolean;
begin
 Result := ((PCardinal(@Value)^ and $7F800000)  = $7F800000) and
           ((PCardinal(@Value)^ and $007FFFFF) <> $00000000);
end;

function IsNan64(const Value: Double): Boolean;
begin
  Result := ((PInt64(@Value)^ and $7FF0000000000000)  = $7FF0000000000000) and
            ((PInt64(@Value)^ and $000FFFFFFFFFFFFF) <> $0000000000000000);
end;


// SINC Function
function Sinc(const Input: Double): Double;
var
  pix : Double;
begin
 if (Input = 0)
  then result := 1
  else
   begin
    pix := PI * Input;
    result := sin(pix) / pix;
   end;
end;

function Sinc(const Input: Single): Single;
var
  pix : Double;
begin
 if (Input = 0)
  then result := 1
  else
   begin
    pix := PI * Input;
    result := sin(pix) / pix;
   end;
end;

function Sigmoid(const Input: Single): Single;
begin
 if (abs(Input) < 1)
  then Result := Input * (1.5 - 0.5 * Input * Input)
  else
   if Input < 0
    then Result := -1
    else Result :=  1;
end;

function Sigmoid(const Input: Double): Double;
begin
 if (abs(Input) < 1)
  then Result := Input * (1.5 - 0.5 * Input * Input)
  else
   if Input < 0
    then Result := -1
    else Result :=  1;
end;

function EvaluatePolynomial(Coefficients: array of Single; Input: Single): Single;
var
  i : Integer;
begin
 Result := Coefficients[0];
 i := 1;

 while i < Length(Coefficients) do
  begin
    Result := Result * Input + Coefficients[i];
    inc(i);
  end;
end;

function EvaluatePolynomial(Coefficients: array of Double; Input: Double): Double;
var
  i : Integer;
begin
 Result := Coefficients[0];
 i := 1;

 while i < Length(Coefficients) do
  begin
    Result := Result * Input + Coefficients[i];
    inc(i);
  end;
end;

function EvaluateRational(Nominator, Denominator: array of Single; Input: Single): Double; overload;
begin
 Result := EvaluatePolynomial(Nominator, Input) / EvaluatePolynomial(Denominator, Input);
end;

function EvaluateRational(Nominator, Denominator: array of Double; Input: Double): Double; overload;
begin
 Result := EvaluatePolynomial(Nominator, Input) / EvaluatePolynomial(Denominator, Input);
end;

function EvaluatePolynomialRoot1(A, B : Single): Single;
begin
 if B <> 0 then
  if A <> 0
   then Result := -A / B
   else Result := 0
 else
  if A = 0
   then raise Exception.Create('X is undetermined (A = B = 0)')
   else raise Exception.Create('no solution (A <> 0, B = 0)');
end;

function EvaluatePolynomialRoot1(A, B : Double): Double;
begin
 if B <> 0 then
  if A <> 0
   then Result := -A / B
   else Result := 0
 else
  if A = 0
   then raise Exception.Create('X is undetermined (A = B = 0)')
   else raise Exception.Create('no solution (A <> 0, B = 0)');
end;

(*
function EvaluatePolynomialRoot2(Coef: PDAVSingleFixedArray; Z : PCompVector) : Integer;
var
  Delta, F, Q : Float;

begin
  Z^[1].X := 0.0; Z^[1].Y := 0.0;
  Z^[2].X := 0.0; Z^[2].Y := 0.0;

  if Coef^[2] = 0.0 then
    begin
      RootPol2 := RootPol1(Coef^[0], Coef^[1], Z^[1].X);
      Exit;
    end;

  if Coef^[0] = 0.0 then
    begin
      { 0 is root. Eq. becomes linear }
      if RootPol1(Coef^[1], Coef^[2], Z^[1].X) = 1 then
        { Linear eq. has 1 solution }
        RootPol2 := 2
      else
        { Linear eq. is undetermined or impossible }
        RootPol2 := 1;
      Exit;
    end;

  Delta := Sqr(Coef^[1]) - 4.0 * Coef^[0] * Coef^[2];

  { 2 real roots }
  if Delta > 0.0 then
    begin
      RootPol2 := 2;

      { Algorithm for minimizing roundoff errors }
      { See `Numerical Recipes'                  }
      if Coef^[1] >= 0.0 then
        Q := - 0.5 * (Coef^[1] + Sqrt(Delta))
      else
        Q := - 0.5 * (Coef^[1] - Sqrt(Delta));

      Z^[1].X := Q / Coef^[2];
      Z^[2].X := Coef^[0] / Q;

      Exit;
    end;

  { Double real root }
  if Delta = 0.0 then
    begin
      RootPol2 := 2;
      Z^[1].X := - 0.5 * Coef^[1] / Coef^[2];
      Z^[2].X := Z^[1].X;
      Exit;
    end;

  { 2 complex roots }
  RootPol2 := 0;
  F := 0.5 / Coef^[2];
  Z^[1].X := - F * Coef^[1];
  Z^[1].Y := Abs(F) * Sqrt(- Delta);
  Z^[2].X := Z^[1].X;
  Z^[2].Y := - Z^[1].Y;
end;
*)


////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  The procedures below balances a real matrix and isolates eigenvalues      //
//  whenever possible.                                                        //
//                                                                            //
//  On input:                                                                 //
//                                                                            //
//    A contains the input matrix to be balanced.                             //
//                                                                            //
//    LowIndex, HighIndex are the lowest and highest indices of the elements  //
//    of A.                                                                   //
//                                                                            //
//  On output:                                                                //
//                                                                            //
//    A contains the balanced matrix.                                         //
//                                                                            //
//    IndexLow and IndexHigh are two integers such that A[i,j] is equal to    //
//    zero if                                                                 //
//      (1) i is greater than j and                                           //
//      (2) j=LowIndex,...,IndexLow-1 or i=IndexHigh+1,...,HighIndex.         //
//                                                                            //
//    Scale contains information determining the permutations and scaling     //
//    factors used.                                                           //
//                                                                            //
//    Suppose that the principal submatrix in rows IndexLow through IndexHigh //
//    has been balanced, that P[j] denotes the index interchanged             //
//    with j during the permutation step, and that the elements               //
//    of the diagonal matrix used are denoted by D[i,j].  then                //
//        Scale[j] = P[j],    for j = LowIndex,...,IndexLow-1                 //
//                 = D[j,j],      j = IndexLow,...,IndexHigh                  //
//                 = P[j]         j = IndexHigh+1,...,HighIndex.              //
//    the order in which the interchanges are made is                         //
//    HighIndex to IndexHigh+1, then LowIndex to IndexLow-1.                  //
//                                                                            //
//    Note that LowIndex is returned for IndexHigh if IndexHigh is < LowIndex //
//    formally                                                                //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

procedure Balance(A : PDAVSingleFixedMatrix; LowIndex, HighIndex : Integer;
  out IndexLow, IndexHigh: Integer; Scale: PDAVSingleFixedArray);

const
  CRadix = 2;  // Base used in floating number representation

var
  I, J, M           : Integer;
  C, F, G, R, S, B2 : Single;
  Flag, Found, Conv : Boolean;

  procedure Exchange;
  // Row and column exchange
  var
    I : Integer;
  begin
    Scale^[M] := J;
    if J = M then Exit;

    for I := LowIndex to IndexHigh do
     begin
      F := A^[I, J];
      A^[I, J] := A^[I, M];
      A^[I, M] := F;
     end;

    for I := IndexLow to HighIndex do
     begin
      F := A^[J, I];
      A^[J, I] := A^[M, I];
      A^[M, I] := F;
     end;
  end;

begin
 B2 := Sqr(CRadix);
 IndexLow := LowIndex;
 IndexHigh := HighIndex;

 // Search for rows isolating an eigenvalue and push them down
 repeat
  J := IndexHigh;
  repeat
   I := LowIndex;
   repeat
    Flag := (I <> J) and (A^[J, I] <> 0.0);
    I := I + 1;
   until Flag or (I > IndexHigh);
   Found := not Flag;
   if Found then
    begin
     M := IndexHigh;
     Exchange;
     IndexHigh := IndexHigh - 1;
    end;
   J := J - 1;
  until Found or (J < LowIndex);
 until (not Found) or (IndexHigh < LowIndex);

 if IndexHigh < LowIndex then IndexHigh := LowIndex;
 if IndexHigh = LowIndex then Exit;

 // Search for columns isolating an eigenvalue and push them left
 repeat
  J := IndexLow;
  repeat
   I := IndexLow;
   repeat
    Flag := (I <> J) and (A^[I, J] <> 0.0);
    I := I + 1;
   until Flag or (I > IndexHigh);
   Found := not Flag;
   if Found then
    begin
     M := IndexLow;
     Exchange;
     IndexLow := IndexLow + 1;
    end;
   J := J + 1;
  until Found or (J > IndexHigh);
 until (not Found);

 // Now balance the submatrix in rows IndexLow to IndexHigh
 for I := IndexLow to IndexHigh
  do Scale^[I] := 1.0;

 // Iterative loop for norm reduction
 repeat
  Conv := True;

  for I := IndexLow to IndexHigh do
   begin
    C := 0.0;
    R := 0.0;

    for J := IndexLow to IndexHigh do
     if J <> I then
      begin
       C := C + Abs(A^[J, I]);
       R := R + Abs(A^[I, J]);
     end;

    // Guard against zero C or R due to underflow
    if (C <> 0.0) and (R <> 0.0) then
     begin
      G := R / CRadix;
      F := 1.0;
      S := C + R;

      while C < G do
       begin
        F := F * CRadix;
        C := C * B2;
       end;

      G := R * CRadix;

      while C >= G do
       begin
        F := F / CRadix;
        C := C / B2;
       end;

      // Now balance
      if (C + R) / F < 0.95 * S then
       begin
        G := 1.0 / F;
        Scale^[I] := Scale^[I] * F;
        Conv := False;
        for J := IndexLow to HighIndex do A^[I, J] := A^[I, J] * G;
        for J := LowIndex to IndexHigh do A^[J, I] := A^[J, I] * F;
       end;
     end;
   end;
 until Conv;
end;

procedure Balance(A : PDAVDoubleFixedMatrix; LowIndex, HighIndex : Integer;
  out IndexLow, IndexHigh: Integer; Scale: PDAVDoubleFixedArray);

const
  CRadix = 2;  // Base used in floating number representation

var
  I, J, M           : Integer;
  C, F, G, R, S, B2 : Double;
  Flag, Found, Conv : Boolean;

  procedure Exchange;
  // Row and column exchange
  var
    I : Integer;
  begin
    Scale^[M] := J;
    if J = M then Exit;

    for I := LowIndex to IndexHigh do
     begin
      F := A^[I, J];
      A^[I, J] := A^[I, M];
      A^[I, M] := F;
     end;

    for I := IndexLow to HighIndex do
     begin
      F := A^[J, I];
      A^[J, I] := A^[M, I];
      A^[M, I] := F;
     end;
  end;

begin
 B2 := Sqr(CRadix);
 IndexLow := LowIndex;
 IndexHigh := HighIndex;

 // Search for rows isolating an eigenvalue and push them down
 repeat
  J := IndexHigh;
  repeat
   I := LowIndex;
   repeat
    Flag := (I <> J) and (A^[J, I] <> 0.0);
    I := I + 1;
   until Flag or (I > IndexHigh);
   Found := not Flag;
   if Found then
    begin
     M := IndexHigh;
     Exchange;
     IndexHigh := IndexHigh - 1;
    end;
   J := J - 1;
  until Found or (J < LowIndex);
 until (not Found) or (IndexHigh < LowIndex);

 if IndexHigh < LowIndex then IndexHigh := LowIndex;
 if IndexHigh = LowIndex then Exit;

 // Search for columns isolating an eigenvalue and push them left
 repeat
  J := IndexLow;
  repeat
   I := IndexLow;
   repeat
    Flag := (I <> J) and (A^[I, J] <> 0.0);
    I := I + 1;
   until Flag or (I > IndexHigh);
   Found := not Flag;
   if Found then
    begin
     M := IndexLow;
     Exchange;
     IndexLow := IndexLow + 1;
    end;
   J := J + 1;
  until Found or (J > IndexHigh);
 until (not Found);

 // Now balance the submatrix in rows IndexLow to IndexHigh
 for I := IndexLow to IndexHigh
  do Scale^[I] := 1.0;

 // Iterative loop for norm reduction
 repeat
  Conv := True;

  for I := IndexLow to IndexHigh do
   begin
    C := 0.0;
    R := 0.0;

    for J := IndexLow to IndexHigh do
     if J <> I then
      begin
       C := C + Abs(A^[J, I]);
       R := R + Abs(A^[I, J]);
     end;

    // Guard against zero C or R due to underflow
    if (C <> 0.0) and (R <> 0.0) then
     begin
      G := R / CRadix;
      F := 1.0;
      S := C + R;

      while C < G do
       begin
        F := F * CRadix;
        C := C * B2;
       end;

      G := R * CRadix;

      while C >= G do
       begin
        F := F / CRadix;
        C := C / B2;
       end;

      // Now balance
      if (C + R) / F < 0.95 * S then
       begin
        G := 1.0 / F;
        Scale^[I] := Scale^[I] * F;
        Conv := False;
        for J := IndexLow to HighIndex do A^[I, J] := A^[I, J] * G;
        for J := LowIndex to IndexHigh do A^[J, I] := A^[J, I] * F;
       end;
     end;
   end;
 until Conv;
end;

function RadToDeg(const Radians: Extended): Extended;
// Degrees := Radians * 180 / PI
const
  DegPi : Double = (180 / PI);
begin
  Result := Radians * DegPi;
end;

function RelativeAngle(X1, Y1, X2, Y2: Integer): Single;
const
  MulFak = 180 / Pi;
begin
  Result := ArcTan2(X2 - X1, Y1 - Y2) * MulFak;
end;

function SafeAngle(Angle: Single): Single;
begin
  while Angle < 0 do Angle := Angle + 360;
  while Angle >= 360 do Angle := Angle - 360;
  Result := Angle;
end;

function SolveForX(X, Z: Longint): Longint;
// This function solves for Re in the equation "x is y% of z".
begin
  Result := Round(Z * (X * 0.01));//tt
end;

function SolveForY(Y, Z: Longint): Longint;
// This function solves for Im in the equation "x is y% of z".
begin
  if Z = 0 then Result := 0 else Result := Round((Y * 100.0) / Z); //t
end;


procedure InitConstants;
begin
 ln2      := ln(2);
 ln22     := ln2 * 0.5;
 ln2Rez   := 1 / ln2;
 ln10     := ln(10);
 Randomize;
 RandSeed := Random(MaxInt);
end;

initialization
  InitConstants;

end.
