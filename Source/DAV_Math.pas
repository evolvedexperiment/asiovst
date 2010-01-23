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

function Tanh(const X: Extended): Extended; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Tanh(const X: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Tanh(const X: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure GetSinCos(const Frequency: Double; out SinValue, CosValue : Double); overload;
procedure GetSinCos(const Frequency: Extended; out SinValue, CosValue : Extended); overload;
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

function Sigmoid(const Input: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Sigmoid(const Input: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Sinc(const Input: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Sinc(const Input: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function EvaluatePolynomial(Coefficients: array of Single; Input: Single): Single; overload;
function EvaluatePolynomial(Coefficients: array of Double; Input: Double): Double; overload;
function EvaluateRational(Nominator, Denominator: array of Single; Input: Single): Double; overload;
function EvaluateRational(Nominator, Denominator: array of Double; Input: Double): Double; overload;

const
  CTwoMulTwo2Neg32   : Single = ((2.0 / $10000) / $10000);  // 2^-32
  CMinusOneSixteenth : Single = -0.0625;

var
  ln10, ln2, ln22, ln2Rez : Double;
  RandSeed: Longint = 0;

implementation

uses
  Math;

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


{$IFDEF PUREPASCAL}

function FastRandom: Single;
begin
 Result := 2 * Random - 1;
end;

{$ELSE}

function FastRandom: Single;
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
 FSUBP
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

function Tanh(const X: Extended): Extended;
var
  ep : Extended;
begin
 ep := Exp(2 * X);
 Result := (ep - 1) / (ep + 1);
end;

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
 fsubp
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
