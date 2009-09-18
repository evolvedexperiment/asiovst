unit DAV_Complex;

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
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}
{$IFDEF FPC}
{$DEFINE PUREPASCAL}
{$ENDIF}

type
  PComplexSingle = ^TComplexSingle;

  TComplexSingle = record
    Re: Single;
    Im: Single;
  end;

  PComplexDouble = ^TComplexDouble;

  TComplexDouble = record
    Re: Double;
    Im: Double;
  end;

  PDAVComplexSingleDynArray = ^TDAVComplexSingleDynArray;
  TDAVComplexSingleDynArray = array of TComplexSingle;

  PDAVComplexDoubleDynArray = ^TDAVComplexDoubleDynArray;
  TDAVComplexDoubleDynArray = array of TComplexDouble;

  PDAVComplexSingleFixedArray = ^TDAVComplexSingleFixedArray;
  TDAVComplexSingleFixedArray = array [0..0] of TComplexSingle;

  PDAVComplexDoubleFixedArray = ^TDAVComplexDoubleFixedArray;
  TDAVComplexDoubleFixedArray = array [0..0] of TComplexDouble;

  PDAV2ComplexSingleArray = ^TDAV2ComplexSingleArray;
  TDAV2ComplexSingleArray = array [0..1] of TComplexSingle;
  PDAV4ComplexSingleArray = ^TDAV4ComplexSingleArray;
  TDAV4ComplexSingleArray = array [0..3] of TComplexSingle;

  PDAV2ComplexDoubleArray = ^TDAV2ComplexDoubleArray;
  TDAV2ComplexDoubleArray = array [0..1] of TComplexDouble;
  PDAV4ComplexDoubleArray = ^TDAV4ComplexDoubleArray;
  TDAV4ComplexDoubleArray = array [0..3] of TComplexDouble;

function Complex(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function Complex(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexPolar(const Magnitude, Angle: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexPolar(const Magnitude, Angle: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSign(const A: TComplexSingle): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign(const A: TComplexDouble): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign(const Re, Im: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign(const Re, Im: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexConjugate(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate(const a: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate(const a: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexInvert(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert(const a: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert(const a: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexMagnitude(const Re, Im: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude(const Re, Im: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude(const Complex: TComplexDouble): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude(const Complex: TComplexSingle): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArgument(const Re, Im: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument(const Re, Im: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument(const Complex: TComplexDouble): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument(const Complex: TComplexSingle): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexLog10(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog10(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog10(const Complex: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog10(const Complex: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexAdd(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd(const ARe, AIm, BRe, BIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd(const ARe, AIm, BRe, BIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexAddInplace(var A: TComplexSingle; const B: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace(var A: TComplexDouble; const B: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSubtract(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract(const ARe, AIm, BRe, BIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract(const ARe, AIm, BRe, BIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexSubtractInplace(var A: TComplexSingle; const B: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace(var A: TComplexDouble; const B: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexMultiply(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMultiply(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMultiply(const ARe, AIm, BRe, BIm: Single): TComplexSingle; overload;
function ComplexMultiply(const ARe, AIm, BRe, BIm: Double): TComplexDouble; overload;

procedure ComplexMultiplyInplace(var A: TComplexSingle; const B: TComplexSingle); overload;
procedure ComplexMultiplyInplace(var A: TComplexDouble; const B: TComplexDouble); overload;
procedure ComplexMultiplyInplace(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexMultiplyInplace(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexMultiply2Inplace(var A: TComplexSingle; const B: TComplexSingle); overload;
procedure ComplexMultiply2Inplace(var A: TComplexDouble; const B: TComplexDouble); overload;

function ComplexDivide(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide(const ARe, AIm, BRe, BIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide(const ARe, AIm, BRe, BIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexDivideInplace(var A: TComplexSingle; const B: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace(var A: TComplexDouble; const B: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexReciprocal(const A: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal(const A: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal(const ARe, AIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal(const ARe, AIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexReciprocalInplace(var A: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace(var A: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace(var ARe, AIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace(var ARe, AIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSqr(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr(const a: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr(const a: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSqrt(const Re, Im: Single): TComplexSingle; overload;
function ComplexSqrt(const Re, Im: Double): TComplexDouble; overload;
function ComplexSqrt(const a: TComplexSingle): TComplexSingle; overload;
function ComplexSqrt(const a: TComplexDouble): TComplexDouble; overload;

function ComplexExp(const Re, Im: Single): TComplexSingle; overload;
function ComplexExp(const Re, Im: Double): TComplexDouble; overload;
function ComplexExp(const a: TComplexSingle): TComplexSingle; overload;
function ComplexExp(const a: TComplexDouble): TComplexDouble; overload;

implementation

uses
  Math {$IFDEF Delphi5}, DAV_Common{$ENDIF};

function Complex(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := Re;
  Result.Im := Im;
end;

function Complex(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := Re;
  Result.Im := Im;
end;


function ComplexPolar(const Magnitude, Angle: Single): TComplexSingle;
begin
  Result.Re := Magnitude * cos(Angle);
  Result.Im := Magnitude * sin(Angle);
end;

function ComplexPolar(const Magnitude, Angle: Double): TComplexDouble;
begin
  Result.Re := Magnitude * cos(Angle);
  Result.Im := Magnitude * sin(Angle);
end;

function ComplexSign(const A: TComplexSingle): Single;
begin
  if (A.Re >= 0) and (A.Im > 0) then
    Result := 1
  else
  if (A.Re <= 0) and (A.Im < 0) then
    Result := -1
  else
    Result := sign(A.Re);
end;

function ComplexSign(const A: TComplexDouble): Double;
begin
  if (A.Re >= 0) and (A.Im > 0) then
    Result := 1
  else
  if (A.Re <= 0) and (A.Im < 0) then
    Result := -1
  else
    Result := sign(A.Re);
end;

function ComplexSign(const Re, Im: Single): Single;
begin
  if (Re >= 0) and (Im > 0) then
    Result := 1
  else
  if (Re <= 0) and (Im < 0) then
    Result := -1
  else
    Result := sign(Re);
end;

function ComplexSign(const Re, Im: Double): Double;
begin
  if (Re >= 0) and (Im > 0) then
    Result := 1
  else
  if (Re <= 0) and (Im < 0) then
    Result := -1
  else
    Result := sign(Re);
end;

function ComplexConjugate(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := Re;
  Result.Im := -Im;
end;

function ComplexConjugate(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := Re;
  Result.Im := -Im;
end;


function ComplexConjugate(const a: TComplexSingle): TComplexSingle;
begin
  Result.Re := a.Re;
  Result.Im := -a.Im;
end;

function ComplexConjugate(const a: TComplexDouble): TComplexDouble;
begin
  Result.Re := a.Re;
  Result.Im := -a.Im;
end;


function ComplexInvert(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert(const a: TComplexSingle): TComplexSingle;
begin
  Result.Re := -a.Re;
  Result.Im := -a.Im;
end;

function ComplexInvert(const a: TComplexDouble): TComplexDouble;
begin
  Result.Re := -a.Re;
  Result.Im := -a.Im;
end;


function ComplexLog10(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := Log10((sqr(Re) + Sqr(Im)));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLog10(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := Log10((sqr(Re) + Sqr(Im)));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLog10(const Complex: TComplexSingle): TComplexSingle;
begin
  Result.Re := Log10((sqr(Complex.Re) + Sqr(Complex.Im)));
  Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexLog10(const Complex: TComplexDouble): TComplexDouble;
begin
  Result.Re := Log10((sqr(Complex.Re) + Sqr(Complex.Im)));
  Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexMagnitude(const Re, Im: Single): Single;
begin
  Result := hypot(Re, Im);
end;

function ComplexMagnitude(const Re, Im: Double): Double;
begin
  Result := hypot(Re, Im);
end;

function ComplexMagnitude(const Complex: TComplexDouble): Double;
begin
  Result := hypot(Complex.Re, Complex.Im);
end;

function ComplexMagnitude(const Complex: TComplexSingle): Single;
begin
  Result := hypot(Complex.Re, Complex.Im);
end;

function ComplexArgument(const Re, Im: Single): Single;
begin
  Result := ArcTan2(Im, Re);
end;

function ComplexArgument(const Re, Im: Double): Double;
begin
  Result := ArcTan2(Im, Re);
end;

function ComplexArgument(const Complex: TComplexDouble): Double;
begin
  Result := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexArgument(const Complex: TComplexSingle): Single;
begin
  Result := ArcTan2(Complex.Im, Complex.Re);
end;


function ComplexAdd(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
begin
  Result.Re := ARe + BRe;
  Result.Im := AIm + BIm;
end;

function ComplexAdd(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
begin
  Result.Re := ARe + BRe;
  Result.Im := AIm + BIm;
end;

function ComplexAdd(const A, B: TComplexSingle): TComplexSingle;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

function ComplexAdd(const A, B: TComplexDouble): TComplexDouble;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace(var A: TComplexSingle; const B: TComplexSingle);
begin
  A.Re := A.Re + B.Re;
  A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace(var A: TComplexDouble; const B: TComplexDouble);
begin
  A.Re := A.Re + B.Re;
  A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace(var ARe, AIm: Single; const BRe, BIm: Single);
begin
  ARe := ARe + BRe;
  AIm := AIm + BIm;
end;

procedure ComplexAddInplace(var ARe, AIm: Double; const BRe, BIm: Double);
begin
  ARe := ARe + BRe;
  AIm := AIm + BIm;
end;


function ComplexSubtract(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
begin
  Result.Re := ARe - BRe;
  Result.Im := AIm - BIm;
end;

function ComplexSubtract(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
begin
  Result.Re := ARe - BRe;
  Result.Im := AIm - BIm;
end;

function ComplexSubtract(const A, B: TComplexSingle): TComplexSingle;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

function ComplexSubtract(const A, B: TComplexDouble): TComplexDouble;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace(var A: TComplexSingle; const B: TComplexSingle);
begin
  A.Re := A.Re - B.Re;
  A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace(var A: TComplexDouble; const B: TComplexDouble);
begin
  A.Re := A.Re - B.Re;
  A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace(var ARe, AIm: Single; const BRe, BIm: Single);
begin
  ARe := ARe - BRe;
  AIm := AIm - BIm;
end;

procedure ComplexSubtractInplace(var ARe, AIm: Double; const BRe, BIm: Double);
begin
  ARe := ARe - BRe;
  AIm := AIm - BIm;
end;


function ComplexMultiply(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
begin
  Result.Re := ARe * BRe - AIm * BIm;
  Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
begin
  Result.Re := ARe * BRe - AIm * BIm;
  Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply(const A, B: TComplexSingle): TComplexSingle;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

function ComplexMultiply(const A, B: TComplexDouble): TComplexDouble;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

procedure ComplexMultiplyInplace(var A: TComplexSingle;
  const B: TComplexSingle);
{$IFDEF PUREPASCAL}
var
  Temp: Single;
begin
  Temp := A.Re;
  A.Re := A.Re * B.Re - A.Im * B.Im;
  A.Im := A.Im * B.Re + Temp * B.Im;
end;
{$ELSE}
asm
 fld A.Re.Single    // A.Re
 fld A.Im.Single    // A.Im, A.Re
 fld B.Re.Single    // B.Re, A.Im, A.Re
 fld B.Im.Single    // B.Im, B.Re, A.Im, A.Re
 fld st(3)          // A.Re, B.Im, B.Re, A.Im, A.Re
 fmul st(0), st(2)  // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fld st(3)          // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fmul st(0), st(2)  // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fsubp              // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
 fstp A.Re.Single   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
 fxch st(2)         // A.Im, B.Re, B.Im, A.Re
 fmulp              // A.Im * B.Re, B.Im, A.Re
 fxch st(2)         // B.Im, A.Re, A.Im * B.Re
 fmulp              // B.Im * A.Re, A.Im * B.Re
 faddp              // A.Im * B.Re + A.Re * B.Im
 fstp A.Im.Single   // A.Im := A.Im * B.Re + A.Re * B.Im
end;
{$ENDIF}

procedure ComplexMultiplyInplace(var A: TComplexDouble;
  const B: TComplexDouble);
{$IFDEF PUREPASCAL}
var
  Temp: Double;
begin
  Temp := A.Re;
  A.Re := A.Re * B.Re - A.Im * B.Im;
  A.Im := A.Im * B.Re + Temp * B.Im;
end;
{$ELSE}
asm
 fld A.Re.Double    // A.Re
 fld A.Im.Double    // A.Im, A.Re
 fld B.Re.Double    // B.Re, A.Im, A.Re
 fld B.Im.Double    // B.Im, B.Re, A.Im, A.Re
 fld st(3)          // A.Re, B.Im, B.Re, A.Im, A.Re
 fmul st(0), st(2)  // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fld st(3)          // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fmul st(0), st(2)  // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
 fsubp              // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
 fstp A.Re.Double   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
 fxch st(2)         // A.Im, B.Re, B.Im, A.Re
 fmulp              // A.Im * B.Re, B.Im, A.Re
 fxch st(2)         // B.Im, A.Re, A.Im * B.Re
 fmulp              // B.Im * A.Re, A.Im * B.Re
 faddp              // A.Im * B.Re + A.Re * B.Im
 fstp A.Im.Double   // A.Im := A.Im * B.Re + A.Re * B.Im
end;
{$ENDIF}

procedure ComplexMultiply2Inplace(var A: TComplexSingle;
  const B: TComplexSingle);
{$IFDEF PUREPASCAL}
var
  Temp: Single;
begin
  Temp := A.Re;
  A.Re := A.Re * (sqr(B.Re) - sqr(B.Im)) - 2 * A.Im * B.Im * B.Re;
  A.Im := A.Im * (sqr(B.Re) - sqr(B.Im)) + 2 * Temp * B.Im * B.Re;
end;
{$ELSE}
asm
 fld  B.Re          // B.Re
 fld  B.Im          // B.Im, B.Re
 fmulp              // B.Im * B.Re
 fadd st(0), st(0)  // 2 * B.Im * B.Re = B''

 fld  B.Re          // B.Re, B''
 fmul st(0), st(0)  // B.Re�, B''
 fld  B.Im          // B.Im, B.Re�, B''
 fmul st(0), st(0)  // B.Im�, B.Re�, B''
 fsubp              // B.Im� + B.Re� = B', B''

 fld A.Re           // A.Re, B', B''
 fmul st(0), st(1)  // A.Re * B', B', B''
 fld A.Im           // A.Im, A.Re * B', B', B''
 fmul st(0), st(3)  // A.Im * B'', A.Re * B', B', B''
 fsubp              // A.Re * B' - A.Im * B'' := New A.Re, B', B''

 fxch st(2)         // B'', B', New A.Re
 fmul A.Re          // A.Re * B'', B', New A.Re
 fxch st(1)         // B', A.Re * B'', New A.Re
 fmul A.Im          // A.Im * B', A.Re * B'', New A.Re
 faddp              // A.Im * B' + A.Re * B'' := New A.Im, New A.Re
 fstp A.Im          // New A.Re
 fstp A.Re
end;
{$ENDIF}

procedure ComplexMultiply2Inplace(var A: TComplexDouble;
  const B: TComplexDouble);
{$IFDEF PUREPASCAL}
var
  Btmp : Double;
  Temp : Double;
begin
  Btmp := (sqr(B.Re) - sqr(B.Im)); Temp := A.Re;
  A.Re := A.Re * Btmp - 2 * A.Im * B.Im * B.Re;
  A.Im := A.Im * Btmp + 2 * Temp * B.Im * B.Re;
end;
{$ELSE}
asm
 fld  B.Re          // B.Re
 fld  B.Im          // B.Im, B.Re
 fmulp              // B.Im * B.Re
 fadd st(0), st(0)  // 2 * B.Im * B.Re = B''

 fld  B.Re          // B.Re, B''
 fmul st(0), st(0)  // B.Re�, B''
 fld  B.Im          // B.Im, B.Re�, B''
 fmul st(0), st(0)  // B.Im�, B.Re�, B''
 fsubp              // B.Im� + B.Re� = B', B''

 fld A.Re           // A.Re, B', B''
 fmul st(0), st(1)  // A.Re * B', B', B''
 fld A.Im           // A.Im, A.Re * B', B', B''
 fmul st(0), st(3)  // A.Im * B'', A.Re * B', B', B''
 fsubp              // A.Re * B' - A.Im * B'' := New A.Re, B', B''

 fxch st(2)         // B'', B', New A.Re
 fmul A.Re          // A.Re * B'', B', New A.Re
 fxch st(1)         // B', A.Re * B'', New A.Re
 fmul A.Im          // A.Im * B', A.Re * B'', New A.Re
 faddp              // A.Im * B' + A.Re * B'' := New A.Im, New A.Re
 fstp A.Im          // New A.Re
 fstp A.Re       
end;
{$ENDIF}

procedure ComplexMultiplyInplace(var ARe, AIm: Single;
  const BRe, BIm: Single);
var
  Tmp: Single;
begin
  Tmp := ARe;
  ARe := ARe * BRe - AIm * BIm;
  AIm := AIm * BRe + Tmp * BIm;
end;

procedure ComplexMultiplyInplace(var ARe, AIm: Double;
  const BRe, BIm: Double);
var
  Tmp: Double;
begin
  Tmp := ARe;
  ARe := ARe * BRe - AIm * BIm;
  AIm := AIm * BRe + Tmp * BIm;
end;


function ComplexDivide(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(BRe) + sqr(BIm));
  Result.Re := (ARe * BRe + AIm * BIm) * Divisor;
  Result.Im := (AIm * BRe - ARe * BIm) * Divisor;
end;

function ComplexDivide(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(BRe) + sqr(BIm));
  Result.Re := (ARe * BRe + AIm * BIm) * Divisor;
  Result.Im := (AIm * BRe - ARe * BIm) * Divisor;
end;

function ComplexDivide(const A, B: TComplexSingle): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(B.Re) + sqr(B.Im));
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * Divisor;
end;

function ComplexDivide(const A, B: TComplexDouble): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(B.Re) + sqr(B.Im));
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * Divisor;
end;

////////////////////////////////////////////////////////////////////////////////

procedure ComplexDivideInplace(var A: TComplexSingle; const B: TComplexSingle);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (sqr(B.Re) + sqr(B.Im));
  Temp := A.Re;
  A.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  A.Im := (A.Im * B.Re - Temp * B.Im) * Divisor;
end;

procedure ComplexDivideInplace(var A: TComplexDouble; const B: TComplexDouble);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (sqr(B.Re) + sqr(B.Im));
  Temp := A.Re;
  A.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  A.Im := (A.Im * B.Re - Temp * B.Im) * Divisor;
end;

procedure ComplexDivideInplace(var ARe, AIm: Single; const BRe, BIm: Single);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (sqr(BRe) + sqr(BIm));
  Temp := ARe;
  ARe := (ARe * BRe + AIm * BIm) * Divisor;
  AIm := (AIm * BRe - Temp * BIm) * Divisor;
end;

procedure ComplexDivideInplace(var ARe, AIm: Double; const BRe, BIm: Double);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (sqr(BRe) + sqr(BIm));
  Temp := ARe;
  ARe := (ARe * BRe + AIm * BIm) * Divisor;
  AIm := (AIm * BRe - Temp * BIm) * Divisor;
end;


////////////////////////////////////////////////////////////////////////////////

function ComplexReciprocal(const A: TComplexSingle): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(A.Re) + sqr(A.Im));
  Result.Re := Result.Re * Divisor;
  Result.Im := Result.Im * Divisor;
end;

function ComplexReciprocal(const A: TComplexDouble): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(A.Re) + sqr(A.Im));
  Result.Re := Result.Re * Divisor;
  Result.Im := Result.Im * Divisor;
end;

function ComplexReciprocal(const ARe, AIm: Single): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(ARe) + sqr(AIm));
  Result.Re := Result.Re * Divisor;
  Result.Im := Result.Im * Divisor;
end;

function ComplexReciprocal(const ARe, AIm: Double): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(ARe) + sqr(AIm));
  Result.Re := Result.Re * Divisor;
  Result.Im := Result.Im * Divisor;
end;

////////////////////////////////////////////////////////////////////////////////

procedure ComplexReciprocalInplace(var A: TComplexSingle);
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(A.Re) + sqr(A.Im));
  A.Re := A.Re * Divisor;
  A.Im := A.Im * Divisor;
end;

procedure ComplexReciprocalInplace(var A: TComplexDouble);
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(A.Re) + sqr(A.Im));
  A.Re := A.Re * Divisor;
  A.Im := A.Im * Divisor;
end;

procedure ComplexReciprocalInplace(var ARe, AIm: Single);
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(ARe) + sqr(AIm));
  ARe := ARe * Divisor;
  AIm := AIm * Divisor;
end;

procedure ComplexReciprocalInplace(var ARe, AIm: Double);
var
  Divisor: Double;
begin
  Divisor := 1 / (sqr(ARe) + sqr(AIm));
  ARe := ARe * Divisor;
  AIm := AIm * Divisor;
end;

////////////////////////////////////////////////////////////////////////////////

function ComplexSqr(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := sqr(Re) - sqr(Im);
  Result.Im := 2 * Re * Im;
end;

function ComplexSqr(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := sqr(Re) - sqr(Im);
  Result.Im := 2 * Re * Im;
end;

function ComplexSqr(const a: TComplexSingle): TComplexSingle;
begin
  Result.Re := sqr(a.Re) - sqr(a.Im);
  Result.Im := 2 * a.Re * a.Im;
end;

function ComplexSqr(const a: TComplexDouble): TComplexDouble;
begin
  Result.Re := sqr(a.Re) - sqr(a.Im);
  Result.Im := 2 * a.Re * a.Im;
end;



function ComplexSqrt(const Re, Im: Single): TComplexSingle;

  function FSqrt(x: Single): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Single;
begin
  Mag := ComplexMagnitude(Re, Im);
  Result.Re := FSqrt(0.5 * (Mag + Re));
  Result.Im := FSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt(const Re, Im: Double): TComplexDouble;

  function FSqrt(x: Double): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Double;
begin
  Mag := ComplexMagnitude(Re, Im);
  Result.Re := FSqrt(0.5 * (Mag + Re));
  Result.Im := FSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt(const a: TComplexSingle): TComplexSingle;

  function FSqrt(x: Single): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Single;
begin
  Mag := ComplexMagnitude(a);
  Result.Re := FSqrt(0.5 * (Mag + a.Re));
  Result.Im := FSqrt(0.5 * (Mag - a.Re));
  if (a.Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt(const a: TComplexDouble): TComplexDouble;

  function FSqrt(x: Double): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Double;
begin
  Mag := ComplexMagnitude(a);
  Result.Re := FSqrt(0.5 * (Mag + a.Re));
  Result.Im := FSqrt(0.5 * (Mag - a.Re));
  if (a.Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexExp(const Re, Im: Single): TComplexSingle;
begin
 Result.Im := Exp(Re);
 Result.Re := Result.Im * cos(Im);
 Result.Im := Result.Im * Sin(Im);
end;

function ComplexExp(const Re, Im: Double): TComplexDouble;
begin
 Result.Im := Exp(Re);
 Result.Re := Result.Im * Cos(Im);
 Result.Im := Result.Im * Sin(Im);
end;

function ComplexExp(const a: TComplexSingle): TComplexSingle;
begin
 Result.Im := Exp(a.Re);
 Result.Re := Result.Im * cos(a.Im);
 Result.Im := Result.Im * Sin(a.Im);
end;

function ComplexExp(const a: TComplexDouble): TComplexDouble;
begin
 Result.Im := Exp(a.Re);
 Result.Re := Result.Im * cos(a.Im);
 Result.Im := Result.Im * Sin(a.Im);
end;

end.
