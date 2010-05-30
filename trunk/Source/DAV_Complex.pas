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
//  Portions created by Christian-W. Budde are Copyright (C) 2007-2010        //
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
  PDAV3ComplexSingleArray = ^TDAV3ComplexSingleArray;
  TDAV3ComplexSingleArray = array [0..2] of TComplexSingle;
  PDAV4ComplexSingleArray = ^TDAV4ComplexSingleArray;
  TDAV4ComplexSingleArray = array [0..3] of TComplexSingle;

  PDAV2ComplexDoubleArray = ^TDAV2ComplexDoubleArray;
  TDAV2ComplexDoubleArray = array [0..1] of TComplexDouble;
  PDAV3ComplexDoubleArray = ^TDAV3ComplexDoubleArray;
  TDAV3ComplexDoubleArray = array [0..2] of TComplexDouble;
  PDAV4ComplexDoubleArray = ^TDAV4ComplexDoubleArray;
  TDAV4ComplexDoubleArray = array [0..3] of TComplexDouble;

function Complex32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Complex64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function ComplexPolar32(const Magnitude, Angle: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function ComplexPolar64(const Magnitude, Angle: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function ComplexSign32(const Z: TComplexSingle): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign64(const Z: TComplexDouble): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign32(const Re, Im: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSign64(const Re, Im: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexConjugate32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexConjugate64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexInvert32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexInvert64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexMagnitude32(const Re, Im: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude64(const Re, Im: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude32(const Complex: TComplexSingle): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMagnitude64(const Complex: TComplexDouble): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArgument32(const Re, Im: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument64(const Re, Im: Double): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument32(const Complex: TComplexSingle): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArgument64(const Complex: TComplexDouble): Double; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexAdd32(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd64(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd32(const ARe, AIm, BRe, BIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexAdd64(const ARe, AIm, BRe, BIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexAddInplace32(var A: TComplexSingle; const B: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace64(var A: TComplexDouble; const B: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace32(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexAddInplace64(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSubtract32(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract64(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract32(const ARe, AIm, BRe, BIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSubtract64(const ARe, AIm, BRe, BIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexSubtractInplace32(var A: TComplexSingle; const B: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace64(var A: TComplexDouble; const B: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace32(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexSubtractInplace64(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexMultiply32(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMultiply64(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexMultiply32(const ARe, AIm, BRe, BIm: Single): TComplexSingle; overload;
function ComplexMultiply64(const ARe, AIm, BRe, BIm: Double): TComplexDouble; overload;

procedure ComplexMultiplyInplace32(var A: TComplexSingle; const B: TComplexSingle); overload;
procedure ComplexMultiplyInplace64(var A: TComplexDouble; const B: TComplexDouble); overload;
procedure ComplexMultiplyInplace32(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexMultiplyInplace64(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexMultiply2Inplace32(var A: TComplexSingle; const B: TComplexSingle);
procedure ComplexMultiply2Inplace64(var A: TComplexDouble; const B: TComplexDouble);

function ComplexDivide32(const A, B: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide64(const A, B: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide32(const ARe, AIm, BRe, BIm: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexDivide64(const ARe, AIm, BRe, BIm: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexDivideInplace32(var A: TComplexSingle; const B: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace64(var A: TComplexDouble; const B: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace32(var ARe, AIm: Single; const BRe, BIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexDivideInplace64(var ARe, AIm: Double; const BRe, BIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexReciprocal32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexReciprocal64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

procedure ComplexReciprocalInplace32(var Z: TComplexSingle); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace64(var Z: TComplexDouble); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace32(var ZRe, ZIm: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure ComplexReciprocalInplace64(var ZRe, ZIm: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSqr32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSqr64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSqrt32(const Re, Im: Single): TComplexSingle; overload;
function ComplexSqrt64(const Re, Im: Double): TComplexDouble; overload;
function ComplexSqrt32(const Z: TComplexSingle): TComplexSingle; overload;
function ComplexSqrt64(const Z: TComplexDouble): TComplexDouble; overload;

function ComplexLog1032(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog1064(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog1032(const Complex: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLog1064(const Complex: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexExp32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexExp64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexExp32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexExp64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexLn32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLn64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLn32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexLn64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSin32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSin64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSin32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSin64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexCos32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCos64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCos32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCos64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexTan32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTan64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTan32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTan64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexSinh32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSinh64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSinh32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexSinh64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexCosh32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCosh64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCosh32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexCosh64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexTanh32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTanh64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTanh32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexTanh64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcSin32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcSin64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcSin32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcSin64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcCos32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcCos64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcCos32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcCos64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcTan32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTan64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTan32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTan64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

function ComplexArcTanh32(const Re, Im: Single): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTanh64(const Re, Im: Double): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTanh32(const Z: TComplexSingle): TComplexSingle; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
function ComplexArcTanh64(const Z: TComplexDouble): TComplexDouble; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;

implementation

uses
  Math, DAV_Math;

{ Build Complex Record }

function Complex32(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := Re;
  Result.Im := Im;
end;

function Complex64(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := Re;
  Result.Im := Im;
end;


{ Build Complex Record from Magnitude & Angle }

function ComplexPolar32(const Magnitude, Angle: Single): TComplexSingle;
begin
  Result.Re := Magnitude * Cos(Angle);
  Result.Im := Magnitude * Sin(Angle);
end;

function ComplexPolar64(const Magnitude, Angle: Double): TComplexDouble;
begin
  Result.Re := Magnitude * Cos(Angle);
  Result.Im := Magnitude * Sin(Angle);
end;


{ Complex Sign Function }

function ComplexSign32(const Z: TComplexSingle): Single;
begin
 if (Z.Re >= 0) and (Z.Im > 0)
  then Result := 1 else
 if (Z.Re <= 0) and (Z.Im < 0)
  then Result := -1
  else Result := Sign(Z.Re);
end;

function ComplexSign64(const Z: TComplexDouble): Double;
begin
 if (Z.Re >= 0) and (Z.Im > 0)
  then Result := 1
  else
 if (Z.Re <= 0) and (Z.Im < 0)
  then Result := -1
  else Result := sign(Z.Re);
end;

function ComplexSign32(const Re, Im: Single): Single;
begin
  if (Re >= 0) and (Im > 0) then
    Result := 1
  else
  if (Re <= 0) and (Im < 0) then
    Result := -1
  else
    Result := sign(Re);
end;

function ComplexSign64(const Re, Im: Double): Double;
begin
  if (Re >= 0) and (Im > 0) then
    Result := 1
  else
  if (Re <= 0) and (Im < 0) then
    Result := -1
  else
    Result := sign(Re);
end;


{ ComplexConjugate }

function ComplexConjugate32(const Re, Im: Single): TComplexSingle;
begin
  Result.Re :=  Re;
  Result.Im := -Im;
end;

function ComplexConjugate64(const Re, Im: Double): TComplexDouble;
begin
  Result.Re :=  Re;
  Result.Im := -Im;
end;

function ComplexConjugate32(const Z: TComplexSingle): TComplexSingle;
begin
  Result.Re :=  Z.Re;
  Result.Im := -Z.Im;
end;

function ComplexConjugate64(const Z: TComplexDouble): TComplexDouble;
begin
  Result.Re :=  Z.Re;
  Result.Im := -Z.Im;
end;


{ ComplexInvert }

function ComplexInvert32(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert64(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := -Re;
  Result.Im := -Im;
end;

function ComplexInvert32(const Z: TComplexSingle): TComplexSingle;
begin
  Result.Re := -Z.Re;
  Result.Im := -Z.Im;
end;

function ComplexInvert64(const Z: TComplexDouble): TComplexDouble;
begin
  Result.Re := -Z.Re;
  Result.Im := -Z.Im;
end;


{ ComplexMagnitude }

function ComplexMagnitude32(const Re, Im: Single): Single;
begin
  Result := hypot(Re, Im);
end;

function ComplexMagnitude64(const Re, Im: Double): Double;
begin
  Result := hypot(Re, Im);
end;

function ComplexMagnitude32(const Complex: TComplexSingle): Single;
begin
  Result := hypot(Complex.Re, Complex.Im);
end;

function ComplexMagnitude64(const Complex: TComplexDouble): Double;
begin
  Result := hypot(Complex.Re, Complex.Im);
end;


{ ComplexArgument }

function ComplexArgument32(const Re, Im: Single): Single;
begin
  Result := ArcTan2(Im, Re);
end;

function ComplexArgument64(const Re, Im: Double): Double;
begin
  Result := ArcTan2(Im, Re);
end;

function ComplexArgument32(const Complex: TComplexSingle): Single;
begin
  Result := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexArgument64(const Complex: TComplexDouble): Double;
begin
  Result := ArcTan2(Complex.Im, Complex.Re);
end;


{ ComplexAdd }

function ComplexAdd32(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
begin
  Result.Re := ARe + BRe;
  Result.Im := AIm + BIm;
end;

function ComplexAdd64(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
begin
  Result.Re := ARe + BRe;
  Result.Im := AIm + BIm;
end;

function ComplexAdd32(const A, B: TComplexSingle): TComplexSingle;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;

function ComplexAdd64(const A, B: TComplexDouble): TComplexDouble;
begin
  Result.Re := A.Re + B.Re;
  Result.Im := A.Im + B.Im;
end;


{ ComplexAddInplace }

procedure ComplexAddInplace32(var A: TComplexSingle; const B: TComplexSingle);
begin
  A.Re := A.Re + B.Re;
  A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace64(var A: TComplexDouble; const B: TComplexDouble);
begin
  A.Re := A.Re + B.Re;
  A.Im := A.Im + B.Im;
end;

procedure ComplexAddInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
begin
  ARe := ARe + BRe;
  AIm := AIm + BIm;
end;

procedure ComplexAddInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
begin
  ARe := ARe + BRe;
  AIm := AIm + BIm;
end;


{ ComplexSubtract }

function ComplexSubtract32(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
begin
  Result.Re := ARe - BRe;
  Result.Im := AIm - BIm;
end;

function ComplexSubtract64(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
begin
  Result.Re := ARe - BRe;
  Result.Im := AIm - BIm;
end;

function ComplexSubtract32(const A, B: TComplexSingle): TComplexSingle;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;

function ComplexSubtract64(const A, B: TComplexDouble): TComplexDouble;
begin
  Result.Re := A.Re - B.Re;
  Result.Im := A.Im - B.Im;
end;


{ ComplexSubtractInplace }

procedure ComplexSubtractInplace32(var A: TComplexSingle; const B: TComplexSingle);
begin
  A.Re := A.Re - B.Re;
  A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace64(var A: TComplexDouble; const B: TComplexDouble);
begin
  A.Re := A.Re - B.Re;
  A.Im := A.Im - B.Im;
end;

procedure ComplexSubtractInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
begin
  ARe := ARe - BRe;
  AIm := AIm - BIm;
end;

procedure ComplexSubtractInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
begin
  ARe := ARe - BRe;
  AIm := AIm - BIm;
end;


{ ComplexMultiply }

function ComplexMultiply32(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
begin
  Result.Re := ARe * BRe - AIm * BIm;
  Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply64(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
begin
  Result.Re := ARe * BRe - AIm * BIm;
  Result.Im := AIm * BRe + ARe * BIm;
end;

function ComplexMultiply32(const A, B: TComplexSingle): TComplexSingle;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;

function ComplexMultiply64(const A, B: TComplexDouble): TComplexDouble;
begin
  Result.Re := A.Re * B.Re - A.Im * B.Im;
  Result.Im := A.Im * B.Re + A.Re * B.Im;
end;


{ ComplexMultiplyInplace }

procedure ComplexMultiplyInplace32(var A: TComplexSingle;
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

procedure ComplexMultiplyInplace64(var A: TComplexDouble;
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

procedure ComplexMultiplyInplace32(var ARe, AIm: Single;
  const BRe, BIm: Single);
var
  Tmp: Single;
begin
  Tmp := ARe;
  ARe := ARe * BRe - AIm * BIm;
  AIm := AIm * BRe + Tmp * BIm;
end;

procedure ComplexMultiplyInplace64(var ARe, AIm: Double;
  const BRe, BIm: Double);
var
  Tmp: Double;
begin
  Tmp := ARe;
  ARe := ARe * BRe - AIm * BIm;
  AIm := AIm * BRe + Tmp * BIm;
end;


{ ComplexMultiply2Inplace32 }

procedure ComplexMultiply2Inplace32(var A: TComplexSingle;
  const B: TComplexSingle);
{$IFDEF PUREPASCAL}
var
  Temp: Single;
begin
  Temp := A.Re;
  A.Re := A.Re * (Sqr(B.Re) - Sqr(B.Im)) - 2 * A.Im * B.Im * B.Re;
  A.Im := A.Im * (Sqr(B.Re) - Sqr(B.Im)) + 2 * Temp * B.Im * B.Re;
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

procedure ComplexMultiply2Inplace64(var A: TComplexDouble;
  const B: TComplexDouble);
{$IFDEF PUREPASCAL}
var
  Btmp : Double;
  Temp : Double;
begin
  Btmp := (Sqr(B.Re) - Sqr(B.Im)); Temp := A.Re;
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


{ ComplexDivide }

function ComplexDivide32(const ARe, AIm, BRe, BIm: Single): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Result.Re := (ARe * BRe + AIm * BIm) * Divisor;
  Result.Im := (AIm * BRe - ARe * BIm) * Divisor;
end;

function ComplexDivide64(const ARe, AIm, BRe, BIm: Double): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Result.Re := (ARe * BRe + AIm * BIm) * Divisor;
  Result.Im := (AIm * BRe - ARe * BIm) * Divisor;
end;

function ComplexDivide32(const A, B: TComplexSingle): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * Divisor;
end;

function ComplexDivide64(const A, B: TComplexDouble): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Result.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  Result.Im := (A.Im * B.Re - A.Re * B.Im) * Divisor;
end;


{ ComplexDivideInplace }

procedure ComplexDivideInplace32(var A: TComplexSingle; const B: TComplexSingle);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Temp := A.Re;
  A.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  A.Im := (A.Im * B.Re - Temp * B.Im) * Divisor;
end;

procedure ComplexDivideInplace64(var A: TComplexDouble; const B: TComplexDouble);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(B.Re) + Sqr(B.Im));
  Temp := A.Re;
  A.Re := (A.Re * B.Re + A.Im * B.Im) * Divisor;
  A.Im := (A.Im * B.Re - Temp * B.Im) * Divisor;
end;

procedure ComplexDivideInplace32(var ARe, AIm: Single; const BRe, BIm: Single);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Temp := ARe;
  ARe := (ARe * BRe + AIm * BIm) * Divisor;
  AIm := (AIm * BRe - Temp * BIm) * Divisor;
end;

procedure ComplexDivideInplace64(var ARe, AIm: Double; const BRe, BIm: Double);
var
  Divisor, Temp: Double;
begin
  Divisor := 1 / (Sqr(BRe) + Sqr(BIm));
  Temp := ARe;
  ARe := (ARe * BRe + AIm * BIm) * Divisor;
  AIm := (AIm * BRe - Temp * BIm) * Divisor;
end;


{ ComplexReciprocal }

function ComplexReciprocal32(const Z: TComplexSingle): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := Z.Re * Divisor;
  Result.Im := Z.Im * Divisor;
end;

function ComplexReciprocal64(const Z: TComplexDouble): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Result.Re := Z.Re * Divisor;
  Result.Im := Z.Im * Divisor;
end;

function ComplexReciprocal32(const Re, Im: Single): TComplexSingle;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Re) + Sqr(Im));
  Result.Re := Re * Divisor;
  Result.Im := Im * Divisor;
end;

function ComplexReciprocal64(const Re, Im: Double): TComplexDouble;
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Re) + Sqr(Im));
  Result.Re := Re * Divisor;
  Result.Im := Im * Divisor;
end;


{ ComplexReciprocalInplace }

procedure ComplexReciprocalInplace32(var Z: TComplexSingle);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Z.Re := Z.Re * Divisor;
  Z.Im := Z.Im * Divisor;
end;

procedure ComplexReciprocalInplace64(var Z: TComplexDouble);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(Z.Re) + Sqr(Z.Im));
  Z.Re := Z.Re * Divisor;
  Z.Im := Z.Im * Divisor;
end;

procedure ComplexReciprocalInplace32(var ZRe, ZIm: Single);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(ZRe) + Sqr(ZIm));
  ZRe := ZRe * Divisor;
  ZIm := ZIm * Divisor;
end;

procedure ComplexReciprocalInplace64(var ZRe, ZIm: Double);
var
  Divisor: Double;
begin
  Divisor := 1 / (Sqr(ZRe) + Sqr(ZIm));
  ZRe := ZRe * Divisor;
  ZIm := ZIm * Divisor;
end;


{ ComplexSqr }

function ComplexSqr32(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := Sqr(Re) - Sqr(Im);
  Result.Im := 2 * Re * Im;
end;

function ComplexSqr64(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := Sqr(Re) - Sqr(Im);
  Result.Im := 2 * Re * Im;
end;

function ComplexSqr32(const Z: TComplexSingle): TComplexSingle;
begin
  Result.Re := Sqr(Z.Re) - Sqr(Z.Im);
  Result.Im := 2 * Z.Re * Z.Im;
end;

function ComplexSqr64(const Z: TComplexDouble): TComplexDouble;
begin
  Result.Re := Sqr(Z.Re) - Sqr(Z.Im);
  Result.Im := 2 * Z.Re * Z.Im;
end;


{ ComplexSqrt }

function ComplexSqrt32(const Re, Im: Single): TComplexSingle;

  function FastSqrt(x: Single): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Single;
begin
  Mag := ComplexMagnitude32(Re, Im);
  Result.Re := FastSqrt(0.5 * (Mag + Re));
  Result.Im := FastSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt64(const Re, Im: Double): TComplexDouble;

  function FastSqrt(x: Double): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Double;
begin
  Mag := ComplexMagnitude64(Re, Im);
  Result.Re := FastSqrt(0.5 * (Mag + Re));
  Result.Im := FastSqrt(0.5 * (Mag - Re));
  if (Im < 0.0) then
    Result.Im := -Result.Im;
end;

function ComplexSqrt32(const Z: TComplexSingle): TComplexSingle;

  function FastSqrt(x: Single): Double;
  begin
    if x > 0 then
      Result := Sqrt(x)
    else
      Result := 0;
  end;

var
  Mag: Single;
begin
  Mag := ComplexMagnitude32(Z);
  Result.Re := FastSqrt(0.5 * (Mag + Z.Re));
  Result.Im := FastSqrt(0.5 * (Mag - Z.Re));
  if (Z.Im < 0.0) then Result.Im := -Result.Im;
end;

function ComplexSqrt64(const Z: TComplexDouble): TComplexDouble;

  function FastSqrt(x: Double): Double;
  begin
    if x > 0
     then Result := Sqrt(x)
     else Result := 0;
  end;

var
  Mag: Double;
begin
  Mag := ComplexMagnitude64(Z);
  Result.Re := FastSqrt(0.5 * (Mag + Z.Re));
  Result.Im := FastSqrt(0.5 * (Mag - Z.Re));
  if (Z.Im < 0.0) then Result.Im := -Result.Im;
end;


{ ComplexLog10 }

function ComplexLog1032(const Re, Im: Single): TComplexSingle;
begin
  Result.Re := 0.5 * Log10((Sqr(Re) + Sqr(Im)));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLog1064(const Re, Im: Double): TComplexDouble;
begin
  Result.Re := 0.5 * Log10((Sqr(Re) + Sqr(Im)));
  Result.Im := ArcTan2(Im, Re);
end;

function ComplexLog1032(const Complex: TComplexSingle): TComplexSingle;
begin
  Result.Re := 0.5 * Log10((Sqr(Complex.Re) + Sqr(Complex.Im)));
  Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;

function ComplexLog1064(const Complex: TComplexDouble): TComplexDouble;
begin
  Result.Re := 0.5 * Log10((Sqr(Complex.Re) + Sqr(Complex.Im)));
  Result.Im := ArcTan2(Complex.Im, Complex.Re);
end;


{ ComplexExp }

function ComplexExp32(const Re, Im: Single): TComplexSingle;
begin
 Result.Im := Exp(Re);
 Result.Re := Result.Im * Cos(Im);
 Result.Im := Result.Im * Sin(Im);
end;

function ComplexExp64(const Re, Im: Double): TComplexDouble;
begin
 Result.Im := Exp(Re);
 Result.Re := Result.Im * Cos(Im);
 Result.Im := Result.Im * Sin(Im);
end;

function ComplexExp32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Im := Exp(Z.Re);
 Result.Re := Result.Im * Cos(Z.Im);
 Result.Im := Result.Im * Sin(Z.Im);
end;

function ComplexExp64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Im := Exp(Z.Re);
 Result.Re := Result.Im * Cos(Z.Im);
 Result.Im := Result.Im * Sin(Z.Im);
end;


{ ComplexLn }

function ComplexLn32(const Re, Im: Single): TComplexSingle;
begin
 Result.Re := ln(hypot(Re, Im));
 Result.Im := ArcTan2(Im, Re);
end;

function ComplexLn64(const Re, Im: Double): TComplexDouble;
begin
 Result.Re := ln(hypot(Re, Im));
 Result.Im := ArcTan2(Im, Re);
end;

function ComplexLn32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Re := ln(hypot(Z.Re, Z.Im));
 Result.Im := ArcTan2(Z.Im, Z.Re);
end;

function ComplexLn64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Re := ln(hypot(Z.Re, Z.Im));
 Result.Im := ArcTan2(Z.Im, Z.Re);
end;


{ ComplexSin }

function ComplexSin32(const Re, Im: Single): TComplexSingle;
begin
 Result.Re := Exp(-Im);
 Result.Im := 0.5 * Cos(Re) * (1 / Result.Re - Result.Re);
 Result.Re := 0.5 * Sin(Re) * (1 / Result.Re + Result.Re);
end;

function ComplexSin64(const Re, Im: Double): TComplexDouble;
begin
 Result.Re := Exp(-Im);
 Result.Im := 0.5 * Cos(Re) * (1 / Result.Re - Result.Re);
 Result.Re := 0.5 * Sin(Re) * (1 / Result.Re + Result.Re);
end;

function ComplexSin32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Re := Exp(-Z.Im);
 Result.Im := 0.5 * Cos(Z.Re) * (1 / Result.Re - Result.Re);
 Result.Re := 0.5 * Sin(Z.Re) * (1 / Result.Re + Result.Re);
end;

function ComplexSin64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Re := Exp(-Z.Im);
 Result.Im := 0.5 * Cos(Z.Re) * (1 / Result.Re - Result.Re);
 Result.Re := 0.5 * Sin(Z.Re) * (1 / Result.Re + Result.Re);
end;


{ ComplexCos }

function ComplexCos32(const Re, Im: Single): TComplexSingle;
begin
 Result.Im := Exp(Im);
 Result.Re := 0.5 * Cos(Re) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Re) * (1 / Result.Im - Result.Im);
end;

function ComplexCos64(const Re, Im: Double): TComplexDouble;
begin
 Result.Im := Exp(Im);
 Result.Re := 0.5 * Cos(Re) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Re) * (1 / Result.Im - Result.Im);
end;

function ComplexCos32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Im := Exp(Z.Im);
 Result.Re := 0.5 * Cos(Z.Re) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Z.Re) * (1 / Result.Im - Result.Im);
end;

function ComplexCos64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Im := Exp(Z.Im);
 Result.Re := 0.5 * Cos(Z.Re) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Z.Re) * (1 / Result.Im - Result.Im);
end;


{ ComplexTan }

function ComplexTan32(const Re, Im: Single): TComplexSingle;
var
 ExpIm   : Single;
 ExpRe   : TComplexSingle;
 Divisor : Single;
begin
 ExpIm := Exp(Im);
 GetSinCos(Re, ExpRe.Im, ExpRe.Re);

 Divisor := 1 / (Sqr(ExpRe.Re * (Sqr(ExpIm) + 1)) + Sqr(ExpRe.Im * (Sqr(ExpIm) - 1)));
 Result.Re := ExpRe.Im * ExpRe.Re * 4 * Sqr(ExpIm) * Divisor;
 Result.Im := (Sqr(ExpRe.Re) * (Sqr(Sqr(ExpIm)) - 1) + Sqr(ExpRe.Im) * (Sqr(Sqr(ExpIm)) - 1)) * Divisor;
end;

function ComplexTan64(const Re, Im: Double): TComplexDouble;
var
 ExpIm   : Double;
 ExpRe   : TComplexDouble;
 Divisor : Double;
begin
 ExpIm := Exp(Im);
 GetSinCos(Re, ExpRe.Im, ExpRe.Re);

 Divisor := 1 / (Sqr(ExpRe.Re * (Sqr(ExpIm) + 1)) + Sqr(ExpRe.Im * (Sqr(ExpIm) - 1)));
 Result.Re := ExpRe.Im * ExpRe.Re * 4 * Sqr(ExpIm) * Divisor;
 Result.Im := (Sqr(ExpRe.Re) * (Sqr(Sqr(ExpIm)) - 1) + Sqr(ExpRe.Im) * (Sqr(Sqr(ExpIm)) - 1)) * Divisor;
end;

function ComplexTan32(const Z: TComplexSingle): TComplexSingle;
var
  Value : array [0..1] of TComplexSingle;
begin
 Value[0] := ComplexExp32( Z.Im, -Z.Re);
 Value[1] := ComplexExp32(-Z.Im,  Z.Re);
 Value[0] := ComplexDivide32(ComplexSubtract32(Value[0], Value[1]),
   ComplexAdd32(Value[0], Value[1]));

 Result.Re := -Value[0].Im;
 Result.Im :=  Value[0].Re;
end;

function ComplexTan64(const Z: TComplexDouble): TComplexDouble;
var
 ExpIm   : Double;
 ExpRe   : TComplexDouble;
 Divisor : Double;
begin
 ExpIm := Exp(Z.Im);
 GetSinCos(Z.Re, ExpRe.Im, ExpRe.Re);

 Divisor := 1 / (Sqr(ExpRe.Re * (Sqr(ExpIm) + 1)) + Sqr(ExpRe.Im * (Sqr(ExpIm) - 1)));
 Result.Re := ExpRe.Im * ExpRe.Re * 4 * Sqr(ExpIm) * Divisor;
 Result.Im := (Sqr(ExpRe.Re) * (Sqr(Sqr(ExpIm)) - 1) + Sqr(ExpRe.Im) * (Sqr(Sqr(ExpIm)) - 1)) * Divisor;
end;


{ ComplexSinh }

function ComplexSinh32(const Re, Im: Single): TComplexSingle;
begin
 Result.Im := Exp(Re);
 Result.Re := 0.5 * Cos(Im) * (1 / Result.Im - Result.Im);
 Result.Im := 0.5 * Sin(Im) * (1 / Result.Im + Result.Im);
end;

function ComplexSinh64(const Re, Im: Double): TComplexDouble;
begin
 Result.Im := Exp(Re);
 Result.Re := 0.5 * Cos(Im) * (1 / Result.Im - Result.Im);
 Result.Im := 0.5 * Sin(Im) * (1 / Result.Im + Result.Im);
end;

function ComplexSinh32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Im := Exp(Z.Re);
 Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im - Result.Im);
 Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im + Result.Im);
end;

function ComplexSinh64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Im := Exp(Z.Re);
 Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im - Result.Im);
 Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im + Result.Im);
end;


{ ComplexCosh }

function ComplexCosh32(const Re, Im: Single): TComplexSingle;
begin
 Result.Im := Exp(Re);
 Result.Re := 0.5 * Cos(Im) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Im) * (1 / Result.Im - Result.Im);
end;

function ComplexCosh64(const Re, Im: Double): TComplexDouble;
begin
 Result.Im := Exp(Re);
 Result.Re := 0.5 * Cos(Im) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Im) * (1 / Result.Im - Result.Im);
end;

function ComplexCosh32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Im := Exp(Z.Re);
 Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im - Result.Im);
end;

function ComplexCosh64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Im := Exp(Z.Re);
 Result.Re := 0.5 * Cos(Z.Im) * (1 / Result.Im + Result.Im);
 Result.Im := 0.5 * Sin(Z.Im) * (1 / Result.Im - Result.Im);
end;


{ ComplexTanh }

function ComplexTanh32(const Re, Im: Single): TComplexSingle;
var
  ExpRe : Single;
  ExpIm : TComplexSingle;
  Temp  : TComplexSingle;
begin
 ExpRe := Exp(Re);
 GetSinCos(Im, ExpIm.Im, ExpIm.Re);

 Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
 Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

 Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
 Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

 Result := ComplexDivide32(Temp, Result);
end;

function ComplexTanh64(const Re, Im: Double): TComplexDouble;
var
  ExpRe : Double;
  ExpIm : TComplexDouble;
  Temp  : TComplexDouble;
begin
 ExpRe := Exp(Re);
 GetSinCos(Im, ExpIm.Im, ExpIm.Re);

 Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
 Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

 Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
 Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

 Result := ComplexDivide64(Temp, Result);
end;

function ComplexTanh32(const Z: TComplexSingle): TComplexSingle;
var
  ExpRe : Single;
  ExpIm : TComplexSingle;
  Temp  : TComplexSingle;
begin
 ExpRe := Exp(Z.Re);
 GetSinCos(Z.Im, ExpIm.Im, ExpIm.Re);

 Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
 Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

 Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
 Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

 Result := ComplexDivide32(Temp, Result);
end;

function ComplexTanh64(const Z: TComplexDouble): TComplexDouble;
var
  ExpRe : Double;
  ExpIm : TComplexDouble;
  Temp  : TComplexDouble;
begin
 ExpRe := Exp(Z.Re);
 GetSinCos(Z.Im, ExpIm.Im, ExpIm.Re);

 Temp.Re := ExpIm.Re * (ExpRe - 1 / ExpRe);
 Temp.Im := ExpIm.Im * (ExpRe + 1 / ExpRe);

 Result.Re := ExpIm.Re * (ExpRe + 1 / ExpRe);
 Result.Im := ExpIm.Im * (ExpRe - 1 / ExpRe);

 Result := ComplexDivide64(Temp, Result);
end;


{ ComplexArcSin }

function ComplexArcSin32(const Re, Im: Single): TComplexSingle;
var
  SqrMag : Single;
begin
 SqrMag := Sqr(Sqr(Re) + Sqr(Im));
 Result.Re := 0.5 * Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) - SqrMag);
 Result.Im := 0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) + SqrMag);
end;

function ComplexArcSin64(const Re, Im: Double): TComplexDouble;
var
  SqrMag : Double;
begin
 SqrMag := Sqr(Sqr(Re) + Sqr(Im));
 Result.Re := 0.5 * Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) - SqrMag);
 Result.Im := 0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) + SqrMag);
end;

function ComplexArcSin32(const Z: TComplexSingle): TComplexSingle;
var
  SqrMag : Single;
begin
 SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
 Result.Re := 0.5 * Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) - SqrMag);
 Result.Im := 0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) + SqrMag);
end;

function ComplexArcSin64(const Z: TComplexDouble): TComplexDouble;
var
  SqrMag : Double;
begin
 SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
 Result.Re := 0.5 * Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) - SqrMag);
 Result.Im := 0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) + SqrMag);
end;


{ ComplexArcCos }

function ComplexArcCos32(const Re, Im: Single): TComplexSingle;
var
  SqrMag : Single;
begin
 SqrMag := Sqr(Sqr(Re) + Sqr(Im));
 Result.Re := 0.5 * (Pi - Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) - SqrMag));
 Result.Im := -0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) + SqrMag);
end;

function ComplexArcCos64(const Re, Im: Double): TComplexDouble;
var
  SqrMag : Double;
begin
 SqrMag := Sqr(Sqr(Re) + Sqr(Im));
 Result.Re := 0.5 * (Pi - Sign(Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) - SqrMag));
 Result.Im := -0.5 * Sign(Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Im)) + SqrMag);
end;

function ComplexArcCos32(const Z: TComplexSingle): TComplexSingle;
var
  SqrMag : Single;
begin
 SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
 Result.Re := 0.5 * (Pi - Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) - SqrMag));
 Result.Im := -0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) + SqrMag);
end;

function ComplexArcCos64(const Z: TComplexDouble): TComplexDouble;
var
  SqrMag : Double;
begin
 SqrMag := Sqr(Sqr(Z.Re) + Sqr(Z.Im));
 Result.Re := 0.5 * (Pi - Sign(Z.Re) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) - SqrMag));
 Result.Im := -0.5 * Sign(Z.Im) * ArcCos(Sqrt((SqrMag - 1) + Sqr(2 * Z.Im)) + SqrMag);
end;


{ ComplexArcTan }

function ComplexArcTan32(const Re, Im: Single): TComplexSingle;
begin
 Result.Re := 0.5 * (ComplexArgument32(1 + Im, Re) - ComplexArgument32(1 - Im, Re));
 Result.Im := 0.5 * (Ln(ComplexMagnitude32(1 - Im, Re)) - ln(ComplexMagnitude32(1 + Im, Re)));
end;

function ComplexArcTan64(const Re, Im: Double): TComplexDouble;
begin
 Result.Re := 0.5 * (ComplexArgument64(1 + Im, Re) - ComplexArgument64(1 - Im, Re));
 Result.Im := 0.5 * (ln(ComplexMagnitude64(1 - Im, Re)) - ln(ComplexMagnitude64(1 + Im, Re)));
end;

function ComplexArcTan32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Re := 0.5 * (ComplexArgument32(1 + Z.Im, Z.Re) - ComplexArgument32(1 - Z.Im, Z.Re));
 Result.Im := 0.5 * (ln(ComplexMagnitude32(1 - Z.Im, Z.Re)) - ln(ComplexMagnitude32(1 + Z.Im, Z.Re)));
end;

function ComplexArcTan64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Re := 0.5 * (ComplexArgument64(1 + Z.Im, Z.Re) - ComplexArgument64(1 - Z.Im, Z.Re));
 Result.Im := 0.5 * (ln(ComplexMagnitude64(1 - Z.Im, Z.Re)) - ln(ComplexMagnitude64(1 + Z.Im, Z.Re)));
end;


{ ComplexArcTanh }

function ComplexArcTanh32(const Re, Im: Single): TComplexSingle;
begin
 Result.Re := 0.5 * (ln(ComplexMagnitude32(1 + Re, Im)) - ln(ComplexMagnitude32(1 - Re, Im)));
 Result.Im := 0.5 * (ComplexArgument32(1 + Re, Im) - ComplexArgument32(1 - Re, Im));
end;

function ComplexArcTanh64(const Re, Im: Double): TComplexDouble;
begin
 Result.Re := 0.5 * (ln(ComplexMagnitude64(1 + Re, Im)) - ln(ComplexMagnitude64(1 - Re, Im)));
 Result.Im := 0.5 * (ComplexArgument64(1 + Re, Im) - ComplexArgument64(1 - Re, Im));
end;

function ComplexArcTanh32(const Z: TComplexSingle): TComplexSingle;
begin
 Result.Re := 0.5 * (ln(ComplexMagnitude32(1 + Z.Re, Z.Im)) - ln(ComplexMagnitude32(1 - Z.Re, Z.Im)));
 Result.Im := 0.5 * (ComplexArgument32(1 + Z.Re, Z.Im) - ComplexArgument32(1 - Z.Re, Z.Im));
end;

function ComplexArcTanh64(const Z: TComplexDouble): TComplexDouble;
begin
 Result.Re := 0.5 * (ln(ComplexMagnitude64(1 + Z.Re, Z.Im)) - ln(ComplexMagnitude64(1 - Z.Re, Z.Im)));
 Result.Im := 0.5 * (ComplexArgument64(1 + Z.Re, Z.Im) - ComplexArgument64(1 - Z.Re, Z.Im));
end;

end.
