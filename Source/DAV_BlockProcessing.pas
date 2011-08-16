unit DAV_BlockProcessing;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2011        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  DAV_Types, DAV_Complex, DAV_Bindings;

procedure ComplexMultiplyBlock32(const Buffer, Filter: PDAVComplexSingleFixedArray; const SampleCount: Integer); overload;
procedure ComplexMultiplyBlock32(const InBuffer, Filter: PDAVComplexSingleFixedArray; const SampleCount: Integer; const OutBuffer: PDAVComplexSingleFixedArray); overload;
procedure ComplexMultiplyBlock64(const Buffer, Filter: PDAVComplexDoubleFixedArray; const SampleCount: Integer); overload;
procedure ComplexMultiplyBlock64(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleCount: Integer; const OutBuffer: PDAVComplexDoubleFixedArray); overload;

procedure ComplexMultiplyConjugated32(const InplaceBuffer, Signal: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
procedure ComplexMultiplyConjugated32(const InBuffer, Signal: PDAVComplexSingleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexSingleFixedArray); overload;
procedure ComplexMultiplyConjugated64(const InplaceBuffer, Signal: PDAVComplexDoubleFixedArray; const SampleFrames: Integer); overload;
procedure ComplexMultiplyConjugated64(const InBuffer, Signal: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;

function FindMaximum(Data: PSingle; SampleCount: Integer): Integer; overload;
function FindMaximum(Data: PDouble; SampleCount: Integer): Integer; overload;
procedure CalcMinMax(Data: PSingle; SampleCount: Integer; var MinMax : TDAVMinMaxSingle); overload;
procedure CalcMinMax(Data: PDouble; SampleCount: Integer; var MinMax : TDAVMinMaxDouble); overload;
procedure DCSubstract(Data: PSingle; SampleCount: Integer); overload;
procedure DCSubstract(Data: PDouble; SampleCount: Integer); overload;

procedure ConvertSingleToDouble(Input: PDAVSingleFixedArray; Output: PDAVDoubleFixedArray; SampleCount: Integer);
procedure ConvertDoubleToSingle(Input: PDAVDoubleFixedArray; Output: PDAVSingleFixedArray; SampleCount: Integer);

procedure FillWithZeroes(StartAdr: PDAVSingleFixedArray; StartPos, EndPos, SampleCount: Integer); overload;
procedure FillWithZeroes(StartAdr: PDAVDoubleFixedArray; StartPos, EndPos, SampleCount: Integer); overload;
procedure InvertBuffer(Data: PDAVSingleFixedArray; SampleCount: Integer); overload;
procedure InvertBuffer(Data: PDAVDoubleFixedArray; SampleCount: Integer); overload;

procedure CopyAndCheck32(Input, Output: PSingle; Count: Integer);
procedure CopyAndCheck64(Input, Output: PDouble; Count: Integer);

procedure QuickSort32(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer);
procedure QuickSort64(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer);
procedure QuickSortWithPosition(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;
procedure QuickSortWithPosition(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;
procedure ReorderPositions(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;
procedure ReorderPositions(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;

implementation

uses
  Math;

procedure ComplexMultiplyBlock32(const Buffer, Filter: PDAVComplexSingleFixedArray; const SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 Buffer^[0].Re := Buffer^[0].Re * Filter^[0].Re;
 Buffer^[0].Im := Buffer^[0].Im * Filter^[0].Im;

 for SampleIndex := 1 to SampleCount - 1
  do ComplexMultiplyInplace32(Buffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 // DC
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [EAX].Single
 ADD   EAX, 4
 ADD   EDX, 4

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [EAX].Single
 ADD   EAX, 4
 ADD   EDX, 4

 DEC   ECX
@Start:
  FLD   [EAX    ].Single  // A.Re
  FLD   [EAX + 4].Single  // A.Im, A.Re
  FLD   [EDX    ].Single  // B.Re, A.Im, A.Re
  FLD   [EDX + 4].Single  // B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FSUBP ST(1), ST(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP  [EAX    ].Single  // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH  ST(2)             // A.Im, B.Re, B.Im, A.Re
  FMULP                   // A.Im * B.Re, B.Im, A.Re
  FXCH  ST(2)             // B.Im, A.Re, A.Im * B.Re
  FMULP                   // B.Im * A.Re, A.Im * B.Re
  FADDP ST(1), ST(0)      // A.Im * B.Re + A.Re * B.Im
  FSTP  [EAX + 4].Single  // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD   EAX, 8
  ADD   EDX, 8
 LOOP @Start

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [EAX].Single
{$ENDIF}
end;

procedure ComplexMultiplyBlock32(const InBuffer, Filter: PDAVComplexSingleFixedArray;
  const SampleCount: Integer; const OutBuffer: PDAVComplexSingleFixedArray); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 InBuffer^[0].Re := InBuffer^[0].Re * Filter^[0].Re;
 InBuffer^[0].Im := InBuffer^[0].Im * Filter^[0].Im;

 for SampleIndex := 1 to SampleCount - 1
  do OutBuffer^[SampleIndex] := ComplexMultiply32(InBuffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [ebx].Single
 ADD EAX, 4
 ADD ebx, 4
 ADD EDX, 4

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [ebx].Single
 ADD EAX, 4
 ADD ebx, 4
 ADD EDX, 4

 DEC ECX
@Start:
  FLD   [EAX    ].Single  // A.Re
  FLD   [EAX + 4].Single  // A.Im, A.Re
  FLD   [EDX    ].Single  // B.Re, A.Im, A.Re
  FLD   [EDX + 4].Single  // B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FSUBP ST(1), ST(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP  [ebx    ].Single  // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH  ST(2)             // A.Im, B.Re, B.Im, A.Re
  FMULP                   // A.Im * B.Re, B.Im, A.Re
  FXCH  ST(2)             // B.Im, A.Re, A.Im * B.Re
  FMULP                   // B.Im * A.Re, A.Im * B.Re
  FADDP ST(1), ST(0)      // A.Im * B.Re + A.Re * B.Im
  FSTP [ebx + 4].Single   // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 8
  ADD ebx, 8
  ADD EDX, 8
 LOOP @Start

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [ebx].Single

 pop ebx
{$ENDIF}
end;

procedure ComplexMultiplyBlock64(const Buffer, Filter: PDAVComplexDoubleFixedArray;
  const SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 Buffer^[0].Re := Buffer^[0].Re * Filter^[0].Re;
 Buffer^[0].Im := Buffer^[0].Im * Filter^[0].Im;

 for SampleIndex := 1 to SampleCount - 1
  do ComplexMultiplyInplace64(Buffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 // DC
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [EAX].Double
 ADD EAX, 8
 ADD EDX, 8

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [EAX].Double
 ADD EAX, 8
 ADD EDX, 8

 DEC ECX
@Start:
  FLD   [EAX    ].Double  // A.Re
  FLD   [EAX + 8].Double  // A.Im, A.Re
  FLD   [EDX    ].Double  // B.Re, A.Im, A.Re
  FLD   [EDX + 8].Double  // B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FSUBP ST(1), ST(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP [EAX    ].Double   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH ST(2)              // A.Im, B.Re, B.Im, A.Re
  FMULP                   // A.Im * B.Re, B.Im, A.Re
  FXCH ST(2)              // B.Im, A.Re, A.Im * B.Re
  FMULP                   // B.Im * A.Re, A.Im * B.Re
  FADDP ST(1), ST(0)      // A.Im * B.Re + A.Re * B.Im
  FSTP [EAX + 8].Double   // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 16
  ADD EDX, 16
 LOOP @Start

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [EAX].Double
{$ENDIF}
end;

procedure ComplexMultiplyBlock64(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleCount: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 InBuffer^[0].Re := InBuffer^[0].Re * Filter^[0].Re;
 InBuffer^[0].Im := InBuffer^[0].Im * Filter^[0].Im;

 for SampleIndex := 1 to SampleCount - 1
  do OutBuffer^[SampleIndex] := ComplexMultiply64(InBuffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [ebx].Double
 ADD EAX, 8
 ADD ebx, 8
 ADD EDX, 8

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [ebx].Double
 ADD EAX, 8
 ADD ebx, 8
 ADD EDX, 8

 DEC ECX
@Start:
  FLD   [EAX    ].Double  // A.Re
  FLD   [EAX + 8].Double  // A.Im, A.Re
  FLD   [EDX    ].Double  // B.Re, A.Im, A.Re
  FLD   [EDX + 8].Double  // B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD   ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL  ST(0), ST(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FSUBP ST(1), ST(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP  [ebx    ].Double   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH  ST(2)             // A.Im, B.Re, B.Im, A.Re
  FMULP                   // A.Im * B.Re, B.Im, A.Re
  FXCH  ST(2)             // B.Im, A.Re, A.Im * B.Re
  FMULP                   // B.Im * A.Re, A.Im * B.Re
  FADDP ST(1), ST(0)      // A.Im * B.Re + A.Re * B.Im
  FSTP  [ebx + 8].Double  // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 16
  ADD ebx, 16
  ADD EDX, 16
 LOOP @Start

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [ebx].Double

 pop ebx
{$ENDIF}
end;

procedure ComplexMultiplyConjugated32(const InplaceBuffer, Signal: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 InplaceBuffer^[0].Re := InplaceBuffer^[0].Re * Signal^[0].Re;
 InplaceBuffer^[0].Im := InplaceBuffer^[0].Im * Signal^[0].Im;

 for SampleIndex := 1 to SampleFrames - 1
  do ComplexMultiplyInplace32(InplaceBuffer^[SampleIndex], ComplexConjugate32(Signal^[SampleIndex]));
{$ELSE}
asm
 // DC
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [EAX].Single
 ADD EAX, 4
 ADD EDX, 4

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [EAX].Single
 ADD EAX, 4
 ADD EDX, 4

 DEC ECX
@Start:
  FLD [EAX    ].Single  // A.Re
  FLD [EAX + 4].Single  // A.Im, A.Re
  FLD [EDX    ].Single  // B.Re, A.Im, A.Re
  FLD [EDX + 4].Single  // B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FADDP                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP [EAX    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH ST(2)            // A.Im, B.Re, B.Im, A.Re
  FMULP                 // A.Im * B.Re, B.Im, A.Re
  FXCH ST(2)            // B.Im, A.Re, A.Im * B.Re
  FMULP                 // B.Im * A.Re, A.Im * B.Re
  FSUBP                 // A.Im * B.Re - A.Re * B.Im
  FSTP [EAX + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 8
  ADD EDX, 8
 LOOP @Start

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [EAX].Single
 {$ENDIF}
end;

procedure ComplexMultiplyConjugated32(const InBuffer, Signal: PDAVComplexSingleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexSingleFixedArray); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 InBuffer^[0].Re := InBuffer^[0].Re * Signal^[0].Re;
 InBuffer^[0].Im := InBuffer^[0].Im * Signal^[0].Im;

 for SampleIndex := 1 to SampleFrames - 1
  do OutBuffer^[SampleIndex] := ComplexMultiply32(InBuffer^[SampleIndex], ComplexConjugate32(Signal^[SampleIndex]));
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [ebx].Single
 ADD EAX, 4
 ADD ebx, 4
 ADD EDX, 4

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [ebx].Single
 ADD EAX, 4
 ADD ebx, 4
 ADD EDX, 4

 DEC ECX
@Start:
  FLD [EAX    ].Single  // A.Re
  FLD [EAX + 4].Single  // A.Im, A.Re
  FLD [EDX    ].Single  // B.Re, A.Im, A.Re
  FLD [EDX + 4].Single  // B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FADDP                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP [ebx    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH ST(2)            // A.Im, B.Re, B.Im, A.Re
  FMULP                 // A.Im * B.Re, B.Im, A.Re
  FXCH ST(2)            // B.Im, A.Re, A.Im * B.Re
  FMULP                 // B.Im * A.Re, A.Im * B.Re
  FSUBP                 // A.Im * B.Re + A.Re * B.Im
  FSTP [ebx + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 8
  ADD ebx, 8
  ADD EDX, 8
 LOOP @Start

 // Nyquist
 FLD   [EAX].Single
 FMUL  [EDX].Single
 FSTP  [ebx].Single

 pop ebx
 {$ENDIF}
end;

procedure ComplexMultiplyConjugated64(const InplaceBuffer, Signal: PDAVComplexDoubleFixedArray; const SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 InplaceBuffer^[0].Re := InplaceBuffer^[0].Re * Signal^[0].Re;
 InplaceBuffer^[0].Im := InplaceBuffer^[0].Im * Signal^[0].Im;

 for SampleIndex := 1 to SampleFrames - 1
  do ComplexMultiplyInplace64(InplaceBuffer^[SampleIndex], ComplexConjugate64(Signal^[SampleIndex]));
{$ELSE}
asm
 // DC
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [EAX].Double
 ADD EAX, 8
 ADD EDX, 8

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [EAX].Double
 ADD EAX, 8
 ADD EDX, 8

 DEC ECX
@Start:
  FLD [EAX    ].Double  // A.Re
  FLD [EAX + 8].Double  // A.Im, A.Re
  FLD [EDX    ].Double  // B.Re, A.Im, A.Re
  FLD [EDX + 8].Double  // B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FADDP                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP [EAX    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH ST(2)            // A.Im, B.Re, B.Im, A.Re
  FMULP                 // A.Im * B.Re, B.Im, A.Re
  FXCH ST(2)            // B.Im, A.Re, A.Im * B.Re
  FMULP                 // B.Im * A.Re, A.Im * B.Re
  FSUBP                 // A.Im * B.Re + A.Re * B.Im
  FSTP [EAX + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 16
  ADD EDX, 16
 LOOP @Start

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [EAX].Double
 {$ENDIF}
end;

procedure ComplexMultiplyConjugated64(const InBuffer, Signal: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 InBuffer^[0].Re := InBuffer^[0].Re * Signal^[0].Re;
 InBuffer^[0].Im := InBuffer^[0].Im * Signal^[0].Im;

 for SampleIndex := 1 to SampleFrames - 1
  do OutBuffer^[SampleIndex] := ComplexMultiply64(InBuffer^[SampleIndex], ComplexConjugate64(Signal^[SampleIndex]));
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [ebx].Double
 ADD EAX, 8
 ADD ebx, 8
 ADD EDX, 8

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [ebx].Double
 ADD EAX, 8
 ADD ebx, 8
 ADD EDX, 8

 DEC ECX
@Start:
  FLD [EAX    ].Double  // A.Re
  FLD [EAX + 8].Double  // A.Im, A.Re
  FLD [EDX    ].Double  // B.Re, A.Im, A.Re
  FLD [EDX + 8].Double  // B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FLD ST(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FMUL ST(0), ST(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  FADDP                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FSTP [ebx    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  FXCH ST(2)            // A.Im, B.Re, B.Im, A.Re
  FMULP                 // A.Im * B.Re, B.Im, A.Re
  FXCH ST(2)            // B.Im, A.Re, A.Im * B.Re
  FMULP                 // B.Im * A.Re, A.Im * B.Re
  FSUBP                 // A.Im * B.Re + A.Re * B.Im
  FSTP [ebx + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  ADD EAX, 16
  ADD ebx, 16
  ADD EDX, 16
 LOOP @Start

 // Nyquist
 FLD   [EAX].Double
 FMUL  [EDX].Double
 FSTP  [ebx].Double

 pop ebx
 {$ENDIF}
end;


procedure DCSubstract(Data: PSingle; SampleCount: Integer);
{$IFDEF PUREPASCAL}
var
  InBuf : array [0..0] of Double absolute Data;
  d : Double;
  i : Integer;
begin
 if SampleCount = 0 then Exit;
 d := InBuf[0];
 for i := 1 to SampleCount - 1
  do d := d + InBuf[i];
 d := d / SampleCount;
 for i := 0 to SampleCount - 1
  do InBuf[i] := InBuf[i] - d;
end;
{$ELSE}
asm
 test EDX, EDX
 jz @End

 push EDX
 fldz                            // DC
 @CalcDCLoop:
   DEC EDX
   fadd  [EAX + 4 * EDX].Single  // DC = DC + Value
 jnz @CalcDCLoop
 pop EDX

 mov  [ESP - 4], EDX
 fild [ESP - 4].Integer          // Length, DC
 fdivp ST(1), ST(0)              // RealDC = DC / Length

 @SubstractDCLoop:
   DEC EDX
   FLD  [EAX + 4 * EDX].Single   // Value, RealDC
   fsub ST(0), ST(1)             // Value-RealDC, RealDC
   FSTP  [EAX + 4 * EDX].Single  // RealDC
 jnz @SubstractDCLoop
 FSTP ST(0)                      // clear stack

 @End:
end;
{$ENDIF}

procedure DCSubstract(Data: PDouble; SampleCount: Integer);
{$IFDEF PUREPASCAL}
var
  InBuf : array [0..0] of Double absolute Data;
  d : Double;
  i : Integer;
begin
 if SampleCount = 0 then Exit;
 d := InBuf[0];
 for i := 1 to SampleCount - 1
  do d := d + InBuf[i];
 d := d / SampleCount;
 for i := 0 to SampleCount - 1
  do InBuf[i] := InBuf[i] - d;
end;
{$ELSE}
asm
 test EDX,EDX
 jz @End

 push EDX
 fldz                            // DC
 @CalcDCLoop:
   DEC EDX
   fadd  [EAX + 8 * EDX].Double  // DC = DC + Value
 jnz @CalcDCLoop
 pop EDX

 mov [esp - 4], EDX
 fild [esp - 4].Integer          // Length, DC
 fdivp ST(1), ST(0)              // RealDC = DC / Length

 @SubstractDCLoop:
   DEC EDX
   FLD  [EAX + 8 * EDX].Double  // Value, RealDC
   fsub ST(0), ST(1)            // Value-RealDC, RealDC
   FSTP  [EAX + 8 * EDX].Double // RealDC
 jnz @SubstractDCLoop
 FSTP ST(0)                     // clear stack

 @End:
end;
{$ENDIF}

procedure ConvertSingleToDouble(Input: PDAVSingleFixedArray; Output: PDAVDoubleFixedArray; SampleCount: Integer);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleCount - 1
  do Output^[i] := Input^[i];
end;
{$ELSE}
asm
@Start:
 FLD  [EAX + ECX * 4 - 4].Single
 FSTP [EDX + ECX * 8 - 8].Double
 LOOP @Start
end;
{$ENDIF}

procedure ConvertDoubleToSingle(Input: PDAVDoubleFixedArray; Output: PDAVSingleFixedArray; SampleCount: Integer);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleCount - 1
  do Output^[i] := Input^[i];
end;
{$ELSE}
asm
@Start:
 FLD [EAX + ECX * 8 - 8].Double
 FSTP [EDX + ECX * 4 - 4].Single
 LOOP @Start
end;
{$ENDIF}

procedure CopyAndCheck32(Input, Output: PSingle; Count: Integer);
var
  Index : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   // check for none and copy value
   if ((PLongWord(Input)^ and $7F800000)  = $7F800000) and
     ((PLongWord(Input)^ and $007FFFFF) <> $00000000)
    then Output^ := 0
    else Output^ := Input^;

   // advance pointers
   Inc(Input);
   Inc(Output);
  end;
end;

procedure CopyAndCheck64(Input, Output: PDouble; Count: Integer);
var
  Index : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   // check for none and copy value
   if ((PInt64(Input)^ and $7FF0000000000000)  = $7FF0000000000000) and
     ((PInt64(Input)^ and $000FFFFFFFFFFFFF) <> $0000000000000000)
     then Output^ := 0
     else Output^ := Input^;

   // advance pointers
   Inc(Input);
   Inc(Output);
  end;
end;

function FindMaximum(Data: PSingle; SampleCount: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  i : Integer;
  d : Double;
begin
 Result := 0;
 Assert(SampleCount > 0);
 d := Abs(Data^);
 for i := 1 to SampleCount - 1 do
  begin
   if Abs(Data^) > d then
    begin
     Result := i;
     d := Abs(Data^);
    end;
   Inc(Data);
  end;
end;
{$ELSE}
{$IFDEF CPUx86_64}
var
  i : Integer;
  d : Double;
begin
 Result := 0;
 Assert(SampleCount > 0);
 d := Abs(Data^);
 for i := 1 to SampleCount - 1 do
  begin
   if Abs(Data^) > d then
    begin
     Result := i;
     d := Abs(Data^);
    end;
   Inc(Data);
  end;
end;
{$ELSE}
asm
 test EDX,EDX
 jz @End

 mov result,EDX                // Result := EDX
 DEC EDX
 jnz @End                      // only one sample -> exit!
 FLD  [EAX+4*EDX].Single       // Value
 fabs                          // |Value| = Max

 @FindMaxLoop:
   FLD  [EAX+4*EDX-4].Single   // Value, Max
   fabs                        // |Value|, Max

   fcomi ST(0), ST(1)          // |Value| <-> Max ?
   fstsw ax                    // ax = FPU Status Word
   sahf                        // ax -> EFLAGS register
   jae @NextSample             // if |Value| <-> Max then next sample!
   FXCH                        // OldMax, |Value|
   mov result,EDX              // Result := EDX

   @NextSample:
   FSTP ST(0)                  // Value, Max
   DEC EDX
 jnz @FindMaxLoop

 mov EDX,result              // EDX := Result
 sub EDX,1                   // EDX := EDX - 1  -> index starts at 0!
 mov result,EDX              // Result := EDX

 @End:
end;
{$ENDIF}
{$ENDIF}

function FindMaximum(Data: PDouble; SampleCount: Integer): Integer;
{$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  i : Integer;
  d : Double;
begin
 Result := 0;
 Assert(SampleCount > 0);
 d := abs(Data^);
 for i := 1 to SampleCount - 1 do
  begin
   if abs(Data^) > d then
    begin
     Result := i;
     d := abs(Data^);
    end;
   Inc(Data);
  end;
end;
{$ELSE}
asm
 test EDX,EDX
 jz @End

 mov result,EDX                // Result := EDX
 DEC EDX
 jz @End                       // only one sample -> exit!
 FLD  [EAX+8*EDX].Double       // Value
 fabs                          // |Value| = Max

 @FindMaxLoop:
   FLD  [EAX+8*EDX-8].Double   // Value, Max
   fabs                        // |Value|, Max

   fcomi ST(0), ST(1)          // |Value| <-> Max ?
   fstsw ax                    // ax = FPU Status Word
   sahf                        // ax -> EFLAGS register
   jae @NextSample             // if |Value| <-> Max then next sample!
   FXCH                        // OldMax, |Value|
   mov result,EDX              // Result := EDX

   @NextSample:
   FSTP ST(0)                  // Value, Max
   DEC EDX
 jnz @FindMaxLoop

 mov EDX,result              // EDX := Result
 sub EDX,1                   // EDX := EDX - 1  -> index starts at 0!
 mov result,EDX              // Result := EDX

 @End:
end;
{$ENDIF}

procedure CalcMinMax(Data: PSingle; SampleCount: Integer; var MinMax: TDAVMinMaxSingle);
var
  i : Integer;
begin
 Assert(SampleCount > 0);
 MinMax.min := Data^;
 MinMax.max := Data^;
 for i := 1 to SampleCount - 1 do
  begin
   if Data^ > MinMax.max then MinMax.max := Data^ else
   if Data^ < MinMax.min then MinMax.min := Data^;
   Inc(Data);
  end;
end;

procedure CalcMinMax(Data: PDouble; SampleCount: Integer; var MinMax: TDAVMinMaxDouble);
var
  i : Integer;
begin
 Assert(SampleCount > 0);
 MinMax.min := Data^;
 MinMax.max := Data^;
 for i := 1 to SampleCount - 1 do
  begin
   if Data^ > MinMax.max then MinMax.max := Data^ else
   if Data^ < MinMax.min then MinMax.min := Data^;
   Inc(Data);
  end;
end;

procedure FillWithZeroes(StartAdr: PDAVDoubleFixedArray; StartPos, EndPos, SampleCount: Integer);
begin
 // Set rest to zero
 if StartPos < EndPos
  then
   begin
    FillChar(StartAdr[0], StartPos * SizeOf(StartAdr[0]), 0);
    FillChar(StartAdr[EndPos + 1], (SampleCount - EndPos - 1) * SizeOf(StartAdr[0]), 0);
   end
  else FillChar(StartAdr[EndPos + 1], (StartPos - EndPos - 1) * SizeOf(StartAdr[0]), 0);
end;

procedure FillWithZeroes(StartAdr: PDAVSingleFixedArray; StartPos, EndPos, SampleCount: Integer);
begin
 // Set rest to zero
 if StartPos < EndPos
  then
   begin
    FillChar(StartAdr[0], StartPos * SizeOf(StartAdr[0]), 0);
    FillChar(StartAdr[EndPos + 1], (SampleCount - EndPos - 1) * SizeOf(StartAdr[0]), 0);
   end
  else FillChar(StartAdr[EndPos + 1], (StartPos - EndPos - 1) * SizeOf(StartAdr[0]), 0);
end;

procedure InvertBuffer(Data: PDAVSingleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := -Data[Sample];
end;

procedure InvertBuffer(Data: PDAVDoubleFixedArray; SampleCount: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleCount - 1
  do Data[Sample] := -Data[Sample];
end;

procedure QuickSort32(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer);
var
  I, J: Integer;
  P, T: Single;
begin
 repeat
  I := StartSample;
  J := EndSample;
  P := Data[(StartSample + EndSample) shr 1];
  repeat
    while Data[I] < P do Inc(I);
    while Data[J] > P do DEC(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       Inc(I);
       DEC(J);
      end;
    until I > J;
   if StartSample < J then QuickSort32(Data, StartSample, J);
   StartSample := I;
  until I >= EndSample;
end;

procedure QuickSort64(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer);
var
  I, J: Integer;
  P, T: Double;
begin
 repeat
  I := StartSample;
  J := EndSample;
  P := Data[(StartSample + EndSample) shr 1];
  repeat
    while Data[I] < P do Inc(I);
    while Data[J] > P do DEC(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       Inc(I);
       DEC(J);
      end;
    until I > J;
   if StartSample < J then QuickSort64(Data, StartSample, J);
   StartSample := I;
  until I >= EndSample;
end;

procedure QuickSortWithPosition(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray);
var
  I, J, K : Integer;
  P, T    : Single;
begin
 repeat
  I := StartSample;
  J := EndSample;
  P := Data[(StartSample + EndSample) shr 1];
  repeat
   while Data[I] < P do Inc(I);
   while Data[J] > P do DEC(J);
    if I <= J then
     begin
      T := Data[I];
      Data[I] := Data[J];
      Data[J] := T;
      K := Positions[I];
      Positions[I] := Positions[J];
      Positions[J] := K;
      Inc(I);
      DEC(J);
     end;
   until I > J;

   if StartSample < J then QuickSortWithPosition(Data, StartSample, J, Positions);
   StartSample := I;
  until I >= EndSample;
end;

procedure ReorderPositions(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray);
var
  I, J, K, P : Integer;
  T          : Single;
begin
 repeat
  I := StartSample;
  J := EndSample;
  P := Positions[(StartSample + EndSample) shr 1];
  repeat
    while Positions[I] < P do Inc(I);
    while Positions[J] > P do DEC(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       K := Positions[I];
       Positions[I] := Positions[J];
       Positions[J] := K;
       Inc(I);
       DEC(J);
      end;
    until I > J;
   if StartSample < J then ReorderPositions(Data, StartSample, J, Positions);
   StartSample := I;
  until I >= EndSample;
end;

procedure QuickSortWithPosition(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray);
var
  I, J, K : Integer;
  P, T    : Double;
begin
 repeat
  I := StartSample;
  J := EndSample;
  P := Data[(StartSample + EndSample) shr 1];
  repeat
   while Data[I] < P do Inc(I);
   while Data[J] > P do DEC(J);
    if I <= J then
     begin
      T := Data[I];
      Data[I] := Data[J];
      Data[J] := T;
      K := Positions[I];
      Positions[I] := Positions[J];
      Positions[J] := K;
      Inc(I);
      DEC(J);
     end;
   until I > J;

   if StartSample < J then QuickSortWithPosition(Data, StartSample, J, Positions);
   StartSample := I;
  until I >= EndSample;
end;

procedure ReorderPositions(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray);
var
  I, J, K, P : Integer;
  T          : Double;
begin
 repeat
  I := StartSample;
  J := EndSample;
  P := Positions[(StartSample + EndSample) shr 1];
  repeat
    while Positions[I] < P do Inc(I);
    while Positions[J] > P do DEC(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       K := Positions[I];
       Positions[I] := Positions[J];
       Positions[J] := K;
       Inc(I);
       DEC(J);
      end;
    until I > J;
   if StartSample < J then ReorderPositions(Data, StartSample, J, Positions);
   StartSample := I;
  until I >= EndSample;
end;

procedure BindFunctions;
begin

end;

initialization
  BindFunctions;

end.
