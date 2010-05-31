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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
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

procedure QuickSort32(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer);
procedure QuickSort64(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer);
procedure QuickSortWithPosition(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;
procedure QuickSortWithPosition(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;
procedure ReorderPositions(Data: PDAVSingleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;
procedure ReorderPositions(Data: PDAVDoubleFixedArray; StartSample, EndSample: Integer; Positions: PIntegerArray); overload;

implementation

procedure ComplexMultiplyBlock32(const Buffer, Filter: PDAVComplexSingleFixedArray; const SampleCount: Integer); overload;
{$IFDEF PUREPASCAL}
var
  SampleIndex : Integer;
begin
 Buffer^[0].Re := Buffer^[0].Re * Filter^[0].Re;
 Buffer^[0].Im := Buffer^[0].Im * Filter^[0].Im;

 for SampleIndex := 1 to SampleCount - 1
  do ComplexMultiplyInplace(Buffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 dec ecx
@Start:
  fld   [eax    ].Single  // A.Re
  fld   [eax + 4].Single  // A.Im, A.Re
  fld   [edx    ].Single  // B.Re, A.Im, A.Re
  fld   [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp st(1), st(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp  [eax    ].Single  // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch  st(2)             // A.Im, B.Re, B.Im, A.Re
  fmulp                   // A.Im * B.Re, B.Im, A.Re
  fxch  st(2)             // B.Im, A.Re, A.Im * B.Re
  fmulp                   // B.Im * A.Re, A.Im * B.Re
  faddp st(1), st(0)      // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 4].Single   // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
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
  do OutBuffer^[SampleIndex] := ComplexMultiply(InBuffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 dec ecx
@Start:
  fld   [eax    ].Single  // A.Re
  fld   [eax + 4].Single  // A.Im, A.Re
  fld   [edx    ].Single  // B.Re, A.Im, A.Re
  fld   [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp st(1), st(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp  [ebx    ].Single  // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch  st(2)             // A.Im, B.Re, B.Im, A.Re
  fmulp                   // A.Im * B.Re, B.Im, A.Re
  fxch  st(2)             // B.Im, A.Re, A.Im * B.Re
  fmulp                   // B.Im * A.Re, A.Im * B.Re
  faddp st(1), st(0)      // A.Im * B.Re + A.Re * B.Im
  fstp [ebx + 4].Single   // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add ebx, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single

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
  do ComplexMultiplyInplace(Buffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 dec ecx
@Start:
  fld   [eax    ].Double  // A.Re
  fld   [eax + 8].Double  // A.Im, A.Re
  fld   [edx    ].Double  // B.Re, A.Im, A.Re
  fld   [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp st(1), st(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Double   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)              // A.Im, B.Re, B.Im, A.Re
  fmulp                   // A.Im * B.Re, B.Im, A.Re
  fxch st(2)              // B.Im, A.Re, A.Im * B.Re
  fmulp                   // B.Im * A.Re, A.Im * B.Re
  faddp st(1), st(0)      // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 8].Double   // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
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
  do OutBuffer^[SampleIndex] := ComplexMultiply(InBuffer^[SampleIndex], Filter^[SampleIndex]);
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double
 add eax, 8
 add ebx, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double
 add eax, 8
 add ebx, 8
 add edx, 8

 dec ecx
@Start:
  fld   [eax    ].Double  // A.Re
  fld   [eax + 8].Double  // A.Im, A.Re
  fld   [edx    ].Double  // B.Re, A.Im, A.Re
  fld   [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld   st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul  st(0), st(2)      // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fsubp st(1), st(0)      // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp  [ebx    ].Double   // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch  st(2)             // A.Im, B.Re, B.Im, A.Re
  fmulp                   // A.Im * B.Re, B.Im, A.Re
  fxch  st(2)             // B.Im, A.Re, A.Im * B.Re
  fmulp                   // B.Im * A.Re, A.Im * B.Re
  faddp st(1), st(0)      // A.Im * B.Re + A.Re * B.Im
  fstp  [ebx + 8].Double  // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add ebx, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double

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
  do ComplexMultiplyInplace(InplaceBuffer^[SampleIndex], ComplexConjugate(Signal^[SampleIndex]));
{$ELSE}
asm
 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
 add eax, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re - A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
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
  do OutBuffer^[SampleIndex] := ComplexMultiply(InBuffer^[SampleIndex], ComplexConjugate(Signal^[SampleIndex]));
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single
 add eax, 4
 add ebx, 4
 add edx, 4

 dec ecx
@Start:
  fld [eax    ].Single  // A.Re
  fld [eax + 4].Single  // A.Im, A.Re
  fld [edx    ].Single  // B.Re, A.Im, A.Re
  fld [edx + 4].Single  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re + A.Re * B.Im
  fstp [ebx + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add ebx, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [ebx].Single

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
  do ComplexMultiplyInplace(InplaceBuffer^[SampleIndex], ComplexConjugate(Signal^[SampleIndex]));
{$ELSE}
asm
 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
 add eax, 8
 add edx, 8

 dec ecx
@Start:
  fld [eax    ].Double  // A.Re
  fld [eax + 8].Double  // A.Im, A.Re
  fld [edx    ].Double  // B.Re, A.Im, A.Re
  fld [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
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
  do OutBuffer^[SampleIndex] := ComplexMultiply(InBuffer^[SampleIndex], ComplexConjugate(Signal^[SampleIndex]));
{$ELSE}
asm
 push ebx
 mov ebx, OutBuffer

 // DC
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double
 add eax, 8
 add ebx, 8
 add edx, 8

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double
 add eax, 8
 add ebx, 8
 add edx, 8

 dec ecx
@Start:
  fld [eax    ].Double  // A.Re
  fld [eax + 8].Double  // A.Im, A.Re
  fld [edx    ].Double  // B.Re, A.Im, A.Re
  fld [edx + 8].Double  // B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fld st(3)             // A.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  fmul st(0), st(2)     // A.Im * B.Im, A.Re * B.Re, B.Im, B.Re, A.Im, A.Re
  faddp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  fsubp                 // A.Im * B.Re + A.Re * B.Im
  fstp [ebx + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add ebx, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [ebx].Double

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
   dec EDX
   fadd  [EAX + 4 * EDX].Single  // DC = DC + Value
 jnz @CalcDCLoop
 pop edx

 mov  [ESP - 4], EDX
 fild [ESP - 4].Integer          // Length, DC
 fdivp st(1), st(0)              // RealDC = DC / Length

 @SubstractDCLoop:
   dec EDX
   fld  [EAX + 4 * edx].Single   // Value, RealDC
   fsub st(0), st(1)             // Value-RealDC, RealDC
   fstp  [EAX + 4 * edx].Single  // RealDC
 jnz @SubstractDCLoop
 fstp st(0)                      // clear stack

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
 test edx,edx
 jz @End

 push edx
 fldz                            // DC
 @CalcDCLoop:
   dec edx
   fadd  [eax + 8 * edx].Double  // DC = DC + Value
 jnz @CalcDCLoop
 pop edx

 mov [esp - 4], edx
 fild [esp - 4].Integer          // Length, DC
 fdivp st(1), st(0)              // RealDC = DC / Length

 @SubstractDCLoop:
   dec edx
   fld  [eax + 8 * edx].Double  // Value, RealDC
   fsub st(0), st(1)            // Value-RealDC, RealDC
   fstp  [eax + 8 * edx].Double // RealDC
 jnz @SubstractDCLoop
 fstp st(0)                     // clear stack

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
 fld  [eax + ecx * 4 - 4].Single
 fstp [edx + ecx * 8 - 8].Double
 loop @Start
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
 fld [eax + ecx * 8 - 8].Double
 fstp [edx + ecx * 4 - 4].Single
 loop @Start
end;
{$ENDIF}

function FindMaximum(Data: PSingle; SampleCount: Integer): Integer;
{$IFDEF PUREPASCAL}
var i : Integer;
    d : Double;
begin
 result := 0;
 Assert(SampleCount > 0);
 d := abs(Data^);
 for i:=1 to SampleCount-1 do
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
 test edx,edx
 jz @End

 mov result,edx                // Result := edx
 dec edx
 jnz @End                      // only one sample -> exit!
 fld  [eax+4*edx].Single       // Value
 fabs                          // |Value| = Max

 @FindMaxLoop:
   fld  [eax+4*edx-4].Single   // Value, Max
   fabs                        // |Value|, Max

   fcomi st(0), st(1)          // |Value| <-> Max ?
   fstsw ax                    // ax = FPU Status Word
   sahf                        // ax -> EFLAGS register
   jae @NextSample             // if |Value| <-> Max then next sample!
   fxch                        // OldMax, |Value|
   mov result,edx              // Result := edx

   @NextSample:
   fstp st(0)                  // Value, Max
   dec edx
 jnz @FindMaxLoop

 mov edx,result              // edx := Result
 sub edx,1                   // edx := edx - 1  -> index starts at 0!
 mov result,edx              // Result := edx

 @End:
end;
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
 test edx,edx
 jz @End

 mov result,edx                // Result := edx
 dec edx
 jz @End                       // only one sample -> exit!
 fld  [eax+8*edx].Double       // Value
 fabs                          // |Value| = Max

 @FindMaxLoop:
   fld  [eax+8*edx-8].Double   // Value, Max
   fabs                        // |Value|, Max

   fcomi st(0), st(1)          // |Value| <-> Max ?
   fstsw ax                    // ax = FPU Status Word
   sahf                        // ax -> EFLAGS register
   jae @NextSample             // if |Value| <-> Max then next sample!
   fxch                        // OldMax, |Value|
   mov result,edx              // Result := edx

   @NextSample:
   fstp st(0)                  // Value, Max
   dec edx
 jnz @FindMaxLoop

 mov edx,result              // edx := Result
 sub edx,1                   // edx := edx - 1  -> index starts at 0!
 mov result,edx              // Result := edx

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
    while Data[J] > P do Dec(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       Inc(I);
       Dec(J);
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
    while Data[J] > P do Dec(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       Inc(I);
       Dec(J);
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
   while Data[J] > P do Dec(J);
    if I <= J then
     begin
      T := Data[I];
      Data[I] := Data[J];
      Data[J] := T;
      K := Positions[I];
      Positions[I] := Positions[J];
      Positions[J] := K;
      Inc(I);
      Dec(J);
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
    while Positions[J] > P do Dec(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       K := Positions[I];
       Positions[I] := Positions[J];
       Positions[J] := K;
       Inc(I);
       Dec(J);
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
   while Data[J] > P do Dec(J);
    if I <= J then
     begin
      T := Data[I];
      Data[I] := Data[J];
      Data[J] := T;
      K := Positions[I];
      Positions[I] := Positions[J];
      Positions[J] := K;
      Inc(I);
      Dec(J);
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
    while Positions[J] > P do Dec(J);
     if I <= J then
      begin
       T := Data[I];
       Data[I] := Data[J];
       Data[J] := T;
       K := Positions[I];
       Positions[I] := Positions[J];
       Positions[J] := K;
       Inc(I);
       Dec(J);
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
