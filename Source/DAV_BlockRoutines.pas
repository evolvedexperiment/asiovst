unit DAV_BlockRoutines;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2009        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.INC}

uses
  DAV_Types, DAV_Complex;

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
procedure MixBuffers_FPU(InBuffer: PDouble; MixBuffer: PDouble; SampleFrames: Integer); overload;
procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexSingleFixedArray;
  const SampleFrames: Integer; const OutBuffer: PDAVComplexSingleFixedArray); overload;
procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexDoubleFixedArray;
  const SampleFrames: Integer); overload;
procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;

function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer; overload;
function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer; overload;
procedure CalcMinMax(InBuffer: PSingle; Samples: Integer; var MinMax : TDAVMinMaxSingle); overload;
procedure CalcMinMax(InBuffer: PDouble; Samples: Integer; var MinMax : TDAVMinMaxDouble); overload;
procedure DCSubstract(InBuffer: PSingle; Samples: Integer); overload;
procedure DCSubstract(InBuffer: PDouble; Samples: Integer); overload;
procedure ConvertSingleToDouble(Singles: PDAVSingleFixedArray; Doubles: PDAVDoubleFixedArray; SampleFrames: Integer);
procedure ConvertDoubleToSingle(Doubles: PDAVDoubleFixedArray; Singles: PDAVSingleFixedArray; SampleFrames: Integer);

procedure FillWithZeroes(StartAdr: PDAVSingleFixedArray; StartPos, EndPos, SampleCount: Integer); overload;
procedure FillWithZeroes(StartAdr: PDAVDoubleFixedArray; StartPos, EndPos, SampleCount: Integer); overload;

implementation

procedure MixBuffers_FPU(InBuffer: PSingle; MixBuffer: PSingle; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 4 * ecx - 4].Single
  fadd  [edx + 4 * ecx - 4].Single
  fstp  [edx + 4 * ecx - 4].Single
  loop @Start
end;

procedure MixBuffers_FPU(InBuffer: PDouble; MixBuffer: PDouble; SampleFrames: Integer); overload;
asm
@Start:
  fld   [eax + 8 * ecx - 8].Double
  fadd  [edx + 8 * ecx - 8].Double
  fstp  [edx + 8 * ecx - 8].Double
  loop @Start
end;

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexSingleFixedArray; const SampleFrames: Integer); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 4].Single // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 8
  add edx, 8
 loop @Start

 // Nyquist
 fld   [eax].Single
 fmul  [edx].Single
 fstp  [eax].Single
end;

procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexSingleFixedArray;
  const SampleFrames: Integer; const OutBuffer: PDAVComplexSingleFixedArray); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Single // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
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
end;

procedure ComplexMultiply(const InplaceBuffer, Filter: PDAVComplexDoubleFixedArray;
  const SampleFrames: Integer); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [eax    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
  fstp [eax + 8].Double // A.Im := A.Im * B.Re + A.Re * B.Im
  add eax, 16
  add edx, 16
 loop @Start

 // Nyquist
 fld   [eax].Double
 fmul  [edx].Double
 fstp  [eax].Double
end;

procedure ComplexMultiply(const InBuffer, Filter: PDAVComplexDoubleFixedArray; const SampleFrames: Integer;
  const OutBuffer: PDAVComplexDoubleFixedArray); overload;
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
  fsubp                 // A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fstp [ebx    ].Double // A.Re = A.Re * B.Re - A.Im * B.Im, B.Im, B.Re, A.Im, A.Re
  fxch st(2)            // A.Im, B.Re, B.Im, A.Re
  fmulp                 // A.Im * B.Re, B.Im, A.Re
  fxch st(2)            // B.Im, A.Re, A.Im * B.Re
  fmulp                 // B.Im * A.Re, A.Im * B.Re
  faddp                 // A.Im * B.Re + A.Re * B.Im
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
end;

procedure DCSubstract(InBuffer: PSingle; Samples: Integer);
{$IFDEF PUREPASCAL}
var
  InBuf : array [0..0] of Double absolute InBuffer;
  d : Double;
  i : Integer;
begin
 if Samples = 0 then Exit;
 d := InBuf[0];
 for i := 1 to Samples - 1
  do d := d + InBuf[i];
 d := d / Samples;
 for i := 0 to Samples - 1
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
 fdivp                           // RealDC = DC / Length

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

procedure DCSubstract(InBuffer: PDouble; Samples: Integer);
{$IFDEF PUREPASCAL}
var
  InBuf : array [0..0] of Double absolute InBuffer;
  d : Double;
  i : Integer;
begin
 if Samples = 0 then Exit;
 d := InBuf[0];
 for i := 1 to Samples - 1
  do d := d + InBuf[i];
 d := d / Samples;
 for i := 0 to Samples - 1
  do InBuf[i] := InBuf[i] - d;
end;
{$ELSE}
asm
 test edx,edx
 jz @End

 push edx
 fldz                          // DC
 @CalcDCLoop:
   dec edx
   fadd  [eax+8*edx].Double    // DC = DC + Value
 jnz @CalcDCLoop
 pop edx

 mov [esp-4],edx
 fild [esp-4].Integer          // Length, DC
 fdivp                         // RealDC = DC / Length

 @SubstractDCLoop:
   dec edx
   fld  [eax+8*edx].Double     // Value, RealDC
   fsub st(0),st(1)            // Value-RealDC, RealDC
   fstp  [eax+8*edx].Double    // RealDC
 jnz @SubstractDCLoop
 fstp st(0)                    // clear stack

 @End:
end;
{$ENDIF}

procedure ConvertSingleToDouble(Singles: PDAVSingleFixedArray; Doubles: PDAVDoubleFixedArray; SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Doubles^ := Singles^;
   Inc(Singles);
   Inc(Doubles);
  end;
end;
{$ELSE}
asm
@MarioLand:
 fld  [eax + ecx * 4 - 4].Single
 fstp [edx + ecx * 8 - 8].Double
 loop @MarioLand
end;
{$ENDIF}

procedure ConvertDoubleToSingle(Doubles: PDAVDoubleFixedArray; Singles: PDAVSingleFixedArray; SampleFrames: Integer);
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Singles^ := Doubles^;
   Inc(Singles);
   Inc(Doubles);
  end;
end;
{$ELSE}
asm
@MarioLand:
 fld [eax + ecx * 8 - 8].Double
 fstp [edx + ecx * 4 - 4].Single
 loop @MarioLand
end;
{$ENDIF}

function FindMaximum(InBuffer: PSingle; Samples: Integer): Integer;
{$IFDEF PUREPASCAL}
var i : Integer;
    d : Double;
begin
 result := 0;
 assert(Samples > 0);
 d := abs(InBuffer^);
 for i:=1 to Samples-1 do
  begin
   if abs(InBuffer^) > d then
    begin
     Result := i;
     d := abs(InBuffer^);
    end;
   Inc(InBuffer);
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

function FindMaximum(InBuffer: PDouble; Samples: Integer): Integer;
{$DEFINE PUREPASCAL}
{$IFDEF PUREPASCAL}
var
  i : Integer;
  d : Double;
begin
 result := 0;
 assert(Samples > 0);
 d := abs(InBuffer^);
 for i := 1 to Samples - 1 do
  begin
   if abs(InBuffer^) > d then
    begin
     Result := i;
     d := abs(InBuffer^);
    end;
   Inc(InBuffer);
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

procedure CalcMinMax(InBuffer: PSingle; Samples: Integer; var MinMax: TDAVMinMaxSingle);
var
  i : Integer;
begin
 assert(Samples > 0);
 MinMax.min := InBuffer^;
 MinMax.max := InBuffer^;
 for i := 1 to Samples - 1 do
  begin
   if InBuffer^ > MinMax.max then MinMax.max := InBuffer^ else
   if InBuffer^ < MinMax.min then MinMax.min := InBuffer^;
   Inc(InBuffer);
  end;
end;

procedure CalcMinMax(InBuffer: PDouble; Samples: Integer; var MinMax: TDAVMinMaxDouble);
var
  i : Integer;
begin
 assert(Samples > 0);
 MinMax.min := InBuffer^;
 MinMax.max := InBuffer^;
 for i := 1 to Samples - 1 do
  begin
   if InBuffer^ > MinMax.max then MinMax.max := InBuffer^ else
   if InBuffer^ < MinMax.min then MinMax.min := InBuffer^;
   Inc(InBuffer);
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

end.
