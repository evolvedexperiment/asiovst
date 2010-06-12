unit DAV_BlockConvert32;

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
//                                                                            //
//  Assembler code optimizations are based on Agner Fog's excellent           //
//  documentations. In particular the following document has been studied:    //
//                                                                            //
//  http://www.agner.org/optimize/optimizing_assembly.pdf                     //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

{-$DEFINE PUREPASCAL}

uses
  DAV_Bindings;

{ Prototype }

type
  TBlockConvertToFloat32 = procedure(Destination: PSingle; Source: Pointer; Count: LongInt);


{ Function Pointers }

var
  BlockConvertInt16LSBToFloat32   : TBlockConvertToFloat32;
  BlockConvertInt24LSBToFloat32   : TBlockConvertToFloat32;
  BlockConvertInt32LSBToFloat32   : TBlockConvertToFloat32;
  BlockConvertInt32LSB16ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt32LSB18ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt32LSB20ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt32LSB24ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt16MSBToFloat32   : TBlockConvertToFloat32;
  BlockConvertInt24MSBToFloat32   : TBlockConvertToFloat32;
  BlockConvertInt32MSBToFloat32   : TBlockConvertToFloat32;
  BlockConvertInt32MSB16ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt32MSB18ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt32MSB20ToFloat32 : TBlockConvertToFloat32;
  BlockConvertInt32MSB24ToFloat32 : TBlockConvertToFloat32;


{ Binding Function Pointers }

var
  BindingBlockConvertToFloat64ToFloat32  : TFunctionBinding;
  BindingBlockConvertInt16LSBToFloat32   : TFunctionBinding;
  BindingBlockConvertInt24LSBToFloat32   : TFunctionBinding;
  BindingBlockConvertInt32LSBToFloat32   : TFunctionBinding;
  BindingBlockConvertInt32LSB16ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt32LSB18ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt32LSB20ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt32LSB24ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt16MSBToFloat32   : TFunctionBinding;
  BindingBlockConvertInt24MSBToFloat32   : TFunctionBinding;
  BindingBlockConvertInt32MSBToFloat32   : TFunctionBinding;
  BindingBlockConvertInt32MSB16ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt32MSB18ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt32MSB20ToFloat32 : TFunctionBinding;
  BindingBlockConvertInt32MSB24ToFloat32 : TFunctionBinding;

  BindingBlockConvertToFloat32 : TFunctionBindingList;

implementation

uses
  DAV_Common;

const
  CFloatToShort   : Single = $7F;
  CShortToFloat   : Single = 1 / $7F;
  CFloatToSmall   : Single = $7FFF;
  CSmallToFloat   : Single = 1 / $7FFF;
  CFloatToInt18   : Double = $1FFFF;
  CInt18ToFloat   : Double = 1 / $1FFFF;
  CFloatToInt20   : Double = $7FFFF;
  CInt20ToFloat   : Double = 1 / $7FFFF;
  CFloatToInt24   : Double = $7FFFFF;
  CInt24ToFloat   : Double = 1 / $7FFFFF;
  CFloatToInt32   : Double = $7FFFFFFF;
  CInt32ToFloat   : Double = 1 / $7FFFFFFF;

  // Note:
  // Single = 1 / $7FFF     <=>  Integer = $38000100
  // Single = 1 / $7FFFFFFF <=>  Integer = $30000000

  // EMMS (single precision)
  C2Int32ToSingle : array [0..1] of Integer = ($30000000, $30000000);
  C2Int16ToSingle : array [0..1] of Integer = ($38000100, $38000100);

  // SSE & SSE2 (single precision)
  C4Int32ToSingle : array [0..3] of Integer = ($30000000, $30000000,
    $30000000, $30000000);
  C4SmallToSingle : array [0..3] of Integer = ($38000100, $38000100,
    $38000100, $38000100);
  C4Int18ToSingle : array [0..3] of Single = (1 / $1FFFF, 1 / $1FFFF,
    1 / $1FFFF, 1 / $1FFFF);
  C4Int20ToSingle : array [0..3] of Single = (1 / $7FFFF, 1 / $7FFFF,
    1 / $7FFFF, 1 / $7FFFF);
  C4Int24ToSingle : array [0..3] of Single = (1 / $7FFFFF, 1 / $7FFFFF,
    1 / $7FFFFF, 1 / $7FFFFF);

procedure BlockConvertInt16LSBToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceWord : PWord absolute Source;
  Index      : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceWord^ * CSmallToFloat;
   Inc(SourceWord);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 2
 NEG     ECX
 JNL     @Done

 FLD     CSmallToFloat

@Start:
 FILD    [EDX + ECX * 2].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24LSBToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt  : PInteger absolute Source;
  SourceByte : PByte absolute Source;
  Index      : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := (SourceInt^ and $FFFFFF00) * CInt32ToFloat;
   Inc(SourceByte, 3);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt32ToFloat
 PUSH    EBX

@Start:
 MOV     EBX, [EDX].DWord
 SHL     EBX, 8
 AND     EBX, $FFFFFF00

 MOV     [ESP - 4], EBX
 FILD    [ESP - 4].Single
 FMUL    ST(0), ST(1)

 FSTP    [EAX + ECX * 4].Single
 ADD     EDX, 3
 ADD     ECX, 1
 JS      @Start

 POP     EBX
 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSBToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceInt^ * CInt32ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt32ToFloat

@Start:
 FILD    [EDX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB16ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceInt^ * CSmallToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CSmallToFloat

@Start:
 FILD    [EDX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB18ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceInt^ * CInt18ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD      CInt18ToFloat

@Start:
 FILD    [EDX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB20ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceInt^ * CInt20ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt20ToFloat

@Start:
 FILD    [EDX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB24ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceInt^ * CInt24ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt24ToFloat

@Start:
 FILD    [EDX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt16MSBToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceWord : PWord absolute Source;
  Index      : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap(SourceWord^) * CSmallToFloat;
   Inc(SourceWord);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 2
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CSmallToFloat

@Start:
 MOV     BX, [EDX + 2 * ECX]
 XCHG    BH, BL
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24MSBToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
type
  TByte3Array = Array [0..2] of Byte;
  PByte3Array = ^TByte3Array;
var
  SourceBytes : PByte3Array absolute Source;
  Index       : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := ((SourceBytes^[0] shl 32) + (SourceBytes^[1] shl 24) +
     (SourceBytes^[2] shl 16)) * CInt32ToFloat;
   Inc(SourceBytes, 3);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt24ToFloat
 PUSH    EBX

@Start:
 XOR     EBX, EBX

 MOV     BL, [EDX + 2]
 MOV     BH, [EDX + 1]
 ROR     EBX, 8
 MOV     BH, [EAX]
 ROL     EBX, 8

 MOV     [ESP-4], EBX
 FILD    [ESP-4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     EDX, 3
 ADD     ECX, 1
 JS      @Start

 POP     EBX
 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSBToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap32(SourceInt^) * CInt32ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt32ToFloat

@Start:
 MOV     EBX, [EDX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB16ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap32(SourceInt^) * CSmallToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CSmallToFloat

@Start:
 MOV     EBX, [EDX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB18ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap32(SourceInt^) * CInt18ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt18ToFloat

@Start:
 MOV     EBX, [EDX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB20ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap32(SourceInt^) * CInt20ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt20ToFloat

@Start:
 MOV     EBX, [EDX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB24ToFloat32Native(Destination: PSingle;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap32(SourceInt^) * CInt24ToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, EAX + ECX * 4
 LEA     EDX, EDX + ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt24ToFloat

@Start:
 MOV     EBX, [EDX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EAX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}


{$IFNDEF PUREPASCAL}

{ SSE optimizations }

procedure BlockConvertInt16LSBToFloat32SSE2(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 PUSH     EBX

 MOV      EBX, EAX
 AND      EBX, $F
 JNZ      @Fallback

 MOV      EBX, EDX
 AND      EBX, $F
 JNZ      @Fallback

 MOVUPS   XMM0, C4SmallToSingle

 SUB      ECX, 4
 LEA      EAX, EAX + ECX * 4
 LEA      EDX, EDX + ECX * 2
 NEG      ECX
 JG       @Reminder

@MainAlgorithm:
 XORPS    XMM1, XMM1
 PINSRW   XMM1, [EDX + ECX * 2    ], 0
 PINSRW   XMM1, [EDX + ECX * 2 + 2], 2
 PINSRW   XMM1, [EDX + ECX * 2 + 4], 4
 PINSRW   XMM1, [EDX + ECX * 2 + 6], 6
 CVTDQ2PS XMM1, XMM1
 MULPS    XMM1, XMM0
 MOVAPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 4
 JLE      @MainAlgorithm

@Reminder:
 SUB      ECX, 4
 JNS      @Done

@ReminderLoop:
 XORPS    XMM1, XMM1
 PINSRW   XMM1, [EDX + ECX * 2 + 8], 0
 CVTDQ2PS XMM1, XMM1
 MULPS    XMM1, XMM0
 MOVUPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 1
 JS       @ReminderLoop
 JMP      @Done

@Fallback:
 CALL     BlockConvertInt16LSBToFloat32Native

@Done:
 POP      EBX
end;

procedure BlockConvertInt32LSBToFloat32SSE2(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 PUSH     EBX

 MOV      EBX, EAX
 AND      EBX, $F
 JNZ      @Fallback

 MOV      EBX, EDX
 AND      EBX, $F
 JNZ      @Fallback

 MOVUPS   XMM0, C4Int32ToSingle

 SUB      ECX, 4
 LEA      EAX, EAX + ECX * 4
 LEA      EDX, EDX + ECX * 4
 NEG      ECX
 JG       @Reminder

@MainAlgorithm:
 CVTDQ2PS XMM1, [EDX + ECX * 4]
 MULPS    XMM1, XMM0
 MOVAPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 4
 JLE      @MainAlgorithm

@Reminder:
 SUB      ECX, 4
 JNS      @Done

@ReminderLoop:
 CVTDQ2PS XMM1, [EDX + ECX * 4 + 16]
 MULPS    XMM1, XMM0
 MOVUPS   [EAX + ECX * 4 + 16], XMM1
 ADD      ECX, 1
 JS       @ReminderLoop
 JMP      @Done

@Fallback:
 CALL     BlockConvertInt32LSBToFloat32Native

@Done:
 POP      EBX
end;

procedure BlockConvertInt32LSB16ToFloat32SSE2(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 PUSH     EBX

 MOV      EBX, EAX
 AND      EBX, $F
 JNZ      @Fallback

 MOV      EBX, EDX
 AND      EBX, $F
 JNZ      @Fallback

 MOVUPS   XMM0, C4SmallToSingle

 SUB      ECX, 4
 LEA      EAX, EAX + ECX * 4
 LEA      EDX, EDX + ECX * 4
 NEG      ECX
 JG       @Reminder

@MainAlgorithm:
 CVTDQ2PS XMM1, [EDX + ECX * 4]
 MULPS    XMM1, XMM0
 MOVAPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 4
 JLE      @MainAlgorithm

@Reminder:
 SUB      ECX, 4
 JNS      @Done

@ReminderLoop:
 CVTDQ2PS XMM1, [EDX + ECX * 4 + 16]
 MULPS    XMM1, XMM0
 MOVUPS   [EAX + ECX * 4 + 16], XMM1
 ADD      ECX, 1
 JS       @ReminderLoop
 JMP      @Done

@Fallback:
 CALL     BlockConvertInt32LSB16ToFloat32Native

@Done:
 POP      EBX
end;

procedure BlockConvertInt32LSB18ToFloat32SSE2(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 PUSH     EBX

 MOV      EBX, EAX
 AND      EBX, $F
 JNZ      @Fallback

 MOV      EBX, EDX
 AND      EBX, $F
 JNZ      @Fallback

 MOVUPS   XMM0, C4Int18ToSingle

 SUB      ECX, 4
 LEA      EAX, EAX + ECX * 4
 LEA      EDX, EDX + ECX * 4
 NEG      ECX
 JG       @Reminder

@MainAlgorithm:
 CVTDQ2PS XMM1, [EDX + ECX * 4]
 MULPS    XMM1, XMM0
 MOVAPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 4
 JLE      @MainAlgorithm

@Reminder:
 SUB      ECX, 4
 JNS      @Done

@ReminderLoop:
 CVTDQ2PS XMM1, [EDX + ECX * 4 + 16]
 MULPS    XMM1, XMM0
 MOVUPS   [EAX + ECX * 4 + 16], XMM1
 ADD      ECX, 1
 JS       @ReminderLoop
 JMP      @Done

@Fallback:
 CALL     BlockConvertInt32LSB18ToFloat32Native

@Done:
 POP      EBX
end;

procedure BlockConvertInt32LSB20ToFloat32SSE2(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 PUSH     EBX

 MOV      EBX, EAX
 AND      EBX, $F
 JNZ      @Fallback

 MOV      EBX, EDX
 AND      EBX, $F
 JNZ      @Fallback

 MOVUPS   XMM0, C4Int20ToSingle

 SUB      ECX, 4
 LEA      EAX, EAX + ECX * 4
 LEA      EDX, EDX + ECX * 4
 NEG      ECX
 JG       @Reminder

@MainAlgorithm:
 CVTDQ2PS XMM1, [EDX + ECX * 4]
 MULPS    XMM1, XMM0
 MOVAPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 4
 JLE      @MainAlgorithm

@Reminder:
 SUB      ECX, 4
 JNS      @Done

@ReminderLoop:
 CVTDQ2PS XMM1, [EDX + ECX * 4 + 16]
 MULPS    XMM1, XMM0
 MOVUPS   [EAX + ECX * 4 + 16], XMM1
 ADD      ECX, 1
 JS       @ReminderLoop
 JMP      @Done

@Fallback:
 CALL     BlockConvertInt32LSB20ToFloat32Native

@Done:
 POP      EBX
end;

procedure BlockConvertInt32LSB24ToFloat32SSE2(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 PUSH     EBX

 MOV      EBX, EAX
 AND      EBX, $F
 JNZ      @Fallback

 MOV      EBX, EDX
 AND      EBX, $F
 JNZ      @Fallback

 MOVUPS   XMM0, C4Int24ToSingle

 SUB      ECX, 4
 LEA      EAX, EAX + ECX * 4
 LEA      EDX, EDX + ECX * 4
 NEG      ECX
 JG       @Reminder

@MainAlgorithm:
 CVTDQ2PS XMM1, [EDX + ECX * 4]
 MULPS    XMM1, XMM0
 MOVAPS   [EAX + ECX * 4], XMM1
 ADD      ECX, 4
 JLE      @MainAlgorithm

@Reminder:
 SUB      ECX, 4
 JNS      @Done

@ReminderLoop:
 CVTDQ2PS XMM1, [EDX + ECX * 4 + 16]
 MULPS    XMM1, XMM0
 MOVUPS   [EAX + ECX * 4 + 16], XMM1
 ADD      ECX, 1
 JS       @ReminderLoop
 JMP      @Done

@Fallback:
 CALL     BlockConvertInt32LSB24ToFloat32Native

@Done:
 POP      EBX
end;


{ 3D-Now optimizations }

procedure Int32LSBToSingle_3DNow(Destination: PSingle;
  Source: Pointer; Count: Integer);
asm
 FEMMS                          // Fast MMX Enter/Leave
 SHR       ECX, 3               // unroll the loop by 8
 MOVQ      MM4, C2Int32ToSingle // use mm4 as 1/high(Integer) divider
 PREFETCHW [EDX]                // give the mmu a heads-up,
                                // load the total line of mmx0..7 data in the cache
                                // and prepare for modification. (If I understand AMD correctly)
 @Start:
 MOVQ      MM0, [EDX     ]      // Sample 1 | Sample 2
 MOVQ      MM1, [EDX +  8]      // Sample 3 | Sample 4
 MOVQ      MM2, [EDX + 16]      // Sample 5 | Sample 7
 MOVQ      MM3, [EDX + 24]      // Sample 7 | Sample 8
 PI2FD     MM0, MM0             // convert to FP
 PI2FD     MM1, MM1
 PI2FD     MM2, MM2
 PI2FD     MM3, MM3
 PFMUL     MM0, MM4             // divide by high(Integer)
 PFMUL     MM1, MM4
 PFMUL     MM2, MM4
 PFMUL     MM3, MM4
 MOVQ      [EAX     ], MM0      // Store Sample back to RAM
 MOVQ      [EAX +  8], MM1
 MOVQ      [EAX + 16], MM2
 MOVQ      [EAX + 24], MM3
 ADD       EAX, 32
 ADD       EAX, 32
 PREFETCHW [EAX]                // Inform mmu about next Sample Position
 LOOP      @Start

 @Done:
 FEMMS                          // Fast MMX Enter/Leave
end;

{$ENDIF}


procedure BindFunctions;
begin
 // create function binding list for 32-bit float conversions
 BindingBlockConvertToFloat32 := TFunctionBindingList.Create;

 // create function binding for 16-bit integer to 32-bit float conversion
 BindingBlockConvertInt16LSBToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt16LSBToFloat32, @BlockConvertInt16LSBToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt16LSBToFloat32);
 with BindingBlockConvertInt16LSBToFloat32 do
  begin
   Add(@BlockConvertInt16LSBToFloat32Native);
   {$IFNDEF PUREPASCAL}
   Add(@BlockConvertInt16LSBToFloat32SSE2, [pfSSE, pfSSE2], $F);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for 24-bit integer to 32-bit float conversion
 BindingBlockConvertInt24LSBToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt24LSBToFloat32, @BlockConvertInt24LSBToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt24LSBToFloat32);
 with BindingBlockConvertInt24LSBToFloat32 do
  begin
   Add(@BlockConvertInt24LSBToFloat32Native);
  end;

 // create function binding for 32-bit integer to 32-bit float conversion
 BindingBlockConvertInt32LSBToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32LSBToFloat32, @BlockConvertInt32LSBToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32LSBToFloat32);
 with BindingBlockConvertInt32LSBToFloat32 do
  begin
   Add(@BlockConvertInt32LSBToFloat32Native);
   {$IFNDEF PUREPASCAL}
   Add(@BlockConvertInt32LSBToFloat32SSE2, [pfSSE, pfSSE2], $F);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for 16-bit integer to 32-bit float conversion
 BindingBlockConvertInt32LSB16ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32LSB16ToFloat32, @BlockConvertInt32LSB16ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32LSB16ToFloat32);
 with BindingBlockConvertInt32LSB16ToFloat32 do
  begin
   Add(@BlockConvertInt32LSB16ToFloat32Native);
   {$IFNDEF PUREPASCAL}
   Add(@BlockConvertInt32LSB16ToFloat32SSE2, [pfSSE, pfSSE2], $F);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for 18-bit integer to 32-bit float conversion
 BindingBlockConvertInt32LSB18ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32LSB18ToFloat32, @BlockConvertInt32LSB18ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32LSB18ToFloat32);
 with BindingBlockConvertInt32LSB18ToFloat32 do
  begin
   Add(@BlockConvertInt32LSB18ToFloat32Native);
   {$IFNDEF PUREPASCAL}
   Add(@BlockConvertInt32LSB18ToFloat32SSE2, [pfSSE, pfSSE2], $F);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for 20-bit integer to 32-bit float conversion
 BindingBlockConvertInt32LSB20ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32LSB20ToFloat32, @BlockConvertInt32LSB20ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32LSB20ToFloat32);
 with BindingBlockConvertInt32LSB20ToFloat32 do
  begin
   Add(@BlockConvertInt32LSB20ToFloat32Native);
   {$IFNDEF PUREPASCAL}
   Add(@BlockConvertInt32LSB20ToFloat32SSE2, [pfSSE, pfSSE2], $F);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for 24-bit integer to 32-bit float conversion
 BindingBlockConvertInt32LSB24ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32LSB24ToFloat32, @BlockConvertInt32LSB24ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32LSB24ToFloat32);
 with BindingBlockConvertInt32LSB24ToFloat32 do
  begin
   Add(@BlockConvertInt32LSB24ToFloat32Native);
   {$IFNDEF PUREPASCAL}
   Add(@BlockConvertInt32LSB24ToFloat32SSE2, [pfSSE, pfSSE2], $F);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for 16-bit integer to 32-bit float conversion
 BindingBlockConvertInt16MSBToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt16MSBToFloat32, @BlockConvertInt16MSBToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt16MSBToFloat32);
 with BindingBlockConvertInt16MSBToFloat32 do
  begin
   Add(@BlockConvertInt16MSBToFloat32Native);
  end;

 // create function binding for 24-bit integer to 32-bit float conversion
 BindingBlockConvertInt24MSBToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt24MSBToFloat32, @BlockConvertInt24MSBToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt24MSBToFloat32);
 with BindingBlockConvertInt24MSBToFloat32 do
  begin
   Add(@BlockConvertInt24MSBToFloat32Native);
  end;

 // create function binding for 32-bit integer to 32-bit float conversion
 BindingBlockConvertInt32MSBToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32MSBToFloat32, @BlockConvertInt32MSBToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32MSBToFloat32);
 with BindingBlockConvertInt32MSBToFloat32 do
  begin
   Add(@BlockConvertInt32MSBToFloat32Native);
  end;

 // create function binding for 16-bit integer to 32-bit float conversion
 BindingBlockConvertInt32MSB16ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32MSB16ToFloat32, @BlockConvertInt32MSB16ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32MSB16ToFloat32);
 with BindingBlockConvertInt32MSB16ToFloat32 do
  begin
   Add(@BlockConvertInt32MSB16ToFloat32Native);
  end;

 // create function binding for 18-bit integer to 32-bit float conversion
 BindingBlockConvertInt32MSB18ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32MSB18ToFloat32, @BlockConvertInt32MSB18ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32MSB18ToFloat32);
 with BindingBlockConvertInt32MSB18ToFloat32 do
  begin
   Add(@BlockConvertInt32MSB18ToFloat32Native);
  end;

 // create function binding for 20-bit integer to 32-bit float conversion
 BindingBlockConvertInt32MSB20ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32MSB20ToFloat32, @BlockConvertInt32MSB20ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32MSB20ToFloat32);
 with BindingBlockConvertInt32MSB20ToFloat32 do
  begin
   Add(@BlockConvertInt32MSB20ToFloat32Native);
  end;

 // create function binding for 24-bit integer to 32-bit float conversion
 BindingBlockConvertInt32MSB24ToFloat32 := TFunctionBinding.Create(
   @@BlockConvertInt32MSB24ToFloat32, @BlockConvertInt32MSB24ToFloat32Native);
 BindingBlockConvertToFloat32.AddBinding(BindingBlockConvertInt32MSB24ToFloat32);
 with BindingBlockConvertInt32MSB24ToFloat32 do
  begin
   Add(@BlockConvertInt32MSB24ToFloat32Native);
  end;

 // processor specific rebind
 BindingBlockConvertToFloat32.RebindProcessorSpecific;
end;

procedure UnbindFunctions;
begin
 BindingBlockConvertToFloat32.Free;
 BindingBlockConvertToFloat32 := nil;
end;

initialization
  BindFunctions;

finalization
  UnbindFunctions;

end.
