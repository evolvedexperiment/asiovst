unit DAV_BlockConvert;

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

{$DEFINE PUREPASCAL}

type
  TBlockConvertToFloat32 = procedure(Destination: PSingle; Source: Pointer; Count: LongInt);
  TBlockConvertToFloat64 = procedure(Destination: PDouble; Source: Pointer; Count: LongInt);

var
  BlockConvertFloat32ToFloat64    : procedure(Destination: PDouble; Source: PSingle; Count: Integer);
  BlockConvertFloat64ToFloat32    : procedure(Destination: PSingle; Source: PDouble; Count: Integer);

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

  BlockConvertInt16LSBToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt24LSBToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32LSBToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32LSB16ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt32LSB18ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt32LSB20ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt32LSB24ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt16MSBToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt24MSBToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32MSBToFloat64   : TBlockConvertToFloat64;
  BlockConvertInt32MSB16ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt32MSB18ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt32MSB20ToFloat64 : TBlockConvertToFloat64;
  BlockConvertInt32MSB24ToFloat64 : TBlockConvertToFloat64;

implementation

uses
  DAV_Bindings, DAV_Common;

const
  CFloatToShort : Single = $7F;
  CShortToFloat : Single = 1 / $7F;
  CFloatToSmall : Single = $7FFF;
  CSmallToFloat : Single = 1 / $7FFF;
  CFloatToInt18 : Double = $1FFFF;
  CInt18ToFloat : Double = 1 / $1FFFF;
  CFloatToInt20 : Double = $7FFFF;
  CInt20ToFloat : Double = 1 / $7FFFF;
  CFloatToInt24 : Double = $7FFFFF;
  CInt24ToFloat : Double = 1 / $7FFFFF;
  CFloatToInt   : Double = $7FFFFFFF;
  CIntToFloat   : Double = 1 / $7FFFFFFF;

procedure BlockConvertFloat32ToFloat64Native(Destination: PDouble;
  Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^;
   Inc(Destination);
   Inc(Source);
  end;
end;
{$ELSE}
asm
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done
@Start:
 FLD     [EAX + ECX * 4].Single
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

@Done:
end;
{$ENDIF}

procedure BlockConvertFloat64ToFloat32Native(Destination: PSingle;
  Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index : Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^;
   Inc(Destination);
   Inc(Source);
  end;
end;
{$ELSE}
asm
 LEA     EAX, ECX * 8
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done
@Start:
 FLD     [EAX + ECX * 8].Double
 FSTP    [EDX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

@Done:
end;
{$ENDIF}

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
 LEA     EAX, ECX * 2
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CSmallToFloat

@Start:
 FILD    [EAX + ECX * 2].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
   Destination^ := (SourceInt^ and $FFFFFF00) * CIntToFloat;
   Inc(SourceByte, 3);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CIntToFloat
 PUSH    EBX

@Start:
 MOV     EBX, [EAX].DWord
 SHL     EBX, 8
 AND     EBX, $FFFFFF00

 MOV     [ESP - 4], EBX
 FILD    [ESP - 4].Single
 FMUL    ST(0), ST(1)

 FSTP    [EDX + ECX * 4].Single
 ADD     EAX, 3
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
   Destination^ := SourceInt^ * CIntToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CIntToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CSmallToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD      CInt18ToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt20ToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt24ToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 2
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CSmallToFloat

@Start:
 MOV     BX, [EAX + 2 * ECX]
 XCHG    BH, BL
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
     (SourceBytes^[2] shl 16)) * CIntToFloat;
   Inc(SourceBytes, 3);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 FLD     CInt24ToFloat
 PUSH    EBX

@Start:
 XOR     EBX, EBX

 MOV     BL, [EAX + 2]
 MOV     BH, [EAX + 1]
 ROR     EBX, 8
 MOV     BH, [EAX]
 ROL     EBX, 8

 MOV     [ESP-4], EBX
 FILD    [ESP-4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
 ADD     EAX, 3
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
   Destination^ := Swap32(SourceInt^) * CIntToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CIntToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CSmallToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt18ToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt20ToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 4
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt24ToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 4].Single
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

{ ...ToFloat64 }

procedure BlockConvertInt16LSBToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 2
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CSmallToFloat

@Start:
 FILD    [EAX + ECX * 2].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24LSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt  : PInteger absolute Source;
  SourceByte : PByte absolute Source;
  Index      : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := (SourceInt^ and $FFFFFF00) * CIntToFloat;
   Inc(SourceByte, 3);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CIntToFloat
 PUSH    EBX

@Start:
 MOV     EBX, [EAX].DWord
 SHL     EBX, 8
 AND     EBX, $FFFFFF00

 MOV     [ESP - 4], EBX
 FILD    [ESP - 4].Single
 FMUL    ST(0), ST(1)

 FSTP    [EDX + ECX * 8].Double
 ADD     EAX, 3
 ADD     ECX, 1
 JS      @Start

 POP     EBX
 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := SourceInt^ * CIntToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CIntToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB16ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CSmallToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB18ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD      CInt18ToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB20ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CInt20ToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32LSB24ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CInt24ToFloat

@Start:
 FILD    [EAX + ECX * 4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt16MSBToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 2
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CSmallToFloat

@Start:
 MOV     BX, [EAX + 2 * ECX]
 XCHG    BH, BL
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt24MSBToFloat64Native(Destination: PDouble;
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
     (SourceBytes^[2] shl 16)) * CIntToFloat;
   Inc(SourceBytes, 3);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 FLD     CInt24ToFloat
 PUSH    EBX

@Start:
 XOR     EBX, EBX

 MOV     BL, [EAX + 2]
 MOV     BH, [EAX + 1]
 ROR     EBX, 8
 MOV     BH, [EAX]
 ROL     EBX, 8

 MOV     [ESP-4], EBX
 FILD    [ESP-4].Single
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     EAX, 3
 ADD     ECX, 1
 JS      @Start

 POP     EBX
 FFREE   ST(0)

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSBToFloat64Native(Destination: PDouble;
  Source: Pointer; Count: Integer);
{$IFDEF PUREPASCAL}
var
  SourceInt : PInteger absolute Source;
  Index     : Integer;
begin
 for Index := 0 to Count - 1 do
  begin
   Destination^ := Swap32(SourceInt^) * CIntToFloat;
   Inc(SourceInt);
   Inc(Destination);
  end;
end;
{$ELSE}
asm
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CIntToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB16ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CSmallToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB18ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt18ToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB20ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt20ToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}

procedure BlockConvertInt32MSB24ToFloat64Native(Destination: PDouble;
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
 LEA     EAX, ECX * 4
 LEA     EDX, ECX * 8
 NEG     ECX
 JNL     @Done

 PUSH    EBX
 FLD     CInt24ToFloat

@Start:
 MOV     EBX, [EAX + ECX * 4]
 BSWAP   EBX
 MOV     [ESP - 4], BX
 FILD    [ESP - 4].Word
 FMUL    ST(0), ST(1)
 FSTP    [EDX + ECX * 8].Double
 ADD     ECX, 1
 JS      @Start

 FFREE   ST(0)
 POP     EBX

@Done:
end;
{$ENDIF}


procedure BindFunctions;
begin
 // Block Inplace ADDition Binding (32 bit)
 with TFunctionBinding.Create(@@BlockConvertFloat32ToFloat64,
   @BlockConvertFloat32ToFloat64Native) do
  begin
   Add(@BlockConvertFloat32ToFloat64Native);
  end;

 // Block Inplace ADDition Binding (64 bit)
 with TFunctionBinding.Create(@@BlockConvertFloat64ToFloat32,
   @BlockConvertFloat64ToFloat32Native) do
  begin
   Add(@BlockConvertFloat64ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt16LSBToFloat32,
   @BlockConvertInt16LSBToFloat32Native) do
  begin
   Add(@BlockConvertInt16LSBToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt24LSBToFloat32,
   @BlockConvertInt24LSBToFloat32Native) do
  begin
   Add(@BlockConvertInt24LSBToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSBToFloat32,
   @BlockConvertInt32LSBToFloat32Native) do
  begin
   Add(@BlockConvertInt32LSBToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB16ToFloat32,
   @BlockConvertInt32LSB16ToFloat32Native) do
  begin
   Add(@BlockConvertInt32LSB16ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB18ToFloat32,
   @BlockConvertInt32LSB18ToFloat32Native) do
  begin
   Add(@BlockConvertInt32LSB18ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB20ToFloat32,
   @BlockConvertInt32LSB20ToFloat32Native) do
  begin
   Add(@BlockConvertInt32LSB20ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB24ToFloat32,
   @BlockConvertInt32LSB24ToFloat32Native) do
  begin
   Add(@BlockConvertInt32LSB24ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt16MSBToFloat32,
   @BlockConvertInt16MSBToFloat32Native) do
  begin
   Add(@BlockConvertInt16MSBToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt24MSBToFloat32,
   @BlockConvertInt24MSBToFloat32Native) do
  begin
   Add(@BlockConvertInt24MSBToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSBToFloat32,
   @BlockConvertInt32MSBToFloat32Native) do
  begin
   Add(@BlockConvertInt32MSBToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB16ToFloat32,
   @BlockConvertInt32MSB16ToFloat32Native) do
  begin
   Add(@BlockConvertInt32MSB16ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB18ToFloat32,
   @BlockConvertInt32MSB18ToFloat32Native) do
  begin
   Add(@BlockConvertInt32MSB18ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB20ToFloat32,
   @BlockConvertInt32MSB20ToFloat32Native) do
  begin
   Add(@BlockConvertInt32MSB20ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB24ToFloat32,
   @BlockConvertInt32MSB24ToFloat32Native) do
  begin
   Add(@BlockConvertInt32MSB24ToFloat32Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt16LSBToFloat64,
   @BlockConvertInt16LSBToFloat64Native) do
  begin
   Add(@BlockConvertInt16LSBToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt24LSBToFloat64,
   @BlockConvertInt24LSBToFloat64Native) do
  begin
   Add(@BlockConvertInt24LSBToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSBToFloat64,
   @BlockConvertInt32LSBToFloat64Native) do
  begin
   Add(@BlockConvertInt32LSBToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB16ToFloat64,
   @BlockConvertInt32LSB16ToFloat64Native) do
  begin
   Add(@BlockConvertInt32LSB16ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB18ToFloat64,
   @BlockConvertInt32LSB18ToFloat64Native) do
  begin
   Add(@BlockConvertInt32LSB18ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB20ToFloat64,
   @BlockConvertInt32LSB20ToFloat64Native) do
  begin
   Add(@BlockConvertInt32LSB20ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32LSB24ToFloat64,
   @BlockConvertInt32LSB24ToFloat64Native) do
  begin
   Add(@BlockConvertInt32LSB24ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt16MSBToFloat64,
   @BlockConvertInt16MSBToFloat64Native) do
  begin
   Add(@BlockConvertInt16MSBToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt24MSBToFloat64,
   @BlockConvertInt24MSBToFloat64Native) do
  begin
   Add(@BlockConvertInt24MSBToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSBToFloat64,
   @BlockConvertInt32MSBToFloat64Native) do
  begin
   Add(@BlockConvertInt32MSBToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB16ToFloat64,
   @BlockConvertInt32MSB16ToFloat64Native) do
  begin
   Add(@BlockConvertInt32MSB16ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB18ToFloat64,
   @BlockConvertInt32MSB18ToFloat64Native) do
  begin
   Add(@BlockConvertInt32MSB18ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB20ToFloat64,
   @BlockConvertInt32MSB20ToFloat64Native) do
  begin
   Add(@BlockConvertInt32MSB20ToFloat64Native);
  end;

 with TFunctionBinding.Create(@@BlockConvertInt32MSB24ToFloat64,
   @BlockConvertInt32MSB24ToFloat64Native) do
  begin
   Add(@BlockConvertInt32MSB24ToFloat64Native);
  end;
end;

initialization
  BindFunctions;

end.
