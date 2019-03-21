{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_Common;

interface

{$I DAV_Compiler.inc}

{$IFDEF FPC}
uses LCLIntf, DAV_Types; {$DEFINE PUREPASCAL}
{$ELSE}
uses
  {$IFDEF MSWINDOWS} Windows, {$ENDIF}
  {$IFDEF UseNativeTypes}Types, {$ENDIF}
  DAV_Types;
{$ENDIF}

{ Byte Ordering }

function Swap16(Value: SmallInt): SmallInt;
function Swap32(Value: LongInt): LongInt;
function Swap64(Value: Int64): Int64;
function Swap80(Value: Extended): Extended;

procedure Flip16(var Value);
procedure Flip32(var Value);
procedure Flip64(var Value);
procedure Flip80(var Value);

function BitCount(Value: Integer): Integer; overload;
function BitCount(Value: Int64): Int64; overload;

procedure Exchange8(var ValueA, ValueB);
procedure Exchange16(var ValueA, ValueB);
procedure Exchange32(var ValueA, ValueB);


{ Limit & Clip, Min & Max }

function Limit(const Value: Single; Lower: Single = -1; Upper: Single = 1): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Limit(const Value: Double; Lower: Double = -1; Upper: Double = 1): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Limit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function RoundLimit(const Value: Single; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function RoundLimit(const Value: Double; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function IntLimit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClip(const Value, Lower, Upper: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClip(const Value, Lower, Upper: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipLower(Value: Single; const Lower: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipLower(Value: Double; const Lower: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipUpper(Value: Single; const Upper: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipUpper(Value: Double; const Upper: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipPositive(Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipPositive(Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipNegative(Value: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function BranchlessClipNegative(Value: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
procedure WrapInt(var Value: Integer; Upper: Integer; Lower: Integer = 0);
function Smallest(const A, B: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Smallest(const A, B: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Largest(const A, B: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function Largest(const A, B: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function LimitAngle(const Angle: Single): Single; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function LimitAngle(const Angle: Double): Double; overload; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

function FastFractional(const Value: Single): Single; overload;
function FastFractional(const Value: Double): Double; overload;
procedure FastAbs(var Value: Single); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure FastAbs(var Value: Double); {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF} overload;
procedure FastAbs(var Value: TDAV4SingleArray); overload;
procedure FastNegative(var Value: Single); overload;
function FastSgn(const Value: Single): Integer;
function FastMin(const A, B: Single) : Single;
function FastMax(const A, B: Single) : Single;
function FastMod(const Arg1, Arg2: Single): Single; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}

{$IFNDEF FPC}
function LaurentRoundInt(const Value: Single): Integer; overload;
function LaurentRoundInt(const Value: Double): Integer; overload;
procedure LaurentRoundInt(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
function LaurentFastFloor(const Value: Single): Integer; overload;
function LaurentFastFloor(const Value: Double): Integer; overload;
procedure LaurentFastFloor(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
function LaurentFastCeil(const Value: Single): Integer; overload;
function LaurentFastCeil(const Value: Double): Integer; overload;
procedure LaurentFastCeil(Input: PSingle; Output:PInteger; SampleFrames: Integer); overload;
function LaurentFastTrunc(const Value: Single): Integer; overload;
function LaurentFastTrunc(const Value: Double): Integer; overload;
function FastRound(Sample: Single): Integer; overload;
function FastRound(Sample: Double): Integer; overload;
{$ENDIF}

function GermaniumDiode(Voltage: Double): Double;
function SiliconDiode(Voltage: Double): Double;

function OnOff(const Value: Single): Boolean;
function unDenormalize(const Value: Single): Single;
procedure DontRaiseExceptionsAndSetFPUcodeword;

{ String Stuff & Messages }

{$IFNDEF DELPHI12_UP}
type
  TSysCharSet = set of AnsiChar;

function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
{$ENDIF}

function Compare4(S1, S2 : PAnsiChar): Boolean; {$IFDEF SUPPORTS_INLINE} inline; {$ENDIF}
function SplitString(S: String; Delimiter: AnsiChar): TStrArray;
function MakeGoodFileName(s: string): string;

{$IFDEF MSWINDOWS}
{$IFDEF CPU386}
function MethodToProcedure(Self: TObject; MethodAddr: Pointer): Pointer; overload;
function MethodToProcedure(Method: TMethod): Pointer; overload;
{$ENDIF}
{$ENDIF}

implementation

uses
  Math, SysUtils, DAV_Consts;

{ Byte Ordering }

type
  T16Bit = record
    case Integer of
      0 :  (v: SmallInt);
      1 :  (b: array[0..1] of Byte);
  end;

  T32Bit = record
    case Integer of
      0 :  (v: LongInt);
      1 :  (b: array[0..3] of Byte);
  end;

  T64Bit = record
    case Integer of
      0 :  (v: Int64);
      1 :  (b: array[0..7] of Byte);
  end;

  T80Bit = record
    case Integer of
      0 :  (v: Extended);
      1 :  (b: array[0..9] of Byte);
  end;

function Swap16(Value: SmallInt): SmallInt;
var
  t: Byte;
begin
  with T16Bit(Value) do
  begin
    t := b[0];
    b[0] := b[1];
    b[1] := t;
    Result := v;
  end;
end;

function Swap32(Value: LongInt): LongInt;
var
  Temp: Byte;
begin
  with T32Bit(Value) do
  begin
    Temp := b[0];
    b[0] := b[3];
    b[3] := Temp;
    Temp := b[1];
    b[1] := b[2];
    b[2] := Temp;
    Result := v;
  end;
end;

function Swap64(Value: Int64): Int64;
var
  Temp: Byte;
begin
  with T64Bit(Value) do
  begin
    Temp := b[0];
    b[0] := b[7];
    b[7] := Temp;
    Temp := b[1];
    b[1] := b[6];
    b[6] := Temp;
    Temp := b[2];
    b[2] := b[5];
    b[5] := Temp;
    Temp := b[3];
    b[3] := b[4];
    b[4] := Temp;
    Result := v;
  end;
end;

function Swap80(Value: Extended): Extended;
var
  Temp: Byte;
  T80B: T80Bit absolute Value;
begin
  with T80B do
  begin
    Temp := b[0];
    b[0] := b[9];
    b[9] := Temp;
    Temp := b[1];
    b[1] := b[8];
    b[8] := Temp;
    Temp := b[2];
    b[2] := b[7];
    b[7] := Temp;
    Temp := b[3];
    b[3] := b[6];
    b[6] := Temp;
    Temp := b[4];
    b[4] := b[5];
    b[5] := Temp;
    Result := V;
  end;
end;

procedure Flip16(var Value);
var
  t: Byte;
begin
  with T16Bit(Value) do
  begin
    t := b[0];
    b[0] := b[1];
    b[1] := t;
  end;
end;

procedure Flip32(var Value);
var
  Temp: Byte;
begin
  with T32Bit(Value) do
  begin
    Temp := b[0];
    b[0] := b[3];
    b[3] := Temp;
    Temp := b[1];
    b[1] := b[2];
    b[2] := Temp;
  end;
end;

procedure Flip64(var Value);
var
  Temp: Byte;
begin
  with T64Bit(Value) do
  begin
    Temp := b[0];
    b[0] := b[7];
    b[7] := Temp;
    Temp := b[1];
    b[1] := b[6];
    b[6] := Temp;
    Temp := b[2];
    b[2] := b[5];
    b[5] := Temp;
    Temp := b[3];
    b[3] := b[4];
    b[4] := Temp;
  end;
end;

procedure Flip80(var Value);
var
  Temp: Byte;
  T80B: T80Bit absolute Value;
begin
  with T80B do
  begin
    Temp := b[0];
    b[0] := b[9];
    b[9] := Temp;
    Temp := b[1];
    b[1] := b[8];
    b[8] := Temp;
    Temp := b[2];
    b[2] := b[7];
    b[7] := Temp;
    Temp := b[3];
    b[3] := b[6];
    b[6] := Temp;
    Temp := b[4];
    b[4] := b[5];
    b[5] := Temp;
  end;
end;

function BitCount(Value: Integer): Integer;
{$IFDEF PUREPASCAL}
begin
  Value := Value - (Value shr 1) and $55555555;
  Value := (Value and $33333333) + ((Value shr 2) and $33333333);
  Value := (Value + (Value shr 4)) and $0F0F0F0F;
  Result := (Value * $01010101) shr 24;
{$ELSE}
asm
    MOV     EAX, Value
    MOV     EDX, EAX
    SHR     EAX, 1
    AND     EAX, $55555555
    SUB     EDX, EAX
    MOV     EAX, EDX
    SHR     EDX, 2
    AND     EAX, $33333333
    AND     EDX, $33333333
    ADD     EAX, EDX
    MOV     EDX, EAX
    SHR     EAX, 4
    ADD     EAX, EDX
    AND     EAX, $0F0F0F0F
    IMUL    EAX, $01010101
    SHR     EAX, 24
    MOV     Result, EAX
{$ENDIF}
end;

function BitCount(Value: Int64): Int64;
begin
  Value := Value - (Value shr 1) and $5555555555555555;
  Value := (Value and $3333333333333333) + ((Value shr 2) and $3333333333333333);
  Value := (Value + (Value shr 4)) and $0F0F0F0F0F0F0F0F;
  Result := (Value * $0101010101010101) shr 56;
end;

/////////////////////////
//                     //
// Exchange two values //
//                     //
/////////////////////////

procedure Exchange8(var ValueA, ValueB);
var
  Temp : Byte;
begin
  Temp := Byte(ValueA);
  Byte(ValueA) := Byte(ValueB);
  Byte(ValueB) := Temp;
end;

procedure Exchange16(var ValueA, ValueB);
var
  Temp : Word;
begin
  Temp := Word(ValueA);
  Word(ValueA) := Word(ValueB);
  Word(ValueB) := Temp;
end;

procedure Exchange32(var ValueA, ValueB);
var
  Temp : Integer;
begin
  Temp := Integer(ValueA);
  Integer(ValueA) := Integer(ValueB);
  Integer(ValueB) := Temp;
end;


{ Limit & Clip, Min & Max }

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Single; Lower: Single = -1; Upper: Single = 1): Single;
begin
  if Value < Lower then
    Result := Lower
  else
  if Value > Upper then
    Result := Upper
  else
    Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Double; Lower: Double = -1; Upper: Double = 1): Double;
begin
  if Value < Lower then
    Result := Lower
  else
  if Value > Upper then
    Result := Upper
  else
    Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function Limit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
  if Value < Lower then
    Result := Lower
  else
  if Value > Upper then
    Result := Upper
  else
    Result := Value;
end;

// Limit a Value to be Lower <= Value <= Upper
function RoundLimit(const Value: Single; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
  Result := Round(Value);
  if Result < Lower then
    Result := Lower
  else
  if Result > Upper then
    Result := Upper;
end;

// Limit a Value to be Lower <= Value <= Upper
function RoundLimit(const Value: Double; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
  Result := Round(Value);
  if Result < Lower then
    Result := Lower
  else
  if Result > Upper then
    Result := Upper;
end;

// Limit a Value to be Lower <= Value <= Upper
function IntLimit(const Value: Integer; Lower: Integer = 0; Upper: Integer = 1): Integer;
begin
  if Value < Lower then
    Result := Lower
  else
  if Value > Upper then
    Result := Upper
  else
    Result := Value;
end;

function BranchlessClip(const Value, Lower, Upper: Single): Single;
begin
  Result := (Abs(Value - Lower) + (Lower + Upper) - Abs(Value - Upper)) * 0.5;
end;

function BranchlessClip(const Value, Lower, Upper: Double): Double;
begin
  Result := (Abs(Value - Lower) + (Lower + Upper) - Abs(Value - Upper)) * 0.5;
end;

function BranchlessClipLower(Value: Single; const Lower: Single): Single;
begin
  Value := Value - Lower;
  Result := (Value + Abs(Value)) * 0.5 + Lower;
end;

function BranchlessClipLower(Value: Double; const Lower: Double): Double;
begin
  Value := Value - Lower;
  Result := (Value + Abs(Value)) * 0.5 + Lower;
end;

function BranchlessClipUpper(Value: Single; const Upper: Single): Single;
begin
  Value := Upper - Value;
  Result := Upper -(Value + Abs(Value)) * 0.5;
end;

function BranchlessClipUpper(Value: Double; const Upper: Double): Double;
begin
  Value := Upper - Value;
  Result := Upper -(Value + Abs(Value)) * 0.5;
end;

function BranchlessClipPositive(Value: Single): Single;
begin
  Result := (Value + Abs(Value)) * 0.5;
end;

function BranchlessClipPositive(Value: Double): Double;
begin
  Result := (Value + Abs(Value)) * 0.5;
end;

function BranchlessClipNegative(Value: Single): Single;
begin
  Result := (Abs(Value) - Value) * 0.5;
end;

function BranchlessClipNegative(Value: Double): Double;
begin
  Result := (Abs(Value) - Value) * 0.5;
end;

procedure WrapInt(var Value: Integer; Upper: Integer; Lower: Integer = 0);
begin
  while Value >= Upper do
    Value := Value - Upper;
  while Value <  Lower do
    Value := Value + Upper;
end;

function Smallest(const A, B: Single): Single;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Smallest(const A, B: Double): Double;
begin
  if A < B then
    Result := A
  else
    Result := B;
end;

function Largest(const A, B: Single): Single;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function Largest(const A, B: Double): Double;
begin
  if A > B then
    Result := A
  else
    Result := B;
end;

function LimitAngle(const Angle: Single): Single;
begin
 Result := Angle;
 while Result <    0 do
   Result := Result + 360;
 while Result >= 360 do
   Result := Result - 360;
end;

function LimitAngle(const Angle: Double): Double;
begin
 Result := Angle;
 while Result <    0 do
   Result := Result + 360;
 while Result >= 360 do
   Result := Result - 360;
end;


{ Math }

function FastFractional(const Value: Single): Single;
{$IFDEF PUREPASCAL}
begin
  Result := Value - Round(Value - 0.5);
{$ELSE}
asm
    FLD     Value.Single
    FLD     Value.Single
    FSUB    CHalf64
    FRNDINT
    FSUBP
{$ENDIF}
end;

function FastFractional(const Value: Double): Double;
{$IFDEF PUREPASCAL}
begin
  Result := Value - Round(Value - 0.5);
{$ELSE}
asm
    FLD     Value.Double
    FLD     Value.Double
    FSUB    CHalf64
    FRNDINT
    FSUBP
{$ENDIF}
end;

procedure FastAbs(var Value: Single);
var
  i : Integer absolute Value;
begin
  i := i and $7FFFFFFF;
end;

procedure FastAbs(var Value: Double);
var
  i : array [0..1] of Integer absolute Value;
begin
  i[0] := i[0] and $7FFFFFFF;
end;

procedure FastAbs(var Value: TDAV4SingleArray); overload;
{$IFDEF PUREPASCAL}
var
  i : array [0..3] of Integer absolute Value;
begin
  i[0] := i[0] and $7FFFFFFF;
  i[1] := i[1] and $7FFFFFFF;
  i[2] := i[2] and $7FFFFFFF;
  i[3] := i[3] and $7FFFFFFF;
{$ELSE}
asm
    FLD     [EAX].Single
    FABS
    FSTP    [EAX].Single
    FLD     [EAX +  4].Single
    FABS
    FSTP    [EAX +  4].Single
    FLD     [EAX +  8].Single
    FABS
    FSTP    [EAX +  8].Single
    FLD     [EAX + 12].Single
    FABS
    FSTP    [EAX + 12].Single
{$ENDIF}
end;

procedure FastNegative(var Value: Single);
var
  i : Integer absolute Value;
const
  CBitMask = $80000000;
begin
  i := Cardinal((@Value)^) xor CBitMask;
end;

function FastMod(const Arg1, Arg2: Single): Single;
var
  Norm : Single;
begin
  Norm := Arg1 / Arg2;
  Result := (Norm - Round(Norm - 0.5)) * Arg2
end;


// LaurentRoundInt

function LaurentRoundInt(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Value);
{$ELSE}
asm
    FLD     Value.Single
    FADD    ST(0), ST(0)
    FADD    CHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
{$ENDIF}
end;

function LaurentRoundInt(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Value);
{$ELSE}
asm
    FLD     Value.Double
    FADD    ST(0), ST(0)
    FADD    CHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
{$ENDIF}
end;

procedure LaurentRoundInt(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
  for i := 0 to SampleFrames - 1 do
  begin
    Output^ := Round(Input^);
    Inc(Output);
    Inc(Input);
  end;
{$ELSE}
asm
@Start:
    FLD     [EAX].Single
    FADD    ST(0), ST(0)
    FADD    CHalf32
    FISTP   [EDX].Integer
    SAR     [EDX].Integer, 1
    ADD     EAX,4
    ADD     EDX,4
    LOOP    @Start
{$ENDIF}
end;

function LaurentFastFloor(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Floor(Value);
{$ELSE}
asm
    FLD     Value.Single
    FADD    ST(0), ST(0)
    FSUB    CHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
{$ENDIF}
end;

function LaurentFastFloor(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Floor(Value);
{$ELSE}
asm
    FLD     Value.Double
    FADD    ST(0), ST(0)
    FSUB    CHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
{$ENDIF}
end;

procedure LaurentFastFloor(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
  for i := 0 to SampleFrames - 1 do
  begin
    Output^ := Floor(Input^);
    Inc(Output);
    Inc(Input);
  end;
{$ELSE}
asm
@Start:
    FLD     [EAX].Single
    FADD    ST(0), ST(0)
    FSUB    CHalf32
    FISTP   [EDX].Integer
    SAR     [EDX].Integer, 1
    ADD     EAX, 4
    ADD     EDX, 4
    LOOP    @Start
{$ENDIF}
end;

// LaurentFastCeil

function LaurentFastCeil(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Ceil(Value);
{$ELSE}
asm
    FLD     Value.Single
    FADD    ST(0), ST(0)
    FSUBR   CMinusHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
    NEG     Result.Integer
{$ENDIF}
end;

function LaurentFastCeil(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Ceil(Value);
{$ELSE}
asm
    FLD     Value.Double
    FADD    ST(0), ST(0)
    FSUBR   CMinusHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
    NEG     Result.Integer
{$ENDIF}
end;

procedure LaurentFastCeil(Input: PSingle; Output: PInteger; SampleFrames: Integer); overload;
{$IFDEF PUREPASCAL}
var
  i : Integer;
begin
  for i := 0 to SampleFrames - 1 do
  begin
    Output^ := Ceil(Input^);
    Inc(Output);
    Inc(Input);
  end;
{$ELSE}
asm
@Start:
    FLD     [EAX].Single
    FADD    ST(0), ST(0)
    FSUBR   CMinusHalf32
    FISTP   [EDX].Integer
    SAR     [EDX].Integer, 1
    NEG     [EDX].Integer
    ADD     EAX, 4
    ADD     EDX, 4
    LOOP    @Start
{$ENDIF}
end;

// Laurent Fast Trunc

function LaurentFastTrunc(const Value: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Trunc(Value);
{$ELSE}
var
  IntCast : Integer absolute Value;
asm
    FLD     Value.Single
    FADD    ST(0), ST(0)
    FABS
    FADD    CMinusHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
    TEST    IntCast, $80000000
    JZ      @Done
    NEG     Result.Integer
@Done:
{$ENDIF}
end;

function LaurentFastTrunc(const Value: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Trunc(Value);
{$ELSE}
var
  ByteCast : array [0..7] of Byte absolute Value;
asm
    FLD     Value.Double
    FADD    ST(0), ST(0)
    FABS
    FADD    CMinusHalf32
    FISTP   Result.Integer
    SAR     Result.Integer, 1
    TEST    ByteCast[4].Integer, $80000000
    JZ      @Done
    NEG     Result.Integer
@Done:
{$ENDIF}
end;


function FastRound(Sample: Single): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Sample);
{$ELSE}
asm
    FLD     Sample.Single
    FRNDINT
    FISTP   Result.Integer
{$ENDIF}
end;

function FastRound(Sample: Double): Integer; overload;
{$IFDEF PUREPASCAL}
begin
  Result := Round(Sample);
{$ELSE}
asm
    FLD     Sample.Double
    FRNDINT
    FISTP   Result.Integer
{$ENDIF}
end;

function FastSgn(const Value: Single): Integer;
var
  IntCast : Integer absolute Value;
begin
  Result := 1 - ((Intcast shr 31) shl 1);
end;

function GermaniumDiode(Voltage: Double): Double;
begin
  Result := 0.085 * (Voltage + Abs(Voltage)) * sqr(Voltage) * Voltage
end;

function SiliconDiode(Voltage: Double): Double;
begin
  Result := 40.6728602E-9 * (Exp(17.7493332 * (Voltage + 0.3)) - 1);
end;

function OnOff(const Value: Single): Boolean;
begin
  Result := Value > 0.5
end;

function UnDenormalize(const Value : Single) : Single;
var
  IntValue : Integer absolute Value;
begin
  if (IntValue and $7F8000) = 0 then
    Result := 0.0
  else
    Result := Value;
end;

procedure DontRaiseExceptionsAndSetFPUcodeword;
{$IFDEF PUREPASCAL}
begin
  SetFPUExceptionMask(exAllArithmeticExceptions);
  SetSSEExceptionMask(exAllArithmeticExceptions);
{$ELSE}
{$IFDEF FPC}
var
  FpuCodeword : Word;
asm
    MOV     FpuCodeword, $133F
    FNCLEX                     // Don't raise pending exceptions enabled by the new flags
    FLDCW   FpuCodeword        // round FPU codeword, with exceptions disabled
{$ELSE}
const
  SCRound8087CW     : Word = $133F; // round FPU codeword, with exceptions disabled
  SCChop8087CW      : Word = $1F3F; // Trunc (chop) FPU codeword, with exceptions disabled
  SCRoundDown8087CW : Word = $173F; // exceptions disabled
  SCRoundUp8087CW   : Word = $1B3F; // exceptions disabled
asm
    FNCLEX                  // Don't raise pending exceptions enabled by the new flags
    FLDCW   SCRound8087CW   // SCRound8087CW: Word = $133F; round FPU codeword, with exceptions disabled

{$ENDIF}
{$ENDIF}
end;

function GetMXCSR: Cardinal;
asm
{$IFDEF CPU_32}
    PUSH    0
    STMXCSR [ESP].DWord
    POP     EAX
{$ENDIF}
{$IFDEF CPUx86_64}
    PUSH    0
    STMXCSR [RSP].DWord
    POP     RAX
{$ENDIF}
end;

procedure SetMXCSR(Value: Cardinal);
asm
{$IFDEF CPU386}
    MOV      [ESP - 4], EAX
    LDMXCSR  [ESP - 4]
{$ENDIF}

{$IFDEF CPUx86_64}
    MOV      [RSP - 4], ECX
    LDMXCSR  [RSP - 4]
{$ENDIF}
end;

(*
procedure SetMxcsrOn(Bit: Byte);
var
  State : PInt64;
  X     : Integer;
begin
  GetMem(State, 512); // needs to be aligned!
  try
    asm
      FXSAVE   State
      MOV      EDX, State
      MOV      EAX, [EDX + 24]
      MOV      EDX, 1
      SHL      EDX, Bit
      OR       EAX, EDX
      MOV      X, EAX
      LDMXCSR  X
    end;
  finally
    FreeMem(State);
  end;
end;

procedure SetMxcsrOff(Bit: Byte);
var
  State : PInt64;
  X     : Integer;
begin
  GetMem(State, 512); // needs to be aligned!
  try
    asm
      FXSAVE   State
      MOV      EDX, State
      MOV      EAX, [EDX + 24]
      MOV      EDX, 1
      SHL      EDX, Bit
      OR       EAX, EDX
      MOV      X, EAX
      LDMXCSR  X
    end;
  finally
    FreeMem(State);
  end;
end;
*)


{ String Functions }

{$IFNDEF DELPHI12_UP}
function CharInSet(C: AnsiChar; const CharSet: TSysCharSet): Boolean;
begin
  Result := C in CharSet;
end;
{$ENDIF}

{$WARNINGS ON}

function Compare4(S1, S2: PAnsiChar): Boolean;
var
  i, Diff : Byte;
begin
  Result := True;
  for i := 0 to 3 do
  begin
    Diff := Byte(S1[i]) - Byte(S2[i]);
    if not (Diff in [0, 32, 224]) then
      Exit(False);
  end;
end;

function SplitString(S: String; Delimiter: AnsiChar): TStrArray;
var
  C : Integer;
begin
  repeat
    SetLength(Result, Length(Result) + 1);
    {$IFDEF DELPHI2009_UP}
    C := AnsiPos(string(Delimiter), S);
    {$ELSE}
    C := Pos(Delimiter, S);
    {$ENDIF}
    if C = 0 then C := Length(S) + 1;
    Result[Length(Result)- 1] := Copy(S, 1, C- 1);
    Delete(S, 1, C);
  until Length(S)= 0;
end;

function MakeGoodFileName(s: string): string;
var
  i: Integer;
begin
  Result := '';
  for i := 1 to Length(s) do
    {$IFDEF DELPHI2009_UP}
    if CharInSet(s[i], ['*', '\', '/', '[', ']', '"', '|', '<', '>', '?', ':']) then
    {$ELSE}
    if not (s[i] in ['*', '\', '/', '[', ']', '"', '|', '<', '>', '?', ':']) then
    {$ENDIF}
      Result := Result + s[i]
    else
      Result := Result + '-';
end;

{$DEFINE PUREPASCAL}

function FastMin(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
  if A > B then
    Result := B
  else
    Result := A
{$ELSE}
asm
    FLD     DWORD PTR [EBP + $08]
    FLD     DWORD PTR [EBP + $0C]
    FCOMI   ST(0), ST(1)
    FCMOVNB ST(0), ST(1)
    FFREE   ST(1)
{$ENDIF}
end;

function FastMax(const A, B: Single) : Single;
{$IFDEF PUREPASCAL}
begin
  if A < B then
    Result := B
  else
    Result := A
end;
{$ELSE}
asm
    FLD     DWORD PTR [EBP + $0C]
    FLD     DWORD PTR [EBP + $08]
    FCOMI   ST(0), ST(1)
    FCMOVNB ST(0), ST(1)
    FFREE   ST(1)
end;
{$ENDIF}


{ Object oriented code conversions }

{$IFDEF MSWINDOWS}
{$IFDEF CPU386}
function MethodToProcedure(Self: TObject; MethodAddr: Pointer): Pointer;
type
  TMethodToProcedure = packed record
    PopEax    : Byte;
    PushSelf  : record
      Opcode  : Byte;
      Self    : Pointer;
    end;
    PushEax   : Byte;
    Jump      : record
      Opcode  : Byte;
      ModRm   : Byte;
      PTarget : ^Pointer;
      Target  : Pointer;
    end;
  end;
var
  MTP : ^TMethodToProcedure absolute Result;
begin
  MTP := VirtualAlloc(nil, SizeOf(MTP^), MEM_COMMIT, PAGE_EXECUTE_READWRITE);
  with MTP^ do
  begin
    PopEax          := $58;
    PushSelf.Opcode := $68;
    PushSelf.Self   := Self;
    PushEax         := $50;
    Jump.Opcode     := $FF;
    Jump.ModRm      := $25;
    Jump.PTarget    := @Jump.Target;
    Jump.Target     := MethodAddr;
  end;
end;

function MethodToProcedure(Method: TMethod): Pointer;
begin
  Result := MethodToProcedure(TObject(Method.data), Method.code);
end;
{$ENDIF}
{$ENDIF}

end.
