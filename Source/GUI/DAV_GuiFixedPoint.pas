unit DAV_GuiFixedPoint;

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

{$I ..\DAV_Compiler.inc}

{ A fixed-point type }

type
  // Fixed point with 16 integer bits and 16 fractional bits. This type has
  // data bits arrangement compatible with Windows.TFixed
  PFixed16Dot16Point = ^TFixed16Dot16Point;
  TFixed16Dot16Point = packed record
  case Integer of
    0: (Fixed: Integer);
    1: (Frac: Word; Int: SmallInt);
  end;

  PFixed16Dot16PointArray = ^TFixed16Dot16PointArray;
  TFixed16Dot16PointArray = array [0..0] of TFixed16Dot16Point;
  PArrayOfFixed16Dot16Point = ^TArrayOfFixed16Dot16Point;
  TArrayOfFixed16Dot16Point = array of TFixed16Dot16Point;
  PArrayOfArrayOfFixed16Dot16Point = ^TArrayOfArrayOfFixed16Dot16Point;
  TArrayOfArrayOfFixed16Dot16Point = array of TArrayOfFixed16Dot16Point;

  PFixed24Dot8Point = ^TFixed16Dot16Point;
  TFixed24Dot8Point = packed record
  case Integer of
    0: (Fixed: Integer);
    1: (Frac: Byte; Low: Byte; High: SmallInt);
  end;

  PFixed24Dot8PointArray = ^TFixed24Dot8PointArray;
  TFixed24Dot8PointArray = array [0..0] of TFixed24Dot8Point;
  PArrayOfFixed24Dot8Point = ^TArrayOfFixed24Dot8Point;
  TArrayOfFixed24Dot8Point = array of TFixed24Dot8Point;
  PArrayOfArrayOfFixed24Dot8Point = ^TArrayOfArrayOfFixed24Dot8Point;
  TArrayOfArrayOfFixed24Dot8Point = array of TArrayOfFixed24Dot8Point;

function ConvertToFixed16Dot16Point(Value: Single): TFixed16Dot16Point; overload;
function ConvertToFixed16Dot16Point(Value: Integer): TFixed16Dot16Point; overload;
function ConvertToFixed24Dot8Point(Value: Single): TFixed24Dot8Point; overload;
function ConvertToFixed24Dot8Point(Value: Integer): TFixed24Dot8Point; overload;
function ConvertFromFixed16Dot16Point(Value: TFixed16Dot16Point): Single; overload;
function ConvertFromFixed24Dot8Point(Value: TFixed24Dot8Point): Single; overload;
function ConvertFromFixed16Dot16PointToInteger(Value: TFixed16Dot16Point): Integer; overload;
function ConvertFromFixed24Dot8PointToInteger(Value: TFixed24Dot8Point): Integer; overload;
function FixedFloor(Value: TFixed16Dot16Point): Integer; overload;
function FixedFloor(Value: TFixed24Dot8Point): Integer; overload;
function FixedCeil(Value: TFixed16Dot16Point): Integer; overload;
function FixedCeil(Value: TFixed24Dot8Point): Integer; overload;
function FixedRound(Value: TFixed16Dot16Point): Integer; overload;
function FixedRound(Value: TFixed24Dot8Point): Integer; overload;
function FixedAdd(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedAdd(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedSub(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedSub(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedMul(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedMul(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedMul(A: TFixed16Dot16Point; B: Integer): TFixed16Dot16Point; overload;
function FixedMul(A: TFixed24Dot8Point; B: Integer): TFixed24Dot8Point; overload;
function Fixed16Dot16Mul(A, B: Integer): Integer; overload;
function Fixed24Dot8Mul(A, B: Integer): Integer; overload;
function FixedDiv(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedDiv(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedDiv(A: TFixed16Dot16Point; B: Integer): TFixed16Dot16Point; overload;
function FixedDiv(A: TFixed24Dot8Point; B: Integer): TFixed24Dot8Point; overload;
function FixedReciprocal(Value: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedReciprocal(Value: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedSqr(Value: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedSqr(Value: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedSqrtLowResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedSqrtHighResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedSqrt(Value: TFixed24Dot8Point): TFixed24Dot8Point; overload;

const
  CFixed16Dot16Two : TFixed16Dot16Point = (Fixed : $20000);
  CFixed16Dot16One : TFixed16Dot16Point = (Fixed : $10000);
  CFixed16Dot16Half : TFixed16Dot16Point = (Fixed : $8000);
  CFixed16Dot16ToFloat = 1 / $10000;
  CFixed16Dot16PI : TFixed16Dot16Point = (Fixed : Round(PI * $10000));

  CFixed24Dot8Two : TFixed24Dot8Point = (Fixed : $200);
  CFixed24Dot8One : TFixed24Dot8Point = (Fixed : $100);
  CFixed24Dot8Half : TFixed24Dot8Point = (Fixed : $80);
  CFixed24Dot8ToFloat = 1 / $100;
  CFixed24Dot8PI : TFixed24Dot8Point = (Fixed : Round(PI * $100));

implementation

function ConvertToFixed16Dot16Point(Value: Single): TFixed16Dot16Point;
begin
 Result.Fixed := Round(Value * CFixed16Dot16One.Fixed);
end;

function ConvertToFixed16Dot16Point(Value: Integer): TFixed16Dot16Point;
begin
 Result.Fixed := Value shl 16;
end;

function ConvertToFixed24Dot8Point(Value: Single): TFixed24Dot8Point;
begin
 Result.Fixed := Round(Value * CFixed24Dot8One.Fixed);
end;

function ConvertToFixed24Dot8Point(Value: Integer): TFixed24Dot8Point;
begin
 Result.Fixed := Value shl 8;
end;

function ConvertFromFixed16Dot16Point(Value: TFixed16Dot16Point): Single;
begin
 Result := Value.Fixed * CFixed16Dot16ToFloat;
end;

function ConvertFromFixed24Dot8Point(Value: TFixed24Dot8Point): Single;
begin
 Result := Value.Fixed * CFixed24Dot8ToFloat;
end;

function ConvertFromFixed16Dot16PointToInteger(Value: TFixed16Dot16Point): Integer;
begin
 Result := Round(Value.Fixed * CFixed16Dot16ToFloat);
end;

function ConvertFromFixed24Dot8PointToInteger(Value: TFixed24Dot8Point): Integer;
begin
 Result := Round(Value.Fixed * CFixed24Dot8ToFloat);
end;

function FixedFloor(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or (Value.Fixed shr 16);
{$ELSE}
asm
  SAR     EAX, 16;
{$ENDIF}
end;

function FixedFloor(Value: TFixed24Dot8Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or (Value.Fixed shr 8);
{$ELSE}
asm
  SAR     EAX, 8;
{$ENDIF}
end;

function FixedCeil(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or ((Value.Fixed + $FFFF) shr 16);
{$ELSE}
asm
  ADD     EAX, $FFFF
  SAR     EAX, 16;
{$ENDIF}
end;

function FixedCeil(Value: TFixed24Dot8Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or ((Value.Fixed + $FF) shr 8);
{$ELSE}
asm
  ADD     EAX, $FF
  SAR     EAX, 8;
{$ENDIF}
end;

function FixedRound(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or (Value.Fixed + $7FFF) shr 16;
{$ELSE}
asm
  ADD     EAX, $7FFF
  SAR     EAX, 16
{$ENDIF}
end;

function FixedRound(Value: TFixed24Dot8Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or (Value.Fixed + $7F) shr 8;
{$ELSE}
asm
  ADD     EAX, $7F
  SAR     EAX, 8
{$ENDIF}
end;

function FixedAdd(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     EAX, EDX
{$ENDIF}
end;

function FixedAdd(A, B: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     EAX, EDX
{$ENDIF}
end;

function FixedSub(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     EAX, EDX
{$ENDIF}
end;

function FixedSub(A, B: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     EAX, EDX
{$ENDIF}
end;

function FixedMul(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed16Dot16ToFloat * B.Fixed);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 16
{$ENDIF}
end;

function FixedMul(A, B: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed24Dot8ToFloat * B.Fixed);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 8
{$ENDIF}
end;

function FixedMul(A: TFixed16Dot16Point; B: Integer): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed16Dot16ToFloat * B);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 16
{$ENDIF}
end;

function FixedMul(A: TFixed24Dot8Point; B: Integer): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed * CFixed24Dot8ToFloat * B);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 8
{$ENDIF}
end;

function Fixed16Dot16Mul(A, B: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A * CFixed16Dot16ToFloat * B);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 16
{$ENDIF}
end;

function Fixed24Dot8Mul(A, B: Integer): Integer;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A * CFixed24Dot8ToFloat * B);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 8
{$ENDIF}
end;

function FixedDiv(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed16Dot16One.Fixed);
{$ELSE}
asm
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 16
  SHL     EAX, 16
  IDIV    ECX
{$ENDIF}
end;

function FixedDiv(A, B: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B.Fixed * CFixed24Dot8One.Fixed);
{$ELSE}
asm
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 8
  SHL     EAX, 8
  IDIV    ECX
{$ENDIF}
end;

function FixedDiv(A: TFixed16Dot16Point; B: Integer): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B * CFixed16Dot16One.Fixed);
{$ELSE}
asm
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 16
  SHL     EAX, 16
  IDIV    ECX
{$ENDIF}
end;

function FixedDiv(A: TFixed24Dot8Point; B: Integer): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round(A.Fixed / B * CFixed24Dot8One.Fixed);
{$ELSE}
asm
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 8
  SHL     EAX, 8
  IDIV    ECX
{$ENDIF}
end;

function FixedReciprocal(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
const
  CDividend: Single = 4294967296; // CFixed16Dot16One * CFixed16Dot16One
begin
  IntResult := Round(CDividend / Value.Fixed - 0.5);
{$ELSE}
asm
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
{$ENDIF}
end;

function FixedReciprocal(Value: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
const
  CDividend: Single = 65536; // CFixed24Dot24One * CFixed24Dot24One
begin
  IntResult := Round(CDividend / Value.Fixed - 0.5);
{$ELSE}
asm
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
  SHR     EAX, 16
{$ENDIF}
end;

function FixedSqr(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round((Value.Fixed * CFixed16Dot16ToFloat) * Value.Fixed);
{$ELSE}
asm
  IMUL    EAX
  SHRD    EAX, EDX, 16
{$ENDIF}
end;

function FixedSqr(Value: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
begin
  IntResult := Round((Value.Fixed * CFixed24Dot8ToFloat) * Value.Fixed);
{$ELSE}
asm
  IMUL    EAX
  SHRD    EAX, EDX, 8
{$ENDIF}
end;

function FixedSqrtLowResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
const
  CFixed16Dot16OneAsSingle : Single = 65536;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed16Dot16OneAsSingle) - 0.5);
{$ELSE}
asm
  PUSH    EBX
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EBX, $40000000
@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JL      @Step2
  SUB     EDX, EAX
  JL      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JMP     @Step3
@Step2:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step1
@Step3:
  SHL     EAX, 8
  POP     EBX
{$ENDIF}
end;

function FixedSqrtHighResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
const
  CFixed16Dot16OneAsSingle : Single = 65536;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed16Dot16OneAsSingle));
{$ELSE}
asm
  PUSH    EBX
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EBX, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step1

@Step3:
  MOV     EBX, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, EBX
  jb      @Step5
  SUB     EDX, EAX
  jb      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step4

@Step6:
  POP     EBX
{$ENDIF}
end;

function FixedSqrt(Value: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
const
  CFixed24Dot8OneAsSingle : Single = 256;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed24Dot8OneAsSingle) - 0.5);
{$ELSE}
asm
  PUSH    EBX
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EBX, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step1

@Step3:
  MOV     EBX, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, EBX
  jb      @Step5
  SUB     EDX, EAX
  jb      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step4

@Step6:
  POP     EBX
  SHR     EAX, 4
{$ENDIF}
end;

end.
