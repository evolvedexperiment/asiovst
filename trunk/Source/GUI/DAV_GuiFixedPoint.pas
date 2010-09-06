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
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}

{ A fixed-point type }

type
  // This type has data bits arrangement compatible with Windows.TFixed
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

function Fixed16Dot16Point(Value: Single): TFixed16Dot16Point; overload;
function Fixed16Dot16Point(Value: Integer): TFixed16Dot16Point; overload;
function FixedFloor(Value: TFixed16Dot16Point): Integer;
function FixedCeil(Value: TFixed16Dot16Point): Integer;
function FixedRound(Value: TFixed16Dot16Point): Integer;
function FixedAdd(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedSub(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedMul(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedDiv(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedOneOver(Value: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedSqr(Value: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedSqrtLowResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point;
function FixedSqrtHighResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point;

const
  CFixedOne : TFixed16Dot16Point = (Fixed : $10000);
  CFixedHalf : TFixed16Dot16Point = (Fixed : $8000);
  CFixedToFloat = 1 / $10000;
  CFixedPI : TFixed16Dot16Point = (Fixed : Round(PI * $10000));

implementation

function Fixed16Dot16Point(Value: Single): TFixed16Dot16Point;
begin
 Result.Fixed := Round(Value * CFixedOne.Fixed);
end;

function Fixed16Dot16Point(Value: Integer): TFixed16Dot16Point;
begin
 Result.Fixed := Value shl 16;
end;

function FixedFloor(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := Value shr 16;
{$ELSE}
asm
  SAR     EAX, 16;
{$ENDIF}
end;

function FixedCeil(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value + $FFFF) shr 16;
{$ELSE}
asm
  ADD     EAX, $0000FFFF
  SAR     EAX, 16;
{$ENDIF}
end;

function FixedRound(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (A + $7FFF) shr 16;
{$ELSE}
asm
  ADD     EAX, $00007FFF
  SAR     EAX, 16
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

function FixedSub(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
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
begin
  Result := Round(A * CFixedToFloat * B);
{$ELSE}
asm
  IMUL    EDX
  SHRD    EAX, EDX, 16
{$ENDIF}
end;

function FixedDiv(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
begin
  Result := Round(A / B * FixedOne);
{$ELSE}
asm
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 16
  SHL     EAX, 16
  IDIV    ECX
{$ENDIF}
end;

function FixedOneOver(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
const
  CDividend: Single = 4294967296; // FixedOne * FixedOne
begin
  Result := Round(CDividend / Value);
{$ELSE}
asm
  MOV     ECX, EAX
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
{$ENDIF}
end;

function FixedSqr(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
begin
  Result := Round((Value * CFixedToFloat) * Value);
{$ELSE}
asm
  IMUL    EAX
  SHRD    EAX, EDX, 16
{$ENDIF}
end;


function FixedSqrtLowResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
const
  CFixedOneAsSingle : Single = 65536;
begin
  Result := Round(Sqrt(Value * CFixedOneAsSingle));
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
  MOV     ECX,EDX
  SHR     EAX, 1
  OR      EAX, EBX
  SHR     EBX, 2
  JNZ     @Step1
  SHL     EAX, 8
  JMP     @Step3
@Step2:
  SHR     EAX, 1
  SHR     EBX, 2
  JNZ     @Step1
  SHL     EAX, 8
@Step3:
  POP     EBX
{$ENDIF}
end;

function FixedSqrtHighResolution(Value: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
const
  CFixedOneAsSingle : Single = 65536;
begin
  Result := Round(Sqrt(Value * CFixedOneAsSingle));
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
  MOV     ECX,EDX
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

end.

