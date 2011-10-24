unit DAV_FixedPoint;

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
  Math;

type
  PFixed16Dot16Point = ^TFixed16Dot16Point;
  TFixed16Dot16Point = packed record
  public
  {$IFDEF DELPHI14_UP}
    constructor Create(const Fixed: Integer); overload;
    constructor Create(const Frac: Byte; Int: SmallInt); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator LessThan(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TFixed16Dot16Point): Boolean;
    class operator Negative(const Value: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Positive(const Value: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Add(const Lhs, Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Subtract(const Lhs, Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Multiply(const Lhs, Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Divide(const Lhs, Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
    class operator Round(const Value: TFixed16Dot16Point): Integer;
    class operator LeftShift(const Value: TFixed16Dot16Point; Shift: Byte): TFixed16Dot16Point;
    class operator RightShift(const Value: TFixed16Dot16Point; Shift: Byte): TFixed16Dot16Point;

    class function Zero: TFixed16Dot16Point; inline; static;
    class function One: TFixed16Dot16Point; inline; static;
    class function Two: TFixed16Dot16Point; inline; static;
    class function Half: TFixed16Dot16Point; inline; static;

    class function ArcTan2(const A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload; static;

//    procedure Sin; overload;
  {$ENDIF}
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
  {$IFDEF DELPHI14_UP}
  public
    constructor Create(const Fixed: Integer); overload;
    constructor Create(const Frac: Byte; Int: Integer); overload;
    constructor Create(const Frac: Byte; Low: Byte; High: SmallInt); overload;

    // operator overloads
    class operator Equal(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator NotEqual(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator LessThan(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator LessThanOrEqual(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator GreaterThan(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator GreaterThanOrEqual(const Lhs, Rhs: TFixed24Dot8Point): Boolean;
    class operator Negative(const Value: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Positive(const Value: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Add(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Subtract(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Multiply(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Divide(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
    class operator Round(const Value: TFixed24Dot8Point): Integer;
    class operator LeftShift(const Value: TFixed24Dot8Point; Shift: Byte): TFixed24Dot8Point;
    class operator RightShift(const Value: TFixed24Dot8Point; Shift: Byte): TFixed24Dot8Point;

    class function Zero: TFixed24Dot8Point; inline; static;
    class function One: TFixed24Dot8Point; inline; static;
    class function Two: TFixed24Dot8Point; inline; static;
    class function Half: TFixed24Dot8Point; inline; static;

    class function ArcTan2(const A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload; static;

//    procedure Sin; overload;
  {$ENDIF}
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
function FixedAbs(Value: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedAbs(Value: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedSign(Value: TFixed16Dot16Point): TValueSign; overload;
function FixedSign(Value: TFixed24Dot8Point): TValueSign; overload;
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
function FixedMin(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedMin(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
function FixedMax(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
function FixedMax(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;

const
  CFixed16Dot16Two : TFixed16Dot16Point = (Fixed : $20000);
  CFixed16Dot16One : TFixed16Dot16Point = (Fixed : $10000);
  CFixed16Dot16Half : TFixed16Dot16Point = (Fixed : $8000);
  CFixed16Dot16Zero : TFixed16Dot16Point = (Fixed : $0);
  CFixed16Dot16ToFloat = 1 / $10000;
  CFixed16Dot16PI : TFixed16Dot16Point = (Fixed : Round(PI * $10000));
  CFixed16Dot16TWOPI : TFixed16Dot16Point = (Fixed : Round(PI * $20000));

  CFixed24Dot8Two : TFixed24Dot8Point = (Fixed : $200);
  CFixed24Dot8One : TFixed24Dot8Point = (Fixed : $100);
  CFixed24Dot8Half : TFixed24Dot8Point = (Fixed : $80);
  CFixed24Dot8Zero : TFixed24Dot8Point = (Fixed : $0);
  CFixed24Dot8ToFloat = 1 / $100;
  CFixed24Dot8PI : TFixed24Dot8Point = (Fixed : Round(PI * $100));
  CFixed24Dot8TWOPI : TFixed24Dot8Point = (Fixed : Round(PI * $200));

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

function FixedAbs(Value: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

function FixedAbs(Value: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

function FixedSign(Value: TFixed16Dot16Point): TValueSign;
begin
  Result := Sign(Value.Fixed);
end;

function FixedSign(Value: TFixed24Dot8Point): TValueSign;
begin
  Result := Sign(Value.Fixed);
end;

function FixedFloor(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or (Value.Fixed shr 16);
{$ELSE}
asm
  SAR     Value, 16;
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
{$ENDIF}
end;

function FixedFloor(Value: TFixed24Dot8Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or (Value.Fixed shr 8);
{$ELSE}
asm
  SAR     Value, 8;
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
{$ENDIF}
end;

function FixedCeil(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or ((Value.Fixed + $FFFF) shr 16);
{$ELSE}
asm
  ADD     Value, $FFFF
  SAR     Value, 16;
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
{$ENDIF}
end;

function FixedCeil(Value: TFixed24Dot8Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := (Value.Fixed and $80000000) or ((Value.Fixed + $FF) shr 8);
{$ELSE}
asm
  ADD     Value, $FF
  SAR     Value, 8;
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
{$ENDIF}
end;

function FixedRound(Value: TFixed16Dot16Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := {(Value.Fixed and $80000000) or} (Value.Fixed + $7FFF) shr 16;
{$ELSE}
asm
  ADD     Value, $7FFF
  SAR     Value, 16
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
{$ENDIF}
end;

function FixedRound(Value: TFixed24Dot8Point): Integer;
{$IFDEF PUREPASCAL}
begin
  Result := {(Value.Fixed and $80000000) or} (Value.Fixed + $7F) shr 8;
{$ELSE}
asm
  ADD     Value, $7F
  SAR     Value, 8
  {$IFDEF CPUx86_64}
  MOV     Result, Value
  {$ENDIF}
{$ENDIF}
end;

function FixedAdd(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
{$ENDIF}
end;

function FixedAdd(A, B: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed + B.Fixed;
{$ELSE}
asm
  ADD     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
{$ENDIF}
end;

function FixedSub(A, B: TFixed16Dot16Point): TFixed16Dot16Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
{$ENDIF}
end;

function FixedSub(A, B: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
begin
  Result.Fixed := A.Fixed - B.Fixed;
{$ELSE}
asm
  SUB     A, B
  {$IFDEF CPUx86_64}
  MOV     Result, A
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     EAX, A
  IMUL    B
  SHRD    EAX, EDX, 16
  {$ELSE}
  IMUL    B
  SHRD    A, B, 16
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 8
  {$ELSE}
  IMUL    B
  SHRD    A, B, 8
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 16
  {$ELSE}
  IMUL    B
  SHRD    A, B, 16
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 8
  {$ELSE}
  IMUL    B
  SHRD    A, B, 8
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 16
  {$ELSE}
  IMUL    B
  SHRD    A, B, 16
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  IMUL    RDX
  SHRD    RAX, RDX, 8
  {$ELSE}
  IMUL    B
  SHRD    A, B, 8
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 16
  SHL     RAX, 16
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 16
  SHL     A, 16
  IDIV    ECX
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 8
  SHL     RAX, 8
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 8
  SHL     A, 8
  IDIV    ECX
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 16
  SHL     RAX, 16
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    B, A, 16
  SHL     A, 16
  IDIV    ECX
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  MOV     RAX, RCX
  MOV     RCX, RDX
  CDQ
  SHLD    RDX, RAX, 8
  SHL     RAX, 8
  IDIV    RDX
  {$ELSE}
  MOV     ECX, B
  CDQ
  SHLD    EDX, EAX, 8
  SHL     EAX, 8
  IDIV    ECX
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  XOR     RAX, RAX
  MOV     RDX, 1
  IDIV    RCX
  {$ELSE}
  MOV     ECX, Value
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  XOR     RAX, RAX
  MOV     RDX, 1
  IDIV    RCX
  SHR     RAX, 16
  {$ELSE}
  MOV     ECX, Value
  XOR     EAX, EAX
  MOV     EDX, 1
  IDIV    ECX
  SHR     EAX, 16
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  IMUL    RCX, RCX
  SHRD    RCX, RDX, 16
  MOV     RAX, RCX
  {$ELSE}
  IMUL    Value
  SHRD    EAX, EDX, 16
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  IMUL    RCX, RCX
  SHRD    RCX, RDX, 8
  MOV     RAX, RCX
  {$ELSE}
  IMUL    Value
  SHRD    EAX, EDX, 8
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  XOR     EAX, EAX
  MOV     R8D, $40000000

  @Step1:
  MOV     EDX, ECX
  SUB     EDX, R8D
  JL      @Step2
  SUB     EDX, EAX
  JL      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step1
  JMP     @Step3
@Step2:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step1
@Step3:
  SHL     EAX, 8
  {$ELSE}
  PUSH    EBX
  MOV     ECX, Value
  XOR     Value, Value
  MOV     EBX, $40000000
@Step1:
  MOV     EDX, ECX
  SUB     EDX, EBX
  JL      @Step2
  SUB     EDX, Value
  JL      @Step2
  MOV     ECX, EDX
  SHR     Value, 1
  OR      Value, EBX
  SHR     EBX, 2
  JNZ     @Step1
  JMP     @Step3
@Step2:
  SHR     Value, 1
  SHR     EBX, 2
  JNZ     @Step1
@Step3:
  SHL     Value, 8
  POP     EBX
  {$ENDIF}
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
  {$IFDEF CPUx86_64}
  XOR     EAX, EAX
  MOV     R8D, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, R8D
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step1

@Step3:
  MOV     R8D, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, R8D
  jb      @Step5
  SUB     EDX, EAX
  jb      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step4

@Step6:
  {$ELSE}
  PUSH    EBX
  MOV     ECX, Value
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
{$ENDIF}
end;

function FixedSqrt(Value: TFixed24Dot8Point): TFixed24Dot8Point;
{$IFDEF PUREPASCAL}
var
  IntResult : Integer absolute Result;
const
  CFixed24Dot8OneAsSingle : Single = 256;
begin
  IntResult := Round(Sqrt(Value.Fixed * CFixed24Dot8OneAsSingle));
{$ELSE}
asm
  {$IFDEF CPUx86_64}
  XOR     EAX, EAX
  MOV     R8D, $40000000

@Step1:
  MOV     EDX, ECX
  SUB     EDX, R8D
  JB      @Step2
  SUB     EDX, EAX
  JB      @Step2
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step1
  JZ      @Step3

@Step2:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step1

@Step3:
  MOV     R8D, $00004000
  SHL     EAX, 16
  SHL     ECX, 16

@Step4:
  MOV     EDX, ECX
  SUB     EDX, R8D
  jb      @Step5
  SUB     EDX, EAX
  JB      @Step5
  MOV     ECX, EDX
  SHR     EAX, 1
  OR      EAX, R8D
  SHR     R8D, 2
  JNZ     @Step4
  JMP     @Step6

@Step5:
  SHR     EAX, 1
  SHR     R8D, 2
  JNZ     @Step4

@Step6:
  SHR     EAX, 4

  {$ELSE}
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
  JB      @Step5
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
{$ENDIF}
end;

function FixedMin(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
begin
  if A.Fixed < B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMin(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
begin
  if A.Fixed < B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMax(A, B: TFixed16Dot16Point): TFixed16Dot16Point; overload;
begin
  if A.Fixed > B.Fixed then
    Result := A
  else
    Result := B;
end;

function FixedMax(A, B: TFixed24Dot8Point): TFixed24Dot8Point; overload;
begin
  if A.Fixed > B.Fixed then
    Result := A
  else
    Result := B;
end;


{$IFDEF DELPHI14_UP}

{ TFixed16Dot16Point }

constructor TFixed16Dot16Point.Create(const Fixed: Integer);
begin
  Self.Fixed := Fixed;
end;

constructor TFixed16Dot16Point.Create(const Frac: Byte; Int: SmallInt);
begin
  Self.Frac := Frac;
  Self.Int := Fixed;
end;

class operator TFixed16Dot16Point.Add(const Lhs,
  Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  FixedAdd(Lhs, Rhs);
end;

class function TFixed16Dot16Point.ArcTan2(const A,
  B: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result := ConvertToFixed16Dot16Point(
    Math.ArcTan2(ConvertFromFixed16Dot16Point(A), ConvertFromFixed16Dot16Point(B)));
end;

class operator TFixed16Dot16Point.Divide(const Lhs,
  Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed16Dot16Point.Equal(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := Lhs.Fixed = Rhs.Fixed;
end;

class operator TFixed16Dot16Point.GreaterThan(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := Lhs.Fixed > Rhs.Fixed;
end;

class operator TFixed16Dot16Point.GreaterThanOrEqual(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := Lhs.Fixed >= Rhs.Fixed;
end;

class function TFixed16Dot16Point.Half: TFixed16Dot16Point;
begin
  Result := CFixed16Dot16Half;
end;

class operator TFixed16Dot16Point.LeftShift(const Value: TFixed16Dot16Point;
  Shift: Byte): TFixed16Dot16Point;
begin
  Result.Fixed := Value.Fixed shl Shift;
end;

class operator TFixed16Dot16Point.LessThan(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := Lhs.Fixed < Rhs.Fixed;
end;

class operator TFixed16Dot16Point.LessThanOrEqual(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := Lhs.Fixed <= Rhs.Fixed;
end;

class operator TFixed16Dot16Point.Multiply(const Lhs,
  Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  FixedMul(Lhs, Rhs);
end;

class operator TFixed16Dot16Point.Negative(
  const Value: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result.Fixed := -Value.Fixed;
end;

class operator TFixed16Dot16Point.NotEqual(const Lhs,
  Rhs: TFixed16Dot16Point): Boolean;
begin
  Result := Lhs.Fixed <> Rhs.Fixed;
end;

class function TFixed16Dot16Point.One: TFixed16Dot16Point;
begin
  Result := CFixed16Dot16One;
end;

class operator TFixed16Dot16Point.Positive(
  const Value: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

class operator TFixed16Dot16Point.RightShift(const Value: TFixed16Dot16Point;
  Shift: Byte): TFixed16Dot16Point;
begin
  Result.Fixed := Value.Fixed shr Shift;
end;

class operator TFixed16Dot16Point.Round(
  const Value: TFixed16Dot16Point): Integer;
begin
  Result := FixedRound(Value);
end;

class operator TFixed16Dot16Point.Subtract(const Lhs,
  Rhs: TFixed16Dot16Point): TFixed16Dot16Point;
begin
  Result := FixedSub(Lhs, Rhs);
end;

class function TFixed16Dot16Point.Two: TFixed16Dot16Point;
begin
  Result := CFixed16Dot16Two;
end;

class function TFixed16Dot16Point.Zero: TFixed16Dot16Point;
begin
  Result.Fixed := 0;
end;


{ TFixed24Dot8Point }

constructor TFixed24Dot8Point.Create(const Fixed: Integer);
begin
  Self.Fixed := Fixed;
end;

constructor TFixed24Dot8Point.Create(const Frac: Byte; Int: Integer);
begin
  Self.Fixed := Int shl 8;
  Self.Frac := Frac;
end;

constructor TFixed24Dot8Point.Create(const Frac: Byte; Low: Byte;
  High: SmallInt);
begin
  Self.Frac := Frac;
  Self.Low := Low;
  Self.High := High;
end;

class operator TFixed24Dot8Point.Add(const Lhs, Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  FixedAdd(Lhs, Rhs);
end;

class operator TFixed24Dot8Point.Negative(const Value: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result.Fixed := -Value.Fixed;
end;

class operator TFixed24Dot8Point.Positive(const Value: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result.Fixed := Abs(Value.Fixed);
end;

class operator TFixed24Dot8Point.Divide(const Lhs,
  Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result := FixedDiv(Lhs, Rhs);
end;

class operator TFixed24Dot8Point.Equal(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := Lhs.Fixed = Rhs.Fixed;
end;

class operator TFixed24Dot8Point.GreaterThan(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := Lhs.Fixed > Rhs.Fixed;
end;

class operator TFixed24Dot8Point.GreaterThanOrEqual(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := Lhs.Fixed >= Rhs.Fixed;
end;

class function TFixed24Dot8Point.Half: TFixed24Dot8Point;
begin
  Result := CFixed24Dot8Half;
end;

class operator TFixed24Dot8Point.LeftShift(
  const Value: TFixed24Dot8Point; Shift: Byte): TFixed24Dot8Point;
begin
  Result.Fixed := Value.Fixed shl Shift;
end;

class operator TFixed24Dot8Point.LessThan(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := Lhs.Fixed < Rhs.Fixed;
end;

class operator TFixed24Dot8Point.LessThanOrEqual(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := Lhs.Fixed <= Rhs.Fixed;
end;

class operator TFixed24Dot8Point.Multiply(const Lhs,
  Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  FixedMul(Lhs, Rhs);
end;

class operator TFixed24Dot8Point.NotEqual(const Lhs,
  Rhs: TFixed24Dot8Point): Boolean;
begin
  Result := Lhs.Fixed <> Rhs.Fixed;
end;

class function TFixed24Dot8Point.One: TFixed24Dot8Point;
begin
  Result := CFixed24Dot8One;
end;

class operator TFixed24Dot8Point.RightShift(
  const Value: TFixed24Dot8Point; Shift: Byte): TFixed24Dot8Point;
begin
  Result.Fixed := Value.Fixed shr Shift;
end;

class operator TFixed24Dot8Point.Round(const Value: TFixed24Dot8Point): Integer;
begin
  Result := FixedRound(Value)
end;

class operator TFixed24Dot8Point.Subtract(const Lhs,
  Rhs: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result := FixedSub(Lhs, Rhs);
end;

class function TFixed24Dot8Point.Two: TFixed24Dot8Point;
begin
  Result := CFixed24Dot8Two;
end;

class function TFixed24Dot8Point.Zero: TFixed24Dot8Point;
begin
  Result.Fixed := 0;
end;

class function TFixed24Dot8Point.ArcTan2(const A, B: TFixed24Dot8Point): TFixed24Dot8Point;
begin
  Result := ConvertToFixed24Dot8Point(
    Math.ArcTan2(ConvertFromFixed24Dot8Point(A), ConvertFromFixed24Dot8Point(B)));
end;

(*
procedure TFixed24Dot8Point.Sin;
begin
  Self := ConvertToFixed24Dot8Point(Math.ArcTan(ConvertFromFixed24Dot8Point(Self)));
end;
*)

{$ENDIF}

end.
