unit DAV_VectorMath;

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
//  by Christian-W. Budde. All Rights Reserved.                        <<       //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  {$IFDEF FPC}LCLIntf, Types, {$ELSE}Windows, {$ENDIF} Math, DAV_Types;

type
  TDAVVector32 = TDAV4SingleArray;
  TDAVVector64 = TDAV4DoubleArray;

  TDAVHomogeneousVector32 = TDAVVector32;
  TDAVHomogeneousVector64 = TDAVVector64;

  TDAVMatrix32 = Array [0..3] of TDAVVector32;
  TDAVMatrix64 = Array [0..3] of TDAVVector64;

  TSphereVector2D = record
    Azimuth : Single;   // 0..2*PI
    Polar   : Single;   // 0..PI
  end;

  TSphereVector3D = record
    Azimuth : Single;   // 0..2*PI
    Polar   : Single;   // 0..PI
    Radius  : Single;   // is assumed to be 1
  end;

const
  CHomogeneousXVector32    : TDAVHomogeneousVector32 = (1, 0, 0, 0);
  CHomogeneousYVector32    : TDAVHomogeneousVector32 = (0, 1, 0, 0);
  CHomogeneousZVector32    : TDAVHomogeneousVector32 = (0, 0, 1, 0);
  CHomogeneousWVector32    : TDAVHomogeneousVector32 = (0, 0, 0, 1);
  CHomogeneousXYVector32   : TDAVHomogeneousVector32 = (1, 1, 0, 0);
  CHomogeneousXYZVector32  : TDAVHomogeneousVector32 = (1, 1, 1, 0);
  CHomogeneousWXYZVector32 : TDAVHomogeneousVector32 = (1, 1, 1, 1);
  CHomogeneousNullVector32 : TDAVHomogeneousVector32 = (0, 0, 0, 0);

  CHomogeneousXVector64    : TDAVHomogeneousVector64 = (1, 0, 0, 0);
  CHomogeneousYVector64    : TDAVHomogeneousVector64 = (0, 1, 0, 0);
  CHomogeneousZVector64    : TDAVHomogeneousVector64 = (0, 0, 1, 0);
  CHomogeneousWVector64    : TDAVHomogeneousVector64 = (0, 0, 0, 1);
  CHomogeneousXYVector64   : TDAVHomogeneousVector64 = (1, 1, 0, 0);
  CHomogeneousXYZVector64  : TDAVHomogeneousVector64 = (1, 1, 1, 0);
  CHomogeneousXYZWVector64 : TDAVHomogeneousVector64 = (1, 1, 1, 1);
  CHomogeneousNullVector64 : TDAVHomogeneousVector64 = (0, 0, 0, 0);



  CIdentityHomogeneousMatrix32: TDAVMatrix32 = ((1, 0, 0, 0),
                                                (0, 1, 0, 0),
                                                (0, 0, 1, 0),
                                                (0, 0, 0, 1));
  CIdentityHomogeneousMatrix64: TDAVMatrix64 = ((1, 0, 0, 0),
                                                (0, 1, 0, 0),
                                                (0, 0, 1, 0),
                                                (0, 0, 0, 1));
  CEmptyHomogeneousMatrix32: TDAVMatrix32 = ((0, 0, 0, 0),
                                             (0, 0, 0, 0),
                                             (0, 0, 0, 0),
                                             (0, 0, 0, 0));
  CEmptyHomogeneousMatrix64: TDAVMatrix64 = ((0, 0, 0, 0),
                                             (0, 0, 0, 0),
                                             (0, 0, 0, 0),
                                             (0, 0, 0, 0));

  C2PI    : Single = 6.283185307;
  CInv2PI : Single = 1 / 6.283185307;
  CInv360 : Single = 1 / 360;
  C180    : Single = 180;
  C360    : Single = 360;

function VectorMake(const x, y, z, w: Single): TDAVVector32; overload;
procedure SetVector(var Value: TDAVVector32; const x, y, z, w: Single); overload;
procedure SetVector(var Value: TDAVVector32; const vSrc: TDAVVector32); overload;
procedure RstVector(var Value: TDAVVector32); overload;
function VectorAdd(const v1, v2: TDAVVector32): TDAVVector32; overload;
procedure VectorAdd(const v1, v2: TDAVVector32; var vr: TDAVVector32); overload;
function VectorAdd(const Value: TDAVVector32; const f: Single): TDAVVector32; overload;
procedure AddVector(var v1: TDAVVector32; const v2: TDAVVector32); overload;
procedure AddVector(var Value: TDAVVector32; const f: Single); overload;
function VectorSubtract(const V1, V2: TDAVVector32): TDAVVector32; overload;
procedure VectorSubtract(const v1, v2: TDAVVector32; var Result: TDAVVector32); overload;
function VectorSubtract(const v1: TDAVVector32; delta: Single): TDAVVector32; overload;
procedure SubtractVector(var V1: TDAVVector32; const V2: TDAVVector32); overload;
procedure CombineVector(var vr: TDAVVector32; const Value: TDAVVector32; var f: Single); overload;
function VectorCombine(const V1, V2: TDAVVector32; const F1, F2: Single): TDAVVector32; overload;
procedure VectorCombine(const V1, V2: TDAVVector32; const F1, F2: Single; var vr: TDAVVector32); overload;
procedure VectorCombine(const V1, V2: TDAVVector32; const F2: Single; var vr: TDAVVector32); overload;
function VectorCombine3(const V1, V2, V3: TDAVVector32; const F1, F2, F3: Single): TDAVVector32; overload;
procedure VectorCombine3(const V1, V2, V3: TDAVVector32; const F1, F2, F3: Single; var vr: TDAVVector32); overload;
function VectorDotProduct(const V1, V2: TDAVVector32): Single; overload;
function PointProject(const p, origin, direction: TDAVVector32): Single; overload;
function VectorCrossProduct(const V1, V2: TDAVVector32): TDAVVector32; overload;
procedure VectorCrossProduct(const v1, v2: TDAVVector32; var vr: TDAVVector32); overload;
function Lerp(const start, stop, t: Single): Single;
function AngleLerp(start, stop, t: Single): Single;
function DistanceBetweenAngles(angle1, angle2: Single): Single;
function VectorLerp(const v1, v2: TDAVVector32; t: Single): TDAVVector32; overload;
procedure VectorLerp(const v1, v2: TDAVVector32; t: Single; var vr: TDAVVector32); overload;
function VectorLength(const Value: TDAVVector32): Single; overload;
function VectorLength(const Value: array of Single): Single; overload;
function VectorNorm(const x, y: Single): Single; overload;
function VectorNorm(const Value: TDAVVector32): Single; overload;
function VectorNorm(var Value: array of Single): Single; overload;
procedure NormalizeVector(var Value: TDAVVector32); overload;
function VectorNormalize(const Value: TDAVVector32): TDAVVector32; overload;
function VectorNegate(const Value: TDAVVector32): TDAVVector32; overload;
procedure NegateVector(var Value: TDAVVector32); overload;
procedure NegateVector(var Value: array of Single); overload;
procedure ScaleVector(var Value: TDAVVector32; const Factor: Single); overload;
procedure ScaleVector(var Value: TDAVVector32; const Factor: TDAVVector32); overload;
function VectorScale(const Value: TDAVVector32; const Factor: Single): TDAVVector32; overload;
procedure VectorScale(const Value: TDAVVector32; const Factor: Single; var vr: TDAVVector32); overload;
procedure DivideVector(var Value: TDAVVector32; const divider: TDAVVector32); overload;
function VectorEquals(const V1, V2: TDAVVector32): Boolean; overload;
function VectorIsNull(const Value: TDAVVector32): Boolean; overload;
function VectorSpacing(const v1, v2: TDAVVector32): Single; overload;
function VectorDistance(const v1, v2: TDAVVector32): Single; overload;
function VectorDistance2(const v1, v2: TDAVVector32): Single; overload;
procedure RotateVector(var vector: TDAVVector32; const axis: TDAVVector32; Angle: Single); overload;
procedure AbsVector(var Value: TDAVVector32); overload;
function VectorAbs(const Value: TDAVVector32): TDAVVector32; overload;
procedure SetMatrix(var dest: TDAVMatrix64; const src: TDAVMatrix32); overload;
procedure SetMatrixRow(var dest: TDAVMatrix32; rowNb: Integer; const aRow: TDAVVector32); overload;
function CreateScaleMatrix(const Value: TDAVVector32): TDAVMatrix32; overload;
function CreateTranslationMatrix(const Value: TDAVVector32): TDAVMatrix32; overload;
function CreateScaleAndTranslationMatrix(const scale, offset: TDAVVector32): TDAVMatrix32; overload;
function CreateRotationMatrixX(const sine, cosine: Single): TDAVMatrix32; overload;
function CreateRotationMatrixX(const Angle: Single): TDAVMatrix32; overload;
function CreateRotationMatrixY(const sine, cosine: Single): TDAVMatrix32; overload;
function CreateRotationMatrixY(const Angle: Single): TDAVMatrix32; overload;
function CreateRotationMatrixZ(const sine, cosine: Single): TDAVMatrix32; overload;
function CreateRotationMatrixZ(const Angle: Single): TDAVMatrix32; overload;
function CreateRotationMatrix(const anAxis: TDAVVector32; Angle: Single): TDAVMatrix32; overload;
function MatrixMultiply(const M1, M2: TDAVMatrix32): TDAVMatrix32; overload;
procedure MatrixMultiply(const M1, M2: TDAVMatrix32; var MResult: TDAVMatrix32); overload;
function VectorTransform(const Value: TDAVVector32; const M: TDAVMatrix32): TDAVVector32; overload;
procedure SetMatrixLength(Matrix: TDAVDoubleDynMatrix; Size: TPoint); overload;
procedure SetMatrixLength(Matrix: TDAVSingleDynMatrix; Size: TPoint); overload;
function MatrixDeterminant(const M: TDAVMatrix32): Single; overload;
procedure AdjointMatrix(var M: TDAVMatrix32); overload;
procedure ScaleMatrix(var M: TDAVMatrix32; const Factor: Single); overload;
procedure ScaleMatrix(var M: TDAVMatrix32; const Factor: Double); overload;
procedure TranslateMatrix(var M: TDAVMatrix32; const Value: TDAVVector32); overload;
procedure NormalizeMatrix(var M: TDAVMatrix32);
procedure TransposeMatrix(var M: TDAVMatrix32); overload;
procedure InvertMatrix(var M: TDAVMatrix32); overload;
function AnglePreservingMatrixInvert(const mat: TDAVMatrix32): TDAVMatrix32;
procedure SetPlane(var dest: TDAVHomogeneousVector64; const src: TDAVHomogeneousVector32);
procedure NormalizePlane(var plane: TDAVHomogeneousVector32);
function PlaneEvaluatePoint(const plane: TDAVHomogeneousVector32; const Point: TDAVVector32): Single; overload;
function PointIsInHalfSpace(const Point, PlanePoint, PlaneNormal: TDAVVector32): Boolean; overload;
function PointPlaneDistance(const Point, PlanePoint, PlaneNormal: TDAVVector32): Single; overload;

function MakeDblVector(var Value: array of Double): TDAVHomogeneousVector64;
function VectorDblToFlt(const Value: TDAVHomogeneousVector64): TDAVHomogeneousVector32;
function VectorFltToDbl(const Value: TDAVVector32): TDAVHomogeneousVector64;
procedure DivMod(Dividend: Integer; Divisor: Word; var Result, Remainder: Word);
function NormalizeAngle(Angle: Single): Single;
function NormalizeDegAngle(Angle: Single): Single;
function RSqrt(Value: Single): Single;

function GeometryOptimizationMode: String;
procedure BeginFPUOnlySection;
procedure EndFPUOnlySection;

function GetOrthodromicAngle(A, B: TSphereVector2D): Single; overload;
function GetOrthodromicAngle(A, B: TSphereVector3D): Single; overload;
function GetOrthodromicAngle2D(A, B: TSphereVector3D): Single;
function MakeSphereVector2D(const Azimuth, Polar: Single): TSphereVector2D;
function MakeSphereVector3D(const Azimuth, Polar: Single): TSphereVector3D;

var
   // this var is adjusted during "initialization", current values are
   // + 0 : use standard optimized FPU code
   // + 1 : use 3DNow! optimized code (requires K6-2/3 CPU)
   // + 2 : use Intel SSE code (Pentium III, NOT IMPLEMENTED YET !)
  vSIMD: Byte = 0;

implementation

uses
  SysUtils, DAV_Common, DAV_Math;

const
{$IFNDEF PUREPASCAL}
  // FPU status flags (high order byte)
  cwChop: Word = $1F3F;
{$ENDIF}

  CZero: Single = 0.0;
  COne: Single = 1.0;
  COneDotFive: Single = 0.5;

function GeometryOptimizationMode: String;
begin
  case vSIMD of
    0 : Result := 'FPU';
    1 : Result := '3DNow!';
    2 : Result := 'SSE';
  else
    Result := '*ERR*';
   end;
end;

var
  vOldSIMD: Byte;
  vFPUOnlySectionCounter: Integer;

procedure BeginFPUOnlySection;
begin
  if vFPUOnlySectionCounter = 0 then
    vOldSIMD := vSIMD;
  Inc(vFPUOnlySectionCounter);
  vSIMD := 0;
end;

procedure EndFPUOnlySection;
begin
  Dec(vFPUOnlySectionCounter);
  Assert(vFPUOnlySectionCounter >= 0);
  if vFPUOnlySectionCounter = 0
   then vSIMD := vOldSIMD;
end;

function VectorMake(const x, y, z, w: Single): TDAVVector32;
begin
  Result[0] := x;
  Result[1] := y;
  Result[2] := z;
  Result[3] := w;
end;

procedure SetVector(var Value: TDAVVector32; const x, y, z, w: Single);
begin
  Value[0] := x;
  Value[1] := y;
  Value[2] := z;
  Value[3] := w;
end;

procedure SetVector(var Value: TDAVVector32; const vSrc: TDAVVector32);
begin
  Value[0] := vSrc[0];
  Value[1] := vSrc[1];
  Value[2] := vSrc[2];
  Value[3] := vSrc[3];
end;

procedure RstVector(var Value: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    xor   edx, edx
    mov   [eax     ], edx
    mov   [eax +  4], edx
    mov   [eax +  8], edx
    mov   [eax + 12], edx
{$ELSE}
begin
   Value[0] := 0;
   Value[1] := 0;
   Value[2] := 0;
   Value[3] := 0;
{$ENDIF}
end;

function VectorAdd(const v1, v2: TDAVVector32): TDAVVector32;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// movq  mm0, [eax]
    db $0F,$0F,$02,$9E       /// pfadd mm0, [edx]
    db $0F,$7F,$01           /// movq  [ecx], mm0
    db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
    db $0F,$0F,$4A,$08,$9E   /// pfadd mm1, [edx+8]
    db $0F,$7F,$49,$08       /// movq  [ecx+8], mm1
    db $0F,$0E               /// femms
    ret

    @@FPU:
    FLD  DWORD PTR [EAX]
    FADD DWORD PTR [EDX]
    FSTP DWORD PTR [ECX]
    FLD  DWORD PTR [EAX+4]
    FADD DWORD PTR [EDX+4]
    FSTP DWORD PTR [ECX+4]
    FLD  DWORD PTR [EAX+8]
    FADD DWORD PTR [EDX+8]
    FSTP DWORD PTR [ECX+8]
    FLD  DWORD PTR [EAX+12]
    FADD DWORD PTR [EDX+12]
    FSTP DWORD PTR [ECX+12]
{$ELSE}
begin
   Result[0] := v1[0] + v2[0];
   Result[1] := v1[1] + v2[1];
   Result[2] := v1[2] + v2[2];
   Result[3] := v1[3] + v2[3];
{$ENDIF}
end;

procedure VectorAdd(const v1, v2: TDAVVector32; var vr: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// movq  mm0, [eax]
    db $0F,$0F,$02,$9E       /// pfadd mm0, [edx]
    db $0F,$7F,$01           /// movq  [ecx], mm0
    db $0F,$6F,$48,$08       /// movq  mm1, [eax+8]
    db $0F,$0F,$4A,$08,$9E   /// pfadd mm1, [edx+8]
    db $0F,$7F,$49,$08       /// movq  [ecx+8], mm1
    db $0F,$0E               /// femms
    ret

    @@FPU:
    FLD  DWORD PTR [EAX]
    FADD DWORD PTR [EDX]
    FSTP DWORD PTR [ECX]
    FLD  DWORD PTR [EAX+4]
    FADD DWORD PTR [EDX+4]
    FSTP DWORD PTR [ECX+4]
    FLD  DWORD PTR [EAX+8]
    FADD DWORD PTR [EDX+8]
    FSTP DWORD PTR [ECX+8]
    FLD  DWORD PTR [EAX+12]
    FADD DWORD PTR [EDX+12]
    FSTP DWORD PTR [ECX+12]
{$ELSE}
begin
   vr[0] := v1[0] + v2[0];
   vr[1] := v1[1] + v2[1];
   vr[2] := v1[2] + v2[2];
   vr[3] := v1[3] + v2[3];
{$ENDIF}
end;

function VectorAdd(const Value: TDAVVector32; const f: Single): TDAVVector32;
begin
  Result[0] := Value[0] + f;
  Result[1] := Value[1] + f;
  Result[2] := Value[2] + f;
  Result[3] := Value[3] + f;
end;

procedure AddVector(var v1: TDAVVector32; const v2: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
    db $0F,$0F,$02,$9E       /// PFADD MM0, [EDX]
    db $0F,$7F,$00           /// MOVQ  [EAX], MM0
    db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
    db $0F,$0F,$4A,$08,$9E   /// PFADD MM1, [EDX+8]
    db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
    db $0F,$0E               /// FEMMS
    ret
    @@FPU:
    FLD  DWORD PTR [EAX]
    FADD DWORD PTR [EDX]
    FSTP DWORD PTR [EAX]
    FLD  DWORD PTR [EAX+4]
    FADD DWORD PTR [EDX+4]
    FSTP DWORD PTR [EAX+4]
    FLD  DWORD PTR [EAX+8]
    FADD DWORD PTR [EDX+8]
    FSTP DWORD PTR [EAX+8]
    FLD  DWORD PTR [EAX+12]
    FADD DWORD PTR [EDX+12]
    FSTP DWORD PTR [EAX+12]
{$ELSE}
begin
   v1[0] := v1[0] + v2[0];
   v1[1] := v1[1] + v2[1];
   v1[2] := v1[2] + v2[2];
   v1[3] := v1[3] + v2[3];
{$ENDIF}
end;

procedure AddVector(var Value: TDAVVector32; const f: Single);
begin
  Value[0] := Value[0] + f;
  Value[1] := Value[1] + f;
  Value[2] := Value[2] + f;
  Value[3] := Value[3] + f;
end;

function VectorSubtract(const v1, v2: TDAVVector32): TDAVVector32;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
    db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
    db $0F,$7F,$01           /// MOVQ  [ECX], MM0
    db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
    db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
    db $0F,$7F,$49,$08       /// MOVQ  [ECX+8], MM1
    db $0F,$0E               /// FEMMS
    ret
    @@FPU:
    FLD  DWORD PTR [EAX]
    FSUB DWORD PTR [EDX]
    FSTP DWORD PTR [ECX]
    FLD  DWORD PTR [EAX+4]
    FSUB DWORD PTR [EDX+4]
    FSTP DWORD PTR [ECX+4]
    FLD  DWORD PTR [EAX+8]
    FSUB DWORD PTR [EDX+8]
    FSTP DWORD PTR [ECX+8]
    FLD  DWORD PTR [EAX+12]
    FSUB DWORD PTR [EDX+12]
    FSTP DWORD PTR [ECX+12]
{$ELSE}
begin
   Result[0] := v1[0] - v2[0];
   Result[1] := v1[1] - v2[1];
   Result[2] := v1[2] - v2[2];
{$ENDIF}
end;

procedure VectorSubtract(const v1, v2: TDAVVector32; var Result: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
    db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
    db $0F,$7F,$01           /// MOVQ  [ECX], MM0
    db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
    db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
    db $0F,$7F,$49,$08       /// MOVQ  [ECX+8], MM1
    db $0F,$0E               /// FEMMS
    ret
    @@FPU:
    FLD  DWORD PTR [EAX     ]
    FSUB DWORD PTR [EDX     ]
    FSTP DWORD PTR [ECX     ]
    FLD  DWORD PTR [EAX +  4]
    FSUB DWORD PTR [EDX +  4]
    FSTP DWORD PTR [ECX +  4]
    FLD  DWORD PTR [EAX +  8]
    FSUB DWORD PTR [EDX +  8]
    FSTP DWORD PTR [ECX +  8]
    FLD  DWORD PTR [EAX + 12]
    FSUB DWORD PTR [EDX + 12]
    FSTP DWORD PTR [ECX + 12]
{$ELSE}
begin
   Result[0] := v1[0] - v2[0];
   Result[1] := v1[1] - v2[1];
   Result[2] := v1[2] - v2[2];
   Result[3] := v1[3] - v2[3];
{$ENDIF}
end;

function VectorSubtract(const v1: TDAVVector32; delta: Single): TDAVVector32;
begin
  Result[0] := v1[0] - delta;
  Result[1] := v1[1] - delta;
  Result[2] := v1[2] - delta;
  Result[3] := v1[3] - delta;
end;

procedure SubtractVector(var V1: TDAVVector32; const V2: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// MOVQ  MM0, [EAX]
    db $0F,$0F,$02,$9A       /// PFSUB MM0, [EDX]
    db $0F,$7F,$00           /// MOVQ  [EAX], MM0
    db $0F,$6F,$48,$08       /// MOVQ  MM1, [EAX+8]
    db $0F,$0F,$4A,$08,$9A   /// PFSUB MM1, [EDX+8]
    db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
    db $0F,$0E               /// FEMMS
    ret
    @@FPU:
    FLD  DWORD PTR [EAX    ]
    FSUB DWORD PTR [EDX    ]
    FSTP DWORD PTR [EAX    ]
    FLD  DWORD PTR [EAX + 4]
    FSUB DWORD PTR [EDX + 4]
    FSTP DWORD PTR [EAX + 4]
    FLD  DWORD PTR [EAX + 8]
    FSUB DWORD PTR [EDX + 8]
    FSTP DWORD PTR [EAX + 8]
    FLD  DWORD PTR [EAX +12]
    FSUB DWORD PTR [EDX +12]
    FSTP DWORD PTR [EAX +12]
{$ELSE}
begin
   v1[0] := v1[0]-v2[0];
   v1[1] := v1[1]-v2[1];
   v1[2] := v1[2]-v2[2];
   v1[3] := v1[3]-v2[3];
{$ENDIF}
end;

procedure CombineVector(var vr: TDAVVector32; const Value: TDAVVector32;
  var f: Single); overload;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6E,$11           /// MOVD  MM2, [ECX]
    db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
    db $0F,$6F,$02           /// MOVQ  MM0, [EDX]
    db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
    db $0F,$0F,$00,$9E       /// PFADD MM0, [EAX]
    db $0F,$7F,$00           /// MOVQ  [EAX], MM0
    db $0F,$6F,$4A,$08       /// MOVQ  MM1, [EDX+8]
    db $0F,$0F,$CA,$B4       /// PFMUL MM1, MM2
    db $0F,$0F,$48,$08,$9E   /// PFADD MM1, [EAX+8]
    db $0F,$7F,$48,$08       /// MOVQ  [EAX+8], MM1
    db $0F,$0E               /// FEMMS
    ret
    @@FPU:
    FLD  DWORD PTR [EDX     ]
    FMUL DWORD PTR [ECX     ]
    FADD DWORD PTR [EAX     ]
    FSTP DWORD PTR [EAX     ]
    FLD  DWORD PTR [EDX +  4]
    FMUL DWORD PTR [ECX     ]
    FADD DWORD PTR [EAX +  4]
    FSTP DWORD PTR [EAX +  4]
    FLD  DWORD PTR [EDX +  8]
    FMUL DWORD PTR [ECX     ]
    FADD DWORD PTR [EAX +  8]
    FSTP DWORD PTR [EAX +  8]
    FLD  DWORD PTR [EDX + 12]
    FMUL DWORD PTR [ECX     ]
    FADD DWORD PTR [EAX + 12]
    FSTP DWORD PTR [EAX + 12]
{$ELSE}
begin
   vr[0] := vr[0] + Value[0] * f;
   vr[1] := vr[1] + Value[1] * f;
   vr[2] := vr[2] + Value[2] * f;
   vr[3] := vr[3] + Value[3] * f;
{$ENDIF}
end;

function VectorCombine(const V1, V2: TDAVVector32; const F1, F2: Single): TDAVVector32;
begin
  Result[0] := (F1 * V1[0]) + (F2 * V2[0]);
  Result[1] := (F1 * V1[1]) + (F2 * V2[1]);
  Result[2] := (F1 * V1[2]) + (F2 * V2[2]);
  Result[3] := (F1 * V1[3]) + (F2 * V2[3]);
end;

procedure VectorCombine(const V1, V2: TDAVVector32; const F1, F2: Single;
  var vr: TDAVVector32); overload;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:    // 246354
    db $0F,$6E,$4D,$0C       /// MOVD  MM1, [EBP+$0C]
    db $0F,$62,$C9           /// PUNPCKLDQ MM1, MM1
    db $0F,$6E,$55,$08       /// MOVD  MM2, [EBP+$08]
    db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2

    db $0F,$6F,$18           /// MOVQ  MM3, [EAX]
    db $0F,$0F,$D9,$B4       /// PFMUL MM3, MM1
    db $0F,$6F,$22           /// MOVQ  MM4, [EDX]
    db $0F,$0F,$E2,$B4       /// PFMUL MM4, MM2
    db $0F,$0F,$DC,$9E       /// PFADD MM3, MM4
    db $0F,$7F,$19           /// MOVQ  [ECX], MM3

    db $0F,$6F,$68,$08       /// MOVQ  MM5, [EAX+8]
    db $0F,$0F,$E9,$B4       /// PFMUL MM5, MM1
    db $0F,$6F,$72,$08       /// MOVQ  MM6, [EDX+8]
    db $0F,$0F,$F2,$B4       /// PFMUL MM6, MM2
    db $0F,$0F,$EE,$9E       /// PFADD MM5, MM6
    db $0F,$7F,$69,$08       /// MOVQ  [ECX+8], MM5

    db $0F,$0E               /// FEMMS
    pop ebp
    ret $08

    @@FPU:      // 327363
    FLD   DWORD PTR [EAX]
    FMUL  DWORD PTR [EBP + $C]
    FLD   DWORD PTR [EDX]
    FMUL  DWORD PTR [EBP + $8]
    FADDP st(1), st(0)
    FSTP  DWORD PTR [ECX]

    FLD   DWORD PTR [EAX + 4]
    FMUL  DWORD PTR [EBP + $C]
    FLD   DWORD PTR [EDX + 4]
    FMUL  DWORD PTR [EBP + 8]
    FADDP st(1), st(0)
    FSTP  DWORD PTR [ECX + 4]

    FLD   DWORD PTR [EAX + 8]
    FMUL  DWORD PTR [EBP + $C]
    FLD   DWORD PTR [EDX + 8]
    FMUL  DWORD PTR [EBP + $8]
    FADDP st(1), st(0)
    FSTP  DWORD PTR [ECX + 8]

    FLD   DWORD PTR [EAX + 12]
    FMUL  DWORD PTR [EBP + $C]
    FLD   DWORD PTR [EDX + 12]
    FMUL  DWORD PTR [EBP + $8]
    FADDP st(1), st(0)
    FSTP  DWORD PTR [ECX + 12]
{$ELSE}
begin
   vr[0] := (F1 * V1[0]) + (F2 * V2[0]);
   vr[1] := (F1 * V1[1]) + (F2 * V2[1]);
   vr[2] := (F1 * V1[2]) + (F2 * V2[2]);
   vr[3] := (F1 * V1[3]) + (F2 * V2[3]);
{$ENDIF}
end;

procedure VectorCombine(const V1, V2: TDAVVector32; const F2: Single;
  var vr: TDAVVector32); overload;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:    // 121559
    db $0F,$6E,$55,$08       /// MOVD  MM2, [EBP+$08]
    db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2

    db $0F,$6F,$22           /// MOVQ  MM4, [EDX]
    db $0F,$6F,$72,$08       /// MOVQ  MM6, [EDX+8]

    db $0F,$0F,$E2,$B4       /// PFMUL MM4, MM2
    db $0F,$0F,$F2,$B4       /// PFMUL MM6, MM2

    db $0F,$0F,$20,$9E       /// PFADD MM4, [EAX]
    db $0F,$0F,$70,$08,$9E   /// PFADD MM6, [EAX+8]

    db $0F,$7F,$21           /// MOVQ  [ECX], MM4
    db $0F,$7F,$71,$08       /// MOVQ  [ECX+8], MM6

    db $0F,$0E               /// FEMMS
    pop ebp
    ret $04

    @@FPU:   
    FLD  DWORD PTR [EBP + $08]

    FLD  DWORD PTR [EDX]
    FMUL ST, ST(1)
    FADD DWORD PTR [EAX]
    FSTP DWORD PTR [ECX]

    FLD  DWORD PTR [EDX+4]
    FMUL ST, ST(1)
    FADD DWORD PTR [EAX+4]
    FSTP DWORD PTR [ECX+4]

    FLD  DWORD PTR [EDX+8]
    FMUL ST, ST(1)
    FADD DWORD PTR [EAX+8]
    FSTP DWORD PTR [ECX+8]

    FLD  DWORD PTR [EDX+12]
    FMULP
    FADD DWORD PTR [EAX+12]
    FSTP DWORD PTR [ECX+12]
{$ELSE}
begin      // 201283
   vr[0] := V1[0] + (F2 * V2[0]);
   vr[1] := V1[1] + (F2 * V2[1]);
   vr[2] := V1[2] + (F2 * V2[2]);
   vr[3] := V1[3] + (F2 * V2[3]);
{$ENDIF}
end;

function VectorCombine3(const V1, V2, V3: TDAVVector32;
  const F1, F2, F3: Single): TDAVVector32;
begin
  Result[0] := (F1 * V1[0]) + (F2 * V2[0]) + (F3 * V3[0]);
  Result[1] := (F1 * V1[1]) + (F2 * V2[1]) + (F3 * V3[1]);
  Result[2] := (F1 * V1[2]) + (F2 * V2[2]) + (F3 * V3[2]);
  Result[3] := (F1 * V1[3]) + (F2 * V2[3]) + (F3 * V3[3]);
end;

procedure VectorCombine3(const V1, V2, V3: TDAVVector32;
  const F1, F2, F3: Single; var vr: TDAVVector32);
begin
{$IFNDEF PUREPASCAL}
  asm
      test vSIMD, 1
      jz @@FPU
      @@3DNow:    // 197
      db $0F,$6E,$4D,$14       /// MOVD  MM1, [EBP+$14]
      db $0F,$62,$C9           /// PUNPCKLDQ MM1, MM1
      db $0F,$6E,$55,$10       /// MOVD  MM2, [EBP+$10]
      db $0F,$62,$D2           /// PUNPCKLDQ MM2, MM2
      db $0F,$6E,$5D,$0C       /// MOVD  MM3, [EBP+$0C]
      db $0F,$62,$DB           /// PUNPCKLDQ MM3, MM3

      db $0F,$6F,$20           /// MOVQ  MM4, [EAX]
      db $0F,$0F,$E1,$B4       /// PFMUL MM4, MM1
      db $0F,$6F,$2A           /// MOVQ  MM5, [EDX]
      db $0F,$0F,$EA,$B4       /// PFMUL MM5, MM2
      db $0F,$0F,$E5,$9E       /// PFADD MM4, MM5
      db $0F,$6F,$31           /// MOVQ  MM6, [ECX]
      db $0F,$0F,$F3,$B4       /// PFMUL MM6, MM3
      db $0F,$0F,$E6,$9E       /// PFADD MM4, MM6
      db $0F,$7F,$23           /// MOVQ  [EBX], MM4

      db $0F,$6F,$78,$08       /// MOVQ  MM7, [EAX+8]
      db $0F,$0F,$F9,$B4       /// PFMUL MM7, MM1
      db $0F,$6F,$42,$08       /// MOVQ  MM0, [EDX+8]
      db $0F,$0F,$C2,$B4       /// PFMUL MM0, MM2
      db $0F,$0F,$F8,$9E       /// PFADD MM7, MM0
      db $0F,$6F,$69,$08       /// MOVQ  MM5, [ECX+8]
      db $0F,$0F,$EB,$B4       /// PFMUL MM5, MM3
      db $0F,$0F,$FD,$9E       /// PFADD MM7, MM5
      db $0F,$7F,$7B,$08       /// MOVQ  [EBX+8], MM7

      db $0F,$0E               /// FEMMS
      pop ebx
      pop ebp
      ret $10
      @@FPU:      // 263
  end;
{$ENDIF}
  vr[0] := (F1 * V1[0]) + (F2 * V2[0]) + (F3 * V3[0]);
  vr[1] := (F1 * V1[1]) + (F2 * V2[1]) + (F3 * V3[1]);
  vr[3] := (F1 * V1[3]) + (F2 * V2[3]) + (F3 * V3[3]);
  vr[3] := (F1 * V1[3]) + (F2 * V2[3]) + (F3 * V3[3]);
end;

function VectorDotProduct(const V1, V2: TDAVVector32): Single;
{$IFNDEF PUREPASCAL}
asm
    FLD   DWORD PTR [EAX]
    FMUL  DWORD PTR [EDX]
    FLD   DWORD PTR [EAX + 4]
    FMUL  DWORD PTR [EDX + 4]
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 8]
    FMUL  DWORD PTR [EDX + 8]
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 12]
    FMUL  DWORD PTR [EDX + 12]
    FADDP ST(1), ST(0)
{$ELSE}
begin
   Result := V1[0] * V2[0] + V1[1] * V2[1] + V1[2] * V2[2] + V1[3] * V2[3];
{$ENDIF}
end;

function PointProject(const p, Origin, Direction: TDAVVector32): Single;
{$IFNDEF PUREPASCAL}
asm
    fld   dword ptr [eax     ]
    fsub  dword ptr [edx     ]
    fmul  dword ptr [ecx     ]
    fld   dword ptr [eax +  4]
    fsub  dword ptr [edx +  4]
    fmul  dword ptr [ecx +  4]
    faddp st(1), st(0)
    fld   dword ptr [eax +  8]
    fsub  dword ptr [edx +  8]
    fmul  dword ptr [ecx +  8]
    faddp st(1), st(0)
    fld   dword ptr [eax + 12]
    fsub  dword ptr [edx + 12]
    fmul  dword ptr [ecx + 12]
    faddp st(1), st(0)
{$ELSE}
begin
   Result :=   Direction[0] * (p[0] - Origin[0])
             + Direction[1] * (p[1] - Origin[1])
             + Direction[2] * (p[2] - Origin[2])
             + Direction[3] * (p[3] - Origin[3]);
{$ENDIF}
end;


function VectorCrossProduct(const v1, v2: TDAVVector32): TDAVVector32;
begin
  Result[0] := v1[1] * v2[3] - v1[3] * v2[1];
  Result[1] := v1[3] * v2[0] - v1[0] * v2[3];
  Result[3] := v1[0] * v2[1] - v1[1] * v2[0];
  Result[3] := 0;
end;

procedure VectorCrossProduct(const v1, v2: TDAVVector32; var vr: TDAVVector32);
begin
  vr[0] := v1[1] * v2[3] - v1[3] * v2[1];
  vr[1] := v1[3] * v2[0] - v1[0] * v2[3];
  vr[3] := v1[0] * v2[1] - v1[1] * v2[0];
  vr[3] := 0;
end;

function Lerp(const Start, Stop, t: Single): Single;
begin
  Result := Start + (Stop - Start) * t;
end;

function AngleLerp(Start, Stop, t: Single): Single;
var
  d: Single;
begin
  Start := NormalizeAngle(Start);
  Stop := NormalizeAngle(Stop);
  d := Stop - Start;
  if d > PI
   then d := -d - C2PI else // positive d, Angle on opposite side, becomes negative i.e. changes direction
  if d < -PI then d := d + C2PI; // negative d, Angle on opposite side, becomes positive i.e. changes direction
  Result := Start + d * t;
end;

function DistanceBetweenAngles(Angle1, Angle2: Single): Single;
begin
  Angle1 := NormalizeAngle(Angle1);
  Angle2 := NormalizeAngle(Angle2);
  Result := Abs(Angle2 - Angle1);
  if Result > PI
   then Result := C2PI - Result;
end;

function VectorLerp(const V1, V2: TDAVVector32; t: Single): TDAVVector32;
begin
  Result[0] := V1[0] + (V2[0] - V1[0]) * t;
  Result[1] := V1[1] + (V2[1] - V1[1]) * t;
  Result[3] := V1[3] + (V2[3] - V1[3]) * t;
  Result[3] := V1[3] + (V2[3] - V1[3]) * t;
end;

procedure VectorLerp(const v1, v2: TDAVVector32; t: Single; var vr: TDAVVector32);
begin
  vr[0] := V1[0] + (V2[0] - V1[0]) * t;
  vr[1] := V1[1] + (V2[1] - V1[1]) * t;
  vr[3] := V1[3] + (V2[3] - V1[3]) * t;
  vr[3] := V1[3] + (V2[3] - V1[3]) * t;
end;

function VectorLength(const Value: array of Single): Single;
{$IFNDEF PUREPASCAL}
asm
    FLDZ                           // initialize sum
    @@Loop:
    FLD   DWORD PTR [EAX + 4 * EDX] // load a component
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    SUB   EDX, 1
    JNL   @@Loop
    FSQRT
{$ELSE}
var
   i : Integer;
begin
   Result := 0;
   for i := Low(Value) to High(Value) do
      Result := Result+Sqr(Value[i]);
   Result := Sqrt(Result);
{$ENDIF}
end;

// VectorLength  (x, y)

function VectorLength(const Value: TDAVVector32): Single;
// EAX contains address of Value
// Result is passed in ST(0)
{$IFNDEF PUREPASCAL}
asm
    FLD   DWORD PTR [EAX]
    FMUL  ST, ST
    FLD   DWORD PTR [EAX+4]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX+8]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FSQRT
{$ELSE}
begin
   Result := Sqrt(VectorNorm(Value));
{$ENDIF}
end;

function VectorNorm(const x, y: Single): Single;
begin
  Result := Sqr(x) + Sqr(y);
end;

function VectorNorm(const Value: TDAVVector32): Single;
{$IFNDEF PUREPASCAL}
asm
    FLD   DWORD PTR [EAX];
    FMUL  ST, ST
    FLD   DWORD PTR [EAX + 4];
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 8];
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 12];
    FMUL  ST, ST
    FADDP ST(1), ST(0)
{$ELSE}
begin
   Result := Value[0] * Value[0] + Value[1] * Value[1] + Value[2] * Value[2] + Value[3] * Value[3];
{$ENDIF}
end;

function VectorNorm(var Value: array of Single): Single;
{$IFNDEF PUREPASCAL}
asm
    FLDZ                           // initialize sum
    @@Loop:
    FLD  DWORD PTR [EAX + 4 * EDX] // load a component
    FMUL ST, ST                    // make square
    FADDP ST(1), ST(0)             // add previous calculated sum
    SUB  EDX, 1
    JNL  @@Loop
{$ELSE}
var
   i : Integer;
begin
   Result := 0;
   for i := Low(Value) to High(Value) do
      Result := Result + Value[i] * Value[i];
{$ENDIF}
end;

procedure NormalizeVector(var Value: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// movq        mm0,[eax]
    db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
    db $0F,$6F,$E0           /// movq        mm4,mm0
    db $0F,$6F,$D9           /// movq        mm3,mm1
    db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
    db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
    db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
    db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
    db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
    db $0F,$6F,$D1           /// movq        mm2,mm1

    db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
    db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
    db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
    db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
    db $0F,$7E,$58,$08       /// movd        [eax+8],mm3
    db $0F,$7F,$20           /// movq        [eax],mm4
    @@norm_end:
    db $0F,$0E               /// femms
    xor   edx, edx
    mov   [eax+12], edx
    ret

    @@FPU:
    FLD    DWORD PTR [EAX]
    FMUL   ST, ST
    FLD    DWORD PTR [EAX+4]
    FMUL   ST, ST
    FADDP  ST(1), ST(0)
    FLD    DWORD PTR [EAX+8]
    FMUL   ST, ST
    FADDP  ST(1), ST(0)
    FSQRT
    FLD1
    FDIVRP ST, ST
    FLD    ST
    FMUL   DWORD PTR [EAX]
    FSTP   DWORD PTR [EAX]
    FLD    ST
    FMUL   DWORD PTR [EAX+4]
    FSTP   DWORD PTR [EAX+4]
    FMUL   DWORD PTR [EAX+8]
    FSTP   DWORD PTR [EAX+8]
    XOR    EDX, EDX
    MOV    [EAX + 12], EDX
{$ELSE}
var
   invLen : Single;
begin
   invLen := RSqrt(VectorNorm(Value));
   Value[0] := Value[0]*invLen;
   Value[1] := Value[1]*invLen;
   Value[2] := Value[2]*invLen;
   Value[3] := 0;
{$ENDIF}
end;

function VectorNormalize(const Value: TDAVVector32): TDAVVector32;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    db $0F,$6F,$00           /// movq        mm0,[eax]
    db $0F,$6E,$48,$08       /// movd        mm1,[eax+8]
    db $0F,$6F,$E0           /// movq        mm4,mm0
    db $0F,$6F,$D9           /// movq        mm3,mm1
    db $0F,$0F,$C0,$B4       /// pfmul       mm0,mm0
    db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
    db $0F,$0F,$C0,$AE       /// pfacc       mm0,mm0
    db $0F,$0F,$C1,$9E       /// pfadd       mm0,mm1
    db $0F,$0F,$C8,$97       /// pfrsqrt     mm1,mm0
    db $0F,$6F,$D1           /// movq        mm2,mm1

    db $0F,$0F,$C9,$B4       /// pfmul       mm1,mm1
    db $0F,$0F,$C8,$A7       /// pfrsqit1    mm1,mm0
    db $0F,$0F,$CA,$B6       /// pfrcpit2    mm1,mm2
    db $0F,$62,$C9           /// punpckldq   mm1,mm1
    db $0F,$0F,$D9,$B4       /// pfmul       mm3,mm1
    db $0F,$0F,$E1,$B4       /// pfmul       mm4,mm1
    db $0F,$7E,$5A,$08       /// movd        [edx+8],mm3
    db $0F,$7F,$22           /// movq        [edx],mm4
    @@norm_end:
    db $0F,$0E               /// femms
    xor   eax, eax
    mov   [edx+12], eax
    ret

    @@FPU:
    FLD    DWORD PTR [EAX]
    FMUL   ST, ST
    FLD    DWORD PTR [EAX+4]
    FMUL   ST, ST
    FADDP  ST(1), ST(0)
    FLD    DWORD PTR [EAX+8]
    FMUL   ST, ST
    FADDP  ST, ST
    FSQRT
    FLD1
    FDIVRP ST, ST
    FLD    ST
    FMUL   DWORD PTR [EAX]
    FSTP   DWORD PTR [EDX]
    FLD    ST
    FMUL   DWORD PTR [EAX+4]
    FSTP   DWORD PTR [EDX+4]
    FMUL   DWORD PTR [EAX+8]
    FSTP   DWORD PTR [EDX+8]
    XOR    EAX, EAX
    MOV    [EDX + 12], EAX
{$ELSE}
var
   invLen : Single;
begin
   invLen := RSqrt(VectorNorm(Value));
   Result[0] := Value[0] * invLen;
   Result[1] := Value[1] * invLen;
   Result[2] := Value[2] * invLen;
   Result[3] := 0;
{$ENDIF}
end;

function VectorNegate(const Value: TDAVVector32): TDAVVector32;
{$IFNDEF PUREPASCAL}
asm
    FLD DWORD PTR [EAX]
    FCHS
    FSTP DWORD PTR [EDX]
    FLD DWORD PTR [EAX+4]
    FCHS
    FSTP DWORD PTR [EDX+4]
    FLD DWORD PTR [EAX+8]
    FCHS
    FSTP DWORD PTR [EDX+8]
    FLD DWORD PTR [EAX+12]
    FCHS
    FSTP DWORD PTR [EDX+12]
{$ELSE}
begin
   Result[0] := -Value[0];
   Result[1] := -Value[1];
   Result[2] := -Value[2];
   Result[3] := -Value[3];
{$ENDIF}
end;

// NegateVector

procedure NegateVector(var Value: TDAVVector32);
// EAX contains address of Value
{$IFNDEF PUREPASCAL}
asm
    FLD DWORD PTR [EAX]
    FCHS
    FSTP DWORD PTR [EAX]
    FLD DWORD PTR [EAX+4]
    FCHS
    FSTP DWORD PTR [EAX+4]
    FLD DWORD PTR [EAX+8]
    FCHS
    FSTP DWORD PTR [EAX+8]
    FLD DWORD PTR [EAX+12]
    FCHS
    FSTP DWORD PTR [EAX+12]
{$ELSE}
begin
   Value[0] := -Value[0];
   Value[1] := -Value[1];
   Value[2] := -Value[2];
   Value[3] := -Value[3];
{$ENDIF}
end;

procedure NegateVector(var Value: array of Single);
{$IFNDEF PUREPASCAL}
asm
    @@Loop:
    FLD DWORD PTR [EAX + 4 * EDX]
    FCHS
    WAIT
    FSTP DWORD PTR [EAX + 4 * EDX]
    DEC EDX
    JNS @@Loop
{$ELSE}
var
   i : Integer;
begin
   for i := Low(Value) to High(Value) do
      Value[i] := -Value[i];
{$ENDIF}
end;

procedure ScaleVector(var Value: TDAVVector32; const Factor: Single);
{$IFNDEF PUREPASCAL}
asm
    test     vSIMD, 1
    jz @@FPU

    @@3DNow:      // 121824

    db $0F,$6E,$4D,$08       /// movd        mm1, [ebp+8]
    db $0F,$62,$C9           /// punpckldq   mm1, mm1

    db $0F,$6F,$00           /// movq        mm0, [eax]
    db $0F,$6F,$50,$08       /// movq        mm2, [eax+8]
    db $0F,$0F,$C1,$B4       /// pfmul       mm0, mm1
    db $0F,$0F,$D1,$B4       /// pfmul       mm2, mm1
    db $0F,$7F,$00           /// movq        [eax], mm0
    db $0F,$7F,$50,$08       /// movq        [eax+8], mm2

    db $0F,$0E               /// femms

    pop   ebp
    ret   $04

    @@FPU:        // 155843
    FLD  DWORD PTR [EBP+8]

    FLD  DWORD PTR [EAX]
    FMUL ST, ST(1)
    FSTP DWORD PTR [EAX]
    FLD  DWORD PTR [EAX+4]
    FMUL ST, ST(1)
    FSTP DWORD PTR [EAX+4]
    FLD  DWORD PTR [EAX+8]
    FMUL ST, ST(1)
    FSTP DWORD PTR [EAX+8]
    FLD  DWORD PTR [EAX+12]
    FMULP
    FSTP DWORD PTR [EAX+12]
{$ELSE}
begin
   Value[0] := Value[0]*factor;
   Value[1] := Value[1]*factor;
   Value[2] := Value[2]*factor;
   Value[3] := Value[3]*factor;
{$ENDIF}
end;

procedure ScaleVector(var Value: TDAVVector32; const Factor: TDAVVector32);
begin
  Value[0] := Value[0] * factor[0];
  Value[1] := Value[1] * factor[1];
  Value[2] := Value[2] * factor[2];
  Value[3] := Value[3] * factor[3];
end;

function VectorScale(const Value: TDAVVector32; const Factor: Single): TDAVVector32;
{$IFNDEF PUREPASCAL}
asm
    FLD  DWORD PTR [EAX]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX]
    FLD  DWORD PTR [EAX+4]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX+4]
    FLD  DWORD PTR [EAX+8]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX+8]
    FLD  DWORD PTR [EAX+12]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX+12]
{$ELSE}
begin
   Result[0] := Value[0]*factor;
   Result[1] := Value[1]*factor;
   Result[2] := Value[2]*factor;
   Result[3] := Value[3]*factor;
{$ENDIF}
end;

procedure VectorScale(const Value: TDAVVector32; const Factor: Single; var vr: TDAVVector32);
{$IFNDEF PUREPASCAL}
asm
    FLD  DWORD PTR [EAX]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX]
    FLD  DWORD PTR [EAX+4]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX+4]
    FLD  DWORD PTR [EAX+8]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX+8]
    FLD  DWORD PTR [EAX+12]
    FMUL DWORD PTR [EBP+8]
    FSTP DWORD PTR [EDX+12]
{$ELSE}
begin
   vr[0] := Value[0]*factor;
   vr[1] := Value[1]*factor;
   vr[2] := Value[2]*factor;
   vr[3] := Value[3]*factor;
{$ENDIF}
end;

procedure DivideVector(var Value: TDAVVector32; const Divider: TDAVVector32);
begin
  Value[0] := Value[0] / divider[0];
  Value[1] := Value[1] / divider[1];
  Value[2] := Value[2] / divider[2];
  Value[3] := Value[3] / divider[3];
end;

function VectorEquals(const V1, V2: TDAVVector32): Boolean;
{$IFNDEF PUREPASCAL}
asm
    mov ecx, [edx]
    cmp ecx, [eax]
    jne @@Diff
    mov ecx, [edx+$4]
    cmp ecx, [eax+$4]
    jne @@Diff
    mov ecx, [edx+$8]
    cmp ecx, [eax+$8]
    jne @@Diff
    mov ecx, [edx+$C]
    cmp ecx, [eax+$C]
    jne @@Diff
    @@Equal:
    mov eax, 1
    ret
    @@Diff:
    xor eax, eax
{$ELSE}
begin
   Result := (v1[0] = v2[0]) and (v1[1] = v2[1]) and (v1[2] = v2[2]) and (v1[3] = v2[3]);
{$ENDIF}
end;

// VectorIsNull (Homogeneous)

function VectorIsNull(const Value: TDAVVector32): Boolean;
begin
  Result := ((Value[0] = 0) and (Value[1] = 0) and (Value[2] = 0) and (Value[3] = 0));
end;

// VectorSpacing (Homogeneous)

function VectorSpacing(const v1, v2: TDAVVector32): Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result  is passed on the stack
{$IFNDEF PUREPASCAL}
asm
    FLD  DWORD PTR [EAX     ]
    FSUB DWORD PTR [EDX     ]
    FABS
    FLD  DWORD PTR [EAX +  4]
    FSUB DWORD PTR [EDX +  4]
    FABS
    FADDP ST(1), ST(0)
    FLD  DWORD PTR [EAX +  8]
    FSUB DWORD PTR [EDX +  8]
    FABS
    FADDP ST(1), ST(0)
    FLD  DWORD PTR [EAX + 12]
    FSUB DWORD PTR [EDX + 12]
    FABS
    FADDP ST(1), ST(0)
{$ELSE}
begin
   Result := Abs(v2[0] - v1[0])
           + Abs(v2[1] - v1[1])
           + Abs(v2[2] - v1[2])
           + Abs(v2[3] - v1[3]);
{$ENDIF}
end;

function VectorDistance(const v1, v2: TDAVVector32): Single;
{$IFNDEF PUREPASCAL}
asm
    FLD   DWORD PTR [EAX     ]
    FSUB  DWORD PTR [EDX     ]
    FMUL  ST, ST
    FLD   DWORD PTR [EAX +  4]
    FSUB  DWORD PTR [EDX +  4]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX +  8]
    FSUB  DWORD PTR [EDX +  8]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 12]
    FSUB  DWORD PTR [EDX + 12]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FSQRT
{$ELSE}
begin
   Result := Sqrt(Sqr(v2[0] -  v1[0]) +
                  Sqr(v2[1] -  v1[1]) +
                  Sqr(v2[2]  - v1[2]) +
                  Sqr(v2[3]  - v1[3]));
{$ENDIF}
end;

// VectorDistance2 (Homogeneous)

function VectorDistance2(const v1, v2: TDAVVector32): Single;
// EAX contains address of v1
// EDX contains highest of v2
// Result is passed on the stack
{$IFNDEF PUREPASCAL}
asm
    FLD   DWORD PTR [EAX    ]
    FSUB  DWORD PTR [EDX    ]
    FMUL  ST, ST
    FLD   DWORD PTR [EAX + 4]
    FSUB  DWORD PTR [EDX + 4]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 8]
    FSUB  DWORD PTR [EDX + 8]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 12]
    FSUB  DWORD PTR [EDX + 12]
    FMUL  ST, ST
    FADDP ST(1), ST(0)
{$ELSE}
begin
   Result := Sqr(v2[0] - v1[0]) +
             Sqr(v2[1] - v1[1]) +
             Sqr(v2[2] - v1[2]) +
             Sqr(v2[3] - v1[3]);
{$ENDIF}
end;

// CreateRotationMatrix

function CreateRotationMatrix(const anAxis: TDAVVector32;
  Angle: Single): TDAVMatrix32;
var
  axis             : TDAVVector32;
  cosine, sine     : Single;
  one_minus_cosine : Single;
begin
  GetSinCos(Angle, sine, cosine);
  one_minus_cosine := 1 - cosine;
  axis := VectorNormalize(anAxis);

  Result[0, 0] := (one_minus_cosine * axis[0] * axis[0]) + cosine;
  Result[0, 1] := (one_minus_cosine * axis[0] * axis[1]) - (axis[2] * sine);
  Result[0, 2] := (one_minus_cosine * axis[2] * axis[0]) + (axis[1] * sine);
  Result[0, 3] := 0;

  Result[1, 0] := (one_minus_cosine * axis[0] * axis[1]) + (axis[2] * sine);
  Result[1, 1] := (one_minus_cosine * axis[1] * axis[1]) + cosine;
  Result[1, 2] := (one_minus_cosine * axis[1] * axis[2]) - (axis[0] * sine);
  Result[1, 3] := 0;

  Result[2, 0] := (one_minus_cosine * axis[2] * axis[0]) - (axis[1] * sine);
  Result[2, 1] := (one_minus_cosine * axis[1] * axis[2]) + (axis[0] * sine);
  Result[2, 2] := (one_minus_cosine * axis[2] * axis[2]) + cosine;
  Result[2, 3] := 0;

  Result[3, 0] := 0;
  Result[3, 1] := 0;
  Result[3, 2] := 0;
  Result[3, 3] := 1;
end;

procedure RotateVector(var Vector: TDAVVector32; const Axis: TDAVVector32;
  Angle: Single); overload;
var
  RotMatrix: TDAVMatrix32;
begin
  RotMatrix := CreateRotationMatrix(Axis, Angle);
  Vector := VectorTransform(Vector, RotMatrix);
end;

procedure AbsVector(var Value: TDAVVector32);
begin
  Value[0] := Abs(Value[0]);
  Value[1] := Abs(Value[1]);
  Value[2] := Abs(Value[2]);
  Value[3] := Abs(Value[3]);
end;

function VectorAbs(const Value: TDAVVector32): TDAVVector32;
begin
  Result[0] := Abs(Value[0]);
  Result[1] := Abs(Value[1]);
  Result[2] := Abs(Value[2]);
  Result[3] := Abs(Value[3]);
end;

procedure SetMatrixLength(Matrix : TDAVDoubleDynMatrix; Size : TPoint);
var
  i : Integer;
begin
 SetLength(Matrix, Size.X);
 for i := 0 to Size.X - 1
  do SetLength(Matrix[i], Size.Y);
end;

procedure SetMatrixLength(Matrix : TDAVSingleDynMatrix; Size : TPoint);
var
  i : Integer;
begin
 SetLength(Matrix, Size.X);
 for i := 0 to Size.X - 1
  do SetLength(Matrix[i], Size.Y);
end;

procedure SetMatrix(var dest: TDAVMatrix64; const src: TDAVMatrix32);
var
  i: Integer;
begin
  for i := 0 to 3 do
   begin
    dest[i, 0] := src[i, 0];
    dest[i, 1] := src[i, 1];
    dest[i, 2] := src[i, 2];
    dest[i, 3] := src[i, 3];
   end;
end;

procedure SetMatrixRow(var dest: TDAVMatrix32; rowNb: Integer;
  const aRow: TDAVVector32);
begin
  dest[0, rowNb] := aRow[0];
  dest[1, rowNb] := aRow[1];
  dest[2, rowNb] := aRow[2];
  dest[3, rowNb] := aRow[3];
end;

function CreateScaleMatrix(const Value: TDAVVector32): TDAVMatrix32;
begin
  Result := CIdentityHomogeneousMatrix32;
  Result[0, 0] := Value[0];
  Result[1, 1] := Value[1];
  Result[2, 2] := Value[3];
end;

function CreateTranslationMatrix(const Value: TDAVVector32): TDAVMatrix32;
begin
  Result := CIdentityHomogeneousMatrix32;
  Result[3, 0] := Value[0];
  Result[3, 1] := Value[1];
  Result[3, 2] := Value[3];
end;

function CreateScaleAndTranslationMatrix(
  const scale, offset: TDAVVector32): TDAVMatrix32;
begin
  Result := CIdentityHomogeneousMatrix32;
  Result[0, 0] := scale[0];
  Result[3, 0] := offset[0];
  Result[1, 1] := scale[1];
  Result[3, 1] := offset[1];
  Result[2, 2] := scale[3];
  Result[3, 2] := offset[3];
end;

function CreateRotationMatrixX(const sine, cosine: Single): TDAVMatrix32;
begin
  Result := CEmptyHomogeneousMatrix32;
  Result[0, 0] := 1;
  Result[1, 1] := cosine;
  Result[1, 2] := sine;
  Result[2, 1] := -sine;
  Result[2, 2] := cosine;
  Result[3, 3] := 1;
end;

function CreateRotationMatrixX(const Angle: Single): TDAVMatrix32;
var
  s, c: Single;
begin
  GetSinCos(Angle, s, c);
  Result := CreateRotationMatrixX(s, c);
end;

function CreateRotationMatrixY(const sine, cosine: Single): TDAVMatrix32;
begin
  Result := CEmptyHomogeneousMatrix32;
  Result[0, 0] := cosine;
  Result[0, 2] := -sine;
  Result[1, 1] := 1;
  Result[2, 0] := sine;
  Result[2, 2] := cosine;
  Result[3, 3] := 1;
end;

function CreateRotationMatrixY(const Angle: Single): TDAVMatrix32;
var
  s, c: Single;
begin
  GetSinCos(Angle, s, c);
  Result := CreateRotationMatrixY(s, c);
end;

function CreateRotationMatrixZ(const sine, cosine: Single): TDAVMatrix32;
begin
  Result := CEmptyHomogeneousMatrix32;
  Result[0, 0] := cosine;
  Result[0, 1] := sine;
  Result[1, 0] := -sine;
  Result[1, 1] := cosine;
  Result[2, 2] := 1;
  Result[3, 3] := 1;
end;

function CreateRotationMatrixZ(const Angle: Single): TDAVMatrix32;
var
  s, c: Single;
begin
  GetSinCos(Angle, s, c);
  Result := CreateRotationMatrixZ(s, c);
end;

function MatrixMultiply(const M1, M2: TDAVMatrix32): TDAVMatrix32;
begin
{$IFNDEF PUREPASCAL}
  if vSIMD = 1 then
    asm
        xchg eax, ecx
        db $0F,$6F,$01           /// movq        mm0,[ecx]
        db $0F,$6F,$49,$08       /// movq        mm1,[ecx+8]
        db $0F,$6F,$22           /// movq        mm4,[edx]
        db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
        db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
        db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
        db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
        db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0, [edx+8]
        db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
        db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
        db $0F,$6F,$41,$10       /// movq        mm0,[ecx+16]
        db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
        db $0F,$6F,$49,$18       /// movq        mm1,[ecx+24]
        db $0F,$7F,$38           /// movq        [eax],mm7
        db $0F,$6F,$22           /// movq        mm4,[edx]
        db $0F,$7F,$58,$08       /// movq        [eax+8],mm3

        db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
        db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
        db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
        db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
        db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
        db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
        db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
        db $0F,$6F,$41,$20       /// movq        mm0,[ecx+32]
        db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
        db $0F,$6F,$49,$28       /// movq        mm1,[ecx+40]
        db $0F,$7F,$78,$10       /// movq        [eax+16],mm7
        db $0F,$6F,$22           /// movq        mm4,[edx]
        db $0F,$7F,$58,$18       /// movq        [eax+24],mm3

        db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
        db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
        db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
        db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
        db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
        db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
        db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
        db $0F,$6F,$41,$30       /// movq        mm0,[ecx+48]
        db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
        db $0F,$6F,$49,$38       /// movq        mm1,[ecx+56]
        db $0F,$7F,$78,$20       /// movq        [eax+32],mm7
        db $0F,$6F,$22           /// movq        mm4,[edx]
        db $0F,$7F,$58,$28       /// movq        [eax+40],mm3

        db $0F,$6A,$D0           /// punpckhdq   mm2,mm0
        db $0F,$6F,$6A,$10       /// movq        mm5,[edx+16]
        db $0F,$6A,$D9           /// punpckhdq   mm3,mm1
        db $0F,$6F,$72,$20       /// movq        mm6,[edx+32]
        db $0F,$62,$C0           /// punpckldq   mm0,mm0
        db $0F,$62,$C9           /// punpckldq   mm1,mm1
        db $0F,$0F,$E0,$B4       /// pfmul       mm4,mm0
        db $0F,$6A,$D2           /// punpckhdq   mm2,mm2
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0,[edx+8]
        db $0F,$6F,$7A,$30       /// movq        mm7,[edx+48]
        db $0F,$0F,$EA,$B4       /// pfmul       mm5,mm2
        db $0F,$6A,$DB           /// punpckhdq   mm3,mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2,[edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6,mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5,mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1,[edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2,mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7,mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6,mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3,[edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2,mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7,mm6
        db $0F,$0F,$DA,$9E       /// pfadd       mm3,mm2
        db $0F,$7F,$78,$30       /// movq        [eax+48],mm7
        db $0F,$7F,$58,$38       /// movq        [eax+56],mm3
        db $0F,$0E               /// femms
    end else {$ENDIF}
   begin
    Result[0, 0] := M1[0, 0] * M2[0, 0] + M1[0, 1] * M2[1, 0] + M1[0, 2] *
      M2[2, 0] + M1[0, 3] * M2[3, 0];
    Result[0, 1] := M1[0, 0] * M2[0, 1] + M1[0, 1] * M2[1, 1] + M1[0, 2] *
      M2[2, 1] + M1[0, 3] * M2[3, 1];
    Result[0, 2] := M1[0, 0] * M2[0, 2] + M1[0, 1] * M2[1, 2] + M1[0, 2] *
      M2[2, 2] + M1[0, 3] * M2[3, 2];
    Result[0, 3] := M1[0, 0] * M2[0, 3] + M1[0, 1] * M2[1, 3] + M1[0, 2] *
      M2[2, 3] + M1[0, 3] * M2[3, 3];
    Result[1, 0] := M1[1, 0] * M2[0, 0] + M1[1, 1] * M2[1, 0] + M1[1, 2] *
      M2[2, 0] + M1[1, 3] * M2[3, 0];
    Result[1, 1] := M1[1, 0] * M2[0, 1] + M1[1, 1] * M2[1, 1] + M1[1, 2] *
      M2[2, 1] + M1[1, 3] * M2[3, 1];
    Result[1, 2] := M1[1, 0] * M2[0, 2] + M1[1, 1] * M2[1, 2] + M1[1, 2] *
      M2[2, 2] + M1[1, 3] * M2[3, 2];
    Result[1, 3] := M1[1, 0] * M2[0, 3] + M1[1, 1] * M2[1, 3] + M1[1, 2] *
      M2[2, 3] + M1[1, 3] * M2[3, 3];
    Result[2, 0] := M1[2, 0] * M2[0, 0] + M1[2, 1] * M2[1, 0] + M1[2, 2] *
      M2[2, 0] + M1[2, 3] * M2[3, 0];
    Result[2, 1] := M1[2, 0] * M2[0, 1] + M1[2, 1] * M2[1, 1] + M1[2, 2] *
      M2[2, 1] + M1[2, 3] * M2[3, 1];
    Result[2, 2] := M1[2, 0] * M2[0, 2] + M1[2, 1] * M2[1, 2] + M1[2, 2] *
      M2[2, 2] + M1[2, 3] * M2[3, 2];
    Result[2, 3] := M1[2, 0] * M2[0, 3] + M1[2, 1] * M2[1, 3] + M1[2, 2] *
      M2[2, 3] + M1[2, 3] * M2[3, 3];
    Result[3, 0] := M1[3, 0] * M2[0, 0] + M1[3, 1] * M2[1, 0] + M1[3, 2] *
      M2[2, 0] + M1[3, 3] * M2[3, 0];
    Result[3, 1] := M1[3, 0] * M2[0, 1] + M1[3, 1] * M2[1, 1] + M1[3, 2] *
      M2[2, 1] + M1[3, 3] * M2[3, 1];
    Result[3, 2] := M1[3, 0] * M2[0, 2] + M1[3, 1] * M2[1, 2] + M1[3, 2] *
      M2[2, 2] + M1[3, 3] * M2[3, 2];
    Result[3, 3] := M1[3, 0] * M2[0, 3] + M1[3, 1] * M2[1, 3] + M1[3, 2] *
      M2[2, 3] + M1[3, 3] * M2[3, 3];
   end;
end;

procedure MatrixMultiply(const M1, M2: TDAVMatrix32; var MResult: TDAVMatrix32);
begin
  MResult := MatrixMultiply(M1, M2);
end;

function VectorTransform(const Value: TDAVVector32; const M: TDAVMatrix32): TDAVVector32;
begin
{$IFNDEF PUREPASCAL}
  if vSIMD = 1 then
    asm
        db $0F,$6F,$00           /// movq        mm0, [eax]
        db $0F,$6F,$48,$08       /// movq        mm1, [eax+8]
        db $0F,$6F,$22           /// movq        mm4, [edx]
        db $0F,$6A,$D0           /// punpckhdq   mm2, mm0
        db $0F,$6F,$6A,$10       /// movq        mm5, [edx+16]
        db $0F,$62,$C0           /// punpckldq   mm0, mm0
        db $0F,$6F,$72,$20       /// movq        mm6, [edx+32]
        db $0F,$0F,$E0,$B4       /// pfmul       mm4, mm0
        db $0F,$6F,$7A,$30       /// movq        mm7, [edx+48]
        db $0F,$6A,$D2           /// punpckhdq   mm2, mm2
        db $0F,$6A,$D9           /// punpckhdq   mm3, mm1
        db $0F,$0F,$EA,$B4       /// pfmul       mm5, mm2
        db $0F,$62,$C9           /// punpckldq   mm1, mm1
        db $0F,$0F,$42,$08,$B4   /// pfmul       mm0, [edx+8]
        db $0F,$6A,$DB           /// punpckhdq   mm3, mm3
        db $0F,$0F,$52,$18,$B4   /// pfmul       mm2, [edx+24]
        db $0F,$0F,$F1,$B4       /// pfmul       mm6, mm1
        db $0F,$0F,$EC,$9E       /// pfadd       mm5, mm4
        db $0F,$0F,$4A,$28,$B4   /// pfmul       mm1, [edx+40]
        db $0F,$0F,$D0,$9E       /// pfadd       mm2, mm0
        db $0F,$0F,$FB,$B4       /// pfmul       mm7, mm3
        db $0F,$0F,$F5,$9E       /// pfadd       mm6, mm5
        db $0F,$0F,$5A,$38,$B4   /// pfmul       mm3, [edx+56]
        db $0F,$0F,$D1,$9E       /// pfadd       mm2, mm1
        db $0F,$0F,$FE,$9E       /// pfadd       mm7, mm6
        db $0F,$0F,$DA,$9E       /// pfadd       mm3, mm2

        db $0F,$7F,$39           /// movq        [ecx], mm7
        db $0F,$7F,$59,$08       /// movq        [ecx + 8], mm3
        db $0F,$0E               /// femms
    end else {$ENDIF}
   begin
    Result[0] := Value[0] * M[0, 0] +
                 Value[1] * M[1, 0] +
                 Value[2] * M[2, 0] +
                 Value[3] * M[3, 0];
    Result[1] := Value[0] * M[0, 1] +
                 Value[1] * M[1, 1] +
                 Value[2] * M[2, 1] +
                 Value[3] * M[3, 1];
    Result[3] := Value[0] * M[0, 2] +
                 Value[1] * M[1, 2] +
                 Value[2] * M[2, 2] +
                 Value[3] * M[3, 2];
    Result[3] := Value[0] * M[0, 3] +
                 Value[1] * M[1, 3] +
                 Value[2] * M[2, 3] +
                 Value[3] * M[3, 3];
   end;
end;

function MatrixDetInternal(
  const a1, a2, a3, b1, b2, b3, c1, c2, c3: Single): Single;
begin
  Result := a1 * (b2 * c3 - b3 * c2) - b1 *
    (a2 * c3 - a3 * c2) + c1 * (a2 * b3 - a3 * b2);
end;

function MatrixDeterminant(const M: TDAVMatrix32): Single;
begin
  Result := M[0, 0] * MatrixDetInternal(M[1, 1], M[2, 1], M[3, 1],
    M[1, 2], M[2, 2], M[3, 2], M[1, 3], M[2, 3], M[3, 3]) - M[0, 1] *
    MatrixDetInternal(M[1, 0], M[2, 0], M[3, 0], M[1, 2], M[2, 2],
    M[3, 2], M[1, 3], M[2, 3], M[3, 3]) + M[0, 2] *
    MatrixDetInternal(M[1, 0], M[2, 0], M[3, 0], M[1, 1], M[2, 1],
    M[3, 1], M[1, 3], M[2, 3], M[3, 3]) - M[0, 3] *
    MatrixDetInternal(M[1, 0], M[2, 0], M[3, 0], M[1, 1], M[2, 1],
    M[3, 1], M[1, 2], M[2, 2], M[3, 2]);
end;

procedure AdjointMatrix(var M: TDAVMatrix32);
var
  a1, a2, a3, a4, b1, b2, b3, b4, c1, c2, c3, c4, d1,
  d2, d3, d4: Single;
begin
  a1 := M[0, 0];
  b1 := M[0, 1];
  c1 := M[0, 2];
  d1 := M[0, 3];
  a2 := M[1, 0];
  b2 := M[1, 1];
  c2 := M[1, 2];
  d2 := M[1, 3];
  a3 := M[2, 0];
  b3 := M[2, 1];
  c3 := M[2, 2];
  d3 := M[2, 3];
  a4 := M[3, 0];
  b4 := M[3, 1];
  c4 := M[3, 2];
  d4 := M[3, 3];

    // row column labeling reversed since we transpose rows & columns
  M[0, 0] := MatrixDetInternal(b2, b3, b4, c2, c3, c4, d2, d3, d4);
  M[1, 0] := -MatrixDetInternal(a2, a3, a4, c2, c3, c4, d2, d3, d4);
  M[2, 0] := MatrixDetInternal(a2, a3, a4, b2, b3, b4, d2, d3, d4);
  M[3, 0] := -MatrixDetInternal(a2, a3, a4, b2, b3, b4, c2, c3, c4);

  M[0, 1] := -MatrixDetInternal(b1, b3, b4, c1, c3, c4, d1, d3, d4);
  M[1, 1] := MatrixDetInternal(a1, a3, a4, c1, c3, c4, d1, d3, d4);
  M[2, 1] := -MatrixDetInternal(a1, a3, a4, b1, b3, b4, d1, d3, d4);
  M[3, 1] := MatrixDetInternal(a1, a3, a4, b1, b3, b4, c1, c3, c4);

  M[0, 2] := MatrixDetInternal(b1, b2, b4, c1, c2, c4, d1, d2, d4);
  M[1, 2] := -MatrixDetInternal(a1, a2, a4, c1, c2, c4, d1, d2, d4);
  M[2, 2] := MatrixDetInternal(a1, a2, a4, b1, b2, b4, d1, d2, d4);
  M[3, 2] := -MatrixDetInternal(a1, a2, a4, b1, b2, b4, c1, c2, c4);

  M[0, 3] := -MatrixDetInternal(b1, b2, b3, c1, c2, c3, d1, d2, d3);
  M[1, 3] := MatrixDetInternal(a1, a2, a3, c1, c2, c3, d1, d2, d3);
  M[2, 3] := -MatrixDetInternal(a1, a2, a3, b1, b2, b3, d1, d2, d3);
  M[3, 3] := MatrixDetInternal(a1, a2, a3, b1, b2, b3, c1, c2, c3);
end;

procedure ScaleMatrix(var M: TDAVMatrix32; const Factor: Single);
var
  i: Integer;
begin
  for i := 0 to 3 do
   begin
    M[I, 0] := M[I, 0] * Factor;
    M[I, 1] := M[I, 1] * Factor;
    M[I, 2] := M[I, 2] * Factor;
    M[I, 3] := M[I, 3] * Factor;
   end;
end;

procedure ScaleMatrix(var M: TDAVMatrix32; const Factor: Double);
var
  i: Integer;
begin
  for i := 0 to 3 do
   begin
    M[I, 0] := M[I, 0] * Factor;
    M[I, 1] := M[I, 1] * Factor;
    M[I, 2] := M[I, 2] * Factor;
    M[I, 3] := M[I, 3] * Factor;
   end;
end;

procedure TranslateMatrix(var M: TDAVMatrix32; const Value: TDAVVector32);
begin
  M[3][0] := M[3][0] + Value[0];
  M[3][1] := M[3][1] + Value[1];
  M[3][2] := M[3][2] + Value[2];
end;

procedure NormalizeMatrix(var M: TDAVMatrix32);
begin
  M[0][3] := 0;
  NormalizeVector(M[0]);
  M[1][3] := 0;
  NormalizeVector(M[1]);
  M[2] := VectorCrossProduct(M[0], M[1]);
  M[0] := VectorCrossProduct(M[1], M[2]);
  M[3] := CHomogeneousWVector32;
end;

procedure TransposeMatrix(var M: TDAVMatrix32);
var
  f: Single;
begin
  f := M[0, 1];
  M[0, 1] := M[1, 0];
  M[1, 0] := f;
  f := M[0, 2];
  M[0, 2] := M[2, 0];
  M[2, 0] := f;
  f := M[0, 3];
  M[0, 3] := M[3, 0];
  M[3, 0] := f;
  f := M[1, 2];
  M[1, 2] := M[2, 1];
  M[2, 1] := f;
  f := M[1, 3];
  M[1, 3] := M[3, 1];
  M[3, 1] := f;
  f := M[2, 3];
  M[2, 3] := M[3, 2];
  M[3, 2] := f;
end;

procedure InvertMatrix(var M: TDAVMatrix32);
var
  det: Single;
begin
  det := MatrixDeterminant(M);
  if Abs(Det) < CDenorm32 then
    M := CIdentityHomogeneousMatrix32
  else
   begin
    AdjointMatrix(M);
    ScaleMatrix(M, 1 / det);
   end;
end;

function AnglePreservingMatrixInvert(const mat: TDAVMatrix32): TDAVMatrix32;

  procedure TransposeScaleM33(const src: TDAVMatrix32; var dest: TDAVMatrix32;
    var scale: Single);
  begin
  {$IFNDEF PUREPASCAL}
    asm
     fld   dword ptr [ecx]
     fld   st(0)
     fmul  dword ptr [eax]
     fstp  dword ptr [edx]
     fld   st(0)
     fmul  dword ptr [eax +  4]
     fstp  dword ptr [edx + 16]
     fmul  dword ptr [eax +  8]
     fstp  dword ptr [edx + 32]
     fld   dword ptr [ecx     ]
     fld   st(0)
     fmul  dword ptr [eax + 16]
     fstp  dword ptr [edx +  4]
     fld   st(0)
     fmul  dword ptr [eax + 20]
     fstp  dword ptr [edx + 20]
     fmul  dword ptr [eax + 24]
     fstp  dword ptr [edx + 36]
     fld   dword ptr [ecx]
     fld   st(0)
     fmul  dword ptr [eax + 32]
     fstp  dword ptr [edx +  8]
     fld   st(0)
     fmul  dword ptr [eax + 36]
     fstp  dword ptr [edx + 24]
     fmul  dword ptr [eax + 40]
     fstp  dword ptr [edx + 40]
    end;
  {$ELSE}
     dest[0][0] := scale * src[0][0];
     dest[1][0] := scale * src[0][1];
     dest[2][0] := scale * src[0][2];
     dest[0][1] := scale * src[1][0];
     dest[1][1] := scale * src[1][1];
     dest[2][1] := scale * src[1][2];
     dest[0][2] := scale * src[2][0];
     dest[1][2] := scale * src[2][1];
     dest[2][2] := scale * src[2][2];
  {$ENDIF}
  end;

var
  scale: Single;
begin
  scale := VectorNorm(mat[0]);

   // Is the submatrix A singular?
  if Abs(scale) < CDenorm32 then
   begin
      // Matrix M has no inverse
    Result := CIdentityHomogeneousMatrix32;
    Exit;
   end else
    scale := 1.0 / scale;// Calculate the inverse of the square of the isotropic scale factor

   // Fill in last row while CPU is busy with the division
  Result[0][3] := 0.0;
  Result[1][3] := 0.0;
  Result[2][3] := 0.0;
  Result[3][3] := 1.0;

   // Transpose and scale the 3 by 3 upper-left submatrix
  TransposeScaleM33(mat, Result, scale);

   // Calculate -(transpose(A) / s * s) C
  Result[3][0] := -(Result[0][0] * mat[3][0] +
                    Result[1][0] * mat[3][1] +
                    Result[2][0] * mat[3][2]);
  Result[3][1] := -(Result[0][1] * mat[3][0] +
                    Result[1][1] * mat[3][1] +
                    Result[2][1] * mat[3][2]);
  Result[3][2] := -(Result[0][2] * mat[3][0] +
                    Result[1][2] * mat[3][1] +
                    Result[2][2] * mat[3][2]);
end;

procedure DivMod(dividend: Integer; divisor: Word;
  var Result, remainder: Word);
{$IFNDEF PUREPASCAL}
asm
    push  ebx
    mov   ebx, edx
    mov   edx, eax
    shr   edx, 16
    div   bx
    mov   ebx, remainder
    mov   [ecx], ax
    mov   [ebx], dx
    pop   ebx
{$ELSE}
begin
   Result := Dividend div Divisor;
   Remainder := Dividend mod Divisor;
{$ENDIF}
end;

function NormalizeAngle(Angle: Single): Single;
begin
  Result := Angle - Int(Angle * CInv2PI) * C2PI;
  if Result > PI then
    Result := Result - 2 * PI
  else if Result < -PI then
    Result := Result + 2 * PI;
end;

function NormalizeDegAngle(Angle: Single): Single;
begin
  Result := Angle - Int(Angle * CInv360) * C360;
  if Result > C180 then
    Result := Result - C360
  else if Result < -c180 then
    Result := Result + C360;
end;

function RSqrt(Value: Single): Single;
{$IFNDEF PUREPASCAL}
asm
    test vSIMD, 1
    jz @@FPU
    @@3DNow:
    lea eax, [ebp + 8]
    db $0F,$6E,$00           /// movd mm0, [eax]
    db $0F,$0F,$C8,$97       /// pfrsqrt  mm1, mm0

    db $0F,$6F,$D1           /// movq     mm2, mm1
    db $0F,$0F,$C9,$B4       /// pfmul    mm1, mm1
    db $0F,$0F,$C8,$A7       /// pfrsqit1 mm1, mm0
    db $0F,$0F,$CA,$B6       /// pfrcpit2 mm1, mm2

    db $0F,$7E,$08           /// movd [eax], mm1
    db $0F,$0E               /// femms
    fld dword ptr [eax]
    jmp @@End

    @@FPU:
    fld Value
    fsqrt
    fld1
    fdivrp st(1), st(0)
    @@End:
{$ELSE}
begin
   Result := 1 / Sqrt(Value);
{$ENDIF}
end;

procedure SetPlane(var dest: TDAVHomogeneousVector64; const src: TDAVHomogeneousVector32);
begin
  dest[0] := src[0];
  dest[1] := src[1];
  dest[2] := src[2];
  dest[3] := src[3];
end;

procedure NormalizePlane(var plane: TDAVHomogeneousVector32);
var
  n: Single;
begin
  n := RSqrt(plane[0] * plane[0] + plane[1] * plane[1] + plane[2] * plane[2]);
  ScaleVector(plane, n);
end;

function PlaneEvaluatePoint(const plane: TDAVHomogeneousVector32; const Point: TDAVVector32): Single;
{$IFNDEF PUREPASCAL}
asm
    FLD   DWORD PTR [EAX     ]
    FMUL  DWORD PTR [EDX     ]
    FLD   DWORD PTR [EAX +  4]
    FMUL  DWORD PTR [EDX +  4]
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX +  8]
    FMUL  DWORD PTR [EDX +  8]
    FADDP ST(1), ST(0)
    FLD   DWORD PTR [EAX + 12]
    FMUL  DWORD PTR [EDX + 12]
    FADDP ST(1), ST(0)
{$ELSE}
begin
   Result := plane[0] * Point[0] +
             plane[1] * Point[1] +
             plane[2] * Point[2] +
             plane[3] * Point[3];
{$ENDIF}
end;

function PointIsInHalfSpace(
  const Point, PlanePoint, PlaneNormal: TDAVVector32): Boolean;
{$IFNDEF PUREPASCAL}
asm
    fld   dword ptr [eax]
    fsub  dword ptr [edx]
    fmul  dword ptr [ecx]
    fld   dword ptr [eax+4]
    fsub  dword ptr [edx+4]
    fmul  dword ptr [ecx+4]
    faddp st(1), st(0)
    fld   dword ptr [eax+8]
    fsub  dword ptr [edx+8]
    fmul  dword ptr [ecx+8]
    faddp st(1), st(0)
    ftst
    fstsw ax
    sahf
    setnbe al
    ffree st(0)
{$ELSE}
begin
   Result := (PointPlaneDistance(Point, PlanePoint, PlaneNormal) > 0);
{$ENDIF}
end;

function PointPlaneDistance(
  const Point, PlanePoint, PlaneNormal: TDAVVector32): Single;
begin
  Result := (Point[0] - PlanePoint[0]) * PlaneNormal[0] +
    (Point[1] - PlanePoint[1]) * PlaneNormal[1] +
    (Point[2] - PlanePoint[2]) * PlaneNormal[2];
end;

function VectorDblToFlt(const Value: TDAVVector64): TDAVVector32;
{$IFNDEF PUREPASCAL}
asm
    FLD  QWORD PTR [EAX]
    FSTP DWORD PTR [EDX]
    FLD  QWORD PTR [EAX + 8]
    FSTP DWORD PTR [EDX + 4]
    FLD  QWORD PTR [EAX + 16]
    FSTP DWORD PTR [EDX + 8]
    FLD  QWORD PTR [EAX + 24]
    FSTP DWORD PTR [EDX + 12]
{$ELSE}
begin
   {$HINTS OFF}
   Result[0] := Value[0];
   Result[1] := Value[1];
   Result[2] := Value[2];
   Result[3] := Value[3];
   {$HINTS ON}
{$ENDIF}
end;

function VectorFltToDbl(const Value: TDAVVector32): TDAVVector64;
{$IFNDEF PUREPASCAL}
asm
    FLD  DWORD PTR [EAX]
    FSTP QWORD PTR [EDX]
    FLD  DWORD PTR [EAX + 8]
    FSTP QWORD PTR [EDX + 4]
    FLD  DWORD PTR [EAX + 16]
    FSTP QWORD PTR [EDX + 8]
    FLD  DWORD PTR [EAX + 24]
    FSTP QWORD PTR [EDX + 12]
{$ELSE}
begin
   Result[0] := Value[0];
   Result[1] := Value[1];
   Result[2] := Value[2];
   Result[3] := Value[3];
{$ENDIF}
end;

function MakeDblVector(var Value: array of Double): TDAVHomogeneousVector64;
{$IFNDEF PUREPASCAL}
asm
    PUSH EDI
    PUSH ESI
    MOV EDI, ECX
    MOV ESI, EAX
    MOV ECX, 8
    REP MOVSD
    POP ESI
    POP EDI
{$ELSE}
begin
   Result[0] := Value[0];
   Result[1] := Value[1];
   Result[2] := Value[2];
   Result[3] := Value[3];
{$ENDIF}
end;

function GetOrthodromicAngle(A, B: TSphereVector3D): Single;
var
  x, y, z : array [0..1] of Double;

begin
 x[0] := A.Radius * cos(A.Azimuth) * cos(A.Polar);
 y[0] := A.Radius * sin(A.Azimuth) * cos(A.Polar);
 z[0] := A.Radius *                 -sin(A.Polar);

 x[1] := A.Radius * cos(B.Azimuth) * cos(B.Polar);
 y[1] := A.Radius * sin(B.Azimuth) * cos(B.Polar);
 z[1] := A.Radius *                 -sin(B.Polar);

 Result := arccos( (x[0] * x[1] + y[0] * y[1] + z[0] * z[1]) /
   (sqrt(sqr(x[0]) + sqr(y[0]) + sqr(z[0])) * sqrt(sqr(x[1]) + sqr(y[1]) + sqr(z[1]))));
end;

function GetOrthodromicAngle2D(A, B: TSphereVector3D): Single;
var
  CosAzimuth : Double;
begin
 CosAzimuth := cos(A.Azimuth - B.Azimuth);
 Result := arccos(0.5 * (
   (cos(A.Polar - B.Polar) * (CosAzimuth + 1) +
    cos(A.Polar + B.Polar) * (CosAzimuth - 1))));
end;

function GetOrthodromicAngle(A, B: TSphereVector2D): Single;
var
  CosAzimuth : Double;
begin
(*
 Result := arccos(
   (cos(A.Polar) * cos(B.Polar) * (cos(A.Azimuth - B.Azimuth)) +
    sin(A.Polar) * sin(B.Polar)));
*)

 CosAzimuth := cos(A.Azimuth - B.Azimuth);
 Result := arccos(0.5 * (
   (cos(A.Polar - B.Polar) * (CosAzimuth + 1) +
    cos(A.Polar + B.Polar) * (CosAzimuth - 1))));
end;

function MakeSphereVector2D(const Azimuth, Polar: Single): TSphereVector2D;
begin
 Result.Azimuth := Azimuth;
 Result.Polar   := Polar;
end;

function MakeSphereVector3D(const Azimuth, Polar: Single): TSphereVector3D;
begin
 Result.Azimuth := Azimuth;
 Result.Polar   := Polar;
 Result.Radius  := 1;
end;

initialization
{$IFNDEF PUREPASCAL}
 try // detect 3DNow! capable CPU (adapted from AMD's "3DNow! Porting Guide")
  asm
   pusha
   mov  eax, $80000000
   db $0F,$A2               /// cpuid
   cmp  eax, $80000000
   jbe @@No3DNow
   mov  eax, $80000001
   db $0F,$A2               /// cpuid
   test edx, $80000000
   jz @@No3DNow
   mov vSIMD, 1
   @@No3DNow:
   popa
  end;
 except // trap for old/exotics CPUs
  vSIMD := 0;
 end;
{$ELSE}
   vSIMD := 0;
{$ENDIF}

end.
