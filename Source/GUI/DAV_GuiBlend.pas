unit DAV_GuiBlend;

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
//  The below code is still a placeholder! Parts of it may still contain      //
//  copyrighted code. It must be reviewed in detail, before it should be used //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2010        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I ..\DAV_Compiler.inc}
{-$DEFINE PUREPASCAL}

uses
  SysUtils, DAV_GuiCommon, DAV_Bindings;


{ Function Prototypes }

type
  TBlendMergePixel        = function(Foreground, Background: TPixel32): TPixel32;
  TBlendMergePixelInplace = procedure(Foreground: TPixel32; var Background: TPixel32);
  TBlendMergeLine         = procedure(Source, Destination: PPixel32; Count: Cardinal);
  TCombinePixel           = function(Foreground, Background: TPixel32; Weight: Cardinal): TPixel32;
  TCombinePixelInplace    = procedure(Foreground: TPixel32; var Background: TPixel32; Weight: Integer);
  TCombineLine            = procedure(Source, Destination: PPixel32; Count: Integer; Weight: Cardinal);


{ Function Pointers }

var
  BlendPixel          : TBlendMergePixel;
  BlendPixelInplace   : TBlendMergePixelInplace;
  BlendLine           : TBlendMergeLine;
  CombinePixel        : TCombinePixel;
  CombinePixelInplace : TCombinePixelInplace;
  CombineLine         : TCombineLine;
  EMMS                : procedure;
  MergePixel          : TBlendMergePixel;
  MergePixelInplace   : TBlendMergePixelInplace;
  MergeLine           : TBlendMergeLine;



{ Binding Function Pointers }

var
  BindingBlendPixel          : TFunctionBinding;
  BindingBlendPixelInplace   : TFunctionBinding;
  BindingBlendLine           : TFunctionBinding;
  BindingCombinePixel        : TFunctionBinding;
  BindingCombinePixelInplace : TFunctionBinding;
  BindingCombineLine         : TFunctionBinding;
  BindingEMMS                : TFunctionBinding;
  BindingMergePixel          : TFunctionBinding;
  BindingMergePixelInplace   : TFunctionBinding;
  BindingMergeLine           : TFunctionBinding;


{ Binding List }

var
  BindingBlend : TFunctionBindingList;

implementation

uses
  DAV_MemoryUtils;

var
  RcTable: array [Byte, Byte] of Byte;
  DivTable: array [Byte, Byte] of Byte;
  {$IFNDEF PUREPASCAL}
  BiasPointer: Pointer;
  AlphaPointer: Pointer;
  {$ENDIF}

const
  CBias = $00FE00FE; // $00800080

function BlendPixelNative(Foreground, Background: TPixel32): TPixel32;
{$IFDEF PUREPASCAL}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
begin
 if Foreground.A =   0 then Result := Background else
 if Foreground.A = $FF then Result := Foreground else
  with Background do
   begin
    AlphaForeground := @DivTable[Foreground.A];
    AlphaBackground := @DivTable[not Foreground.A];
    R := AlphaForeground[Foreground.R] + AlphaBackground[R];
    G := AlphaForeground[Foreground.G] + AlphaBackground[G];
    B := AlphaForeground[Foreground.B] + AlphaBackground[B];
    A := $FF;
    Result := Background;
   end;
{$ELSE}
asm
  CMP     EAX, $FF000000
  JNC     @Done

  TEST    EAX, $FF000000
  JZ      @CopyPixel

  MOV     ECX, EAX
  SHR     ECX, 24

  PUSH    EBX

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  XOR     ECX, $000000FF
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EAX, EBX
  OR      EAX, $FF000000

  POP     EBX
  RET

@CopyPixel:
  MOV     EAX, EDX
  OR      EAX, $FF000000

@Done:
  RET
{$ENDIF}
end;

procedure BlendPixelInplaceNative(Foreground: TPixel32; var Background: TPixel32);
{$IFDEF PUREPASCAL}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
begin
 if Foreground.A = 0 then Exit;

 if Foreground.A = $FF then
  begin
   Background := Foreground;
   Exit;
  end;

 with Background do
  begin
    AlphaForeground := @DivTable[Foreground.A];
    AlphaBackground := @DivTable[not Foreground.A];
    R := AlphaForeground[Foreground.R] + AlphaBackground[R];
    G := AlphaForeground[Foreground.G] + AlphaBackground[G];
    B := AlphaForeground[Foreground.B] + AlphaBackground[B];
  end;
{$ELSE}
asm
  TEST    EAX, $FF000000
  JZ      @Done

  MOV     ECX, EAX
  SHR     ECX, 24

  CMP     ECX, $FF
  JZ      @CopyPixel

  PUSH    EBX
  PUSH    ESI

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  MOV     ESI, [EDX]
  XOR     ECX, $000000FF
  MOV     EBX, ESI
  AND     ESI, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    ESI, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     ESI, CBias
  AND     ESI, $FF00FF00
  SHR     ESI, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, ESI

  ADD     EAX, EBX
  OR      EAX, $FF000000
  MOV     [EDX], EAX

  POP     ESI
  POP     EBX
  RET

@CopyPixel:
  OR      EAX, $FF000000
  MOV     [EDX], EAX

@Done:
  RET
{$ENDIF}
end;

procedure BlendLineNative(Source, Destination: PPixel32; Count: Integer);
{$IFDEF PUREPASCAL}
begin
 while Count > 0 do
  begin
   BlendPixelInplace(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
  TEST    ECX, ECX
  JS      @Done

  PUSH    EBX
  PUSH    ESI
  PUSH    EDI

  MOV     ESI, EAX
  MOV     EDI, EDX

@LoopStart:
  MOV     EAX, [ESI]
  TEST    EAX, $FF000000
  JZ      @NextPixel

  PUSH    ECX

  MOV     ECX, EAX
  SHR     ECX, 24

  CMP     ECX, $FF
  JZ      @CopyPixel

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  MOV     EDX, [EDI]
  XOR     ECX, $000000FF
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EAX, EBX
@CopyPixel:
  OR      EAX, $FF000000
  MOV     [EDI], EAX
  POP     ECX

@NextPixel:
  ADD     ESI, 4
  ADD     EDI, 4

  DEC     ECX
  JNZ     @LoopStart

  POP     EDI
  POP     ESI
  POP     EBX

@Done:
  RET
{$ENDIF}
end;

function CombinePixelNative(ForeGround, Background: TPixel32; Weight: Cardinal): TPixel32;
{$IFDEF PUREPASCAL}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
begin
 if Weight = 0 then
  begin
   Result := Background;
   Exit;
  end;

 if Weight >= $FF then
  begin
   Result := ForeGround;
   Exit;
  end;

 with ForeGround do
  begin
   AlphaForeground := @DivTable[Weight];
   AlphaBackground := @DivTable[255 - Weight];
   R := AlphaBackground[Background.R] + AlphaForeground[R];
   G := AlphaBackground[Background.G] + AlphaForeground[G];
   B := AlphaBackground[Background.B] + AlphaForeground[B];
   A := AlphaBackground[Background.A] + AlphaForeground[A];
  end;
 Result := ForeGround;
{$ELSE}
asm
  JCXZ    @Copy
  CMP     ECX, $FF
  JE      @Done

  PUSH    EBX

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  XOR     ECX, $000000FF
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EAX, EBX

  POP     EBX
  RET

@Copy:
  MOV     EAX, EDX
@Done:
  RET
{$ENDIF}
end;

procedure CombinePixelInplaceNative(ForeGround: TPixel32; var Background: TPixel32; Weight: Cardinal);
{$IFDEF PUREPASCAL}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
begin
 if Weight = 0
  then Exit;

 if Weight >= $FF then
  begin
   Background := ForeGround;
   Exit;
  end;

 with ForeGround do
  begin
   AlphaForeground := @DivTable[Weight];
   AlphaBackground := @DivTable[255 - Weight];
   R := AlphaBackground[Background.R] + AlphaForeground[R];
   G := AlphaBackground[Background.G] + AlphaForeground[G];
   B := AlphaBackground[Background.B] + AlphaForeground[B];
   A := AlphaBackground[Background.A] + AlphaForeground[A];
  end;
 Background := ForeGround;
{$ELSE}
asm
  JCXZ    @Done
  CMP     ECX, $FF
  JZ      @Copy

  PUSH    EBX
  PUSH    ESI

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX

  MOV     ESI, [EDX]
  XOR     ECX, $000000FF
  MOV     EBX, ESI
  AND     ESI, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    ESI, ECX
  SHR     EBX, 8
  IMUL    EBX, ECX
  ADD     ESI, CBias
  AND     ESI, $FF00FF00
  SHR     ESI, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, ESI

  ADD     EAX, EBX

  MOV     [EDX], EAX

  POP     ESI
  POP     EBX
@Done:
  RET

@Copy:
  MOV     [EDX], EAX
  RET
{$ENDIF}
end;

procedure CombineLineNative(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
{$IFDEF PUREPASCAL}
begin
 while Count > 0 do
  begin
   CombinePixelInplace(Source^, Destination^, Weight);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
  TEST      ECX, ECX
  JS        @Done

  PUSH      EBX
  MOV       EBX, Weight

  TEST      EBX, EBX
  JZ        @LoopEnd        // weight is zero

  CMP       EBX, $FF
  JZ        @DoneMove       // weight = 255  =>  copy src to dst

  PUSH      EDI
  PUSH      ESI

@LoopStart:
  PUSH      EAX
  MOV       EDI, [EAX]
  AND       EAX, $00FF00FF
  AND       EDI, $FF00FF00
  IMUL      EAX, EBX
  SHR       EDI, 8
  IMUL      EDI, EBX
  ADD       EAX, CBias
  AND       EAX, $FF00FF00
  SHR       EAX, 8
  ADD       EDI, CBias
  AND       EDI, $FF00FF00
  OR        EAX, EDI

  MOV       ESI, [EDX]
  XOR       EBX, $000000FF
  MOV       EDI, ESI
  AND       ESI, $00FF00FF
  AND       EDI, $FF00FF00
  IMUL      ESI, EBX
  SHR       EDI, 8
  IMUL      EDI, EBX
  ADD       ESI, CBias
  AND       ESI, $FF00FF00
  SHR       ESI, 8
  ADD       EDI, CBias
  AND       EDI, $FF00FF00
  OR        EDI, ESI

  ADD       EAX, EDI

  MOV       [EDX], EAX

  POP       EAX
  ADD       EAX, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart

@LoopEnd:
  POP       ESI
  POP       EDI

  POP       EBX
  POP       EBP
@Done:
  RET       $0004

@DoneMove:
  SHL       ECX, 2
  CALL      Move
  POP       EBX
{$ENDIF}
end;
(*
end;
*)

procedure EMMSNative;
begin
 // dummy
end;


function MergePixelNative(Foreground, Background: TPixel32): TPixel32;
{$IFDEF PUREPASCAL}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
  AlphaResult     : PByteArray;
  X               : Integer;
begin
 if Foreground.A = $FF then Result := Foreground else
 if Foreground.A = $0  then Result := Background else
 if Background.A = $0  then Result := Foreground else
 if Background.A = $FF
  then Result := BlendPixel(Foreground, Background)
  else
   begin
    AlphaForeground := @DivTable[Foreground.A];
    AlphaBackground := @DivTable[Background.A];
    Result.A := Background.A + Foreground.A - AlphaBackground^[Foreground.A];
    AlphaResult := @RcTable[Result.A];

    // Red component
    Result.R := AlphaBackground[Background.R];
    X := Foreground.R - Result.R;
    if X >= 0
     then Result.R := AlphaResult[AlphaForeground[X] + Result.R]
     else Result.R := AlphaResult[Result.R - AlphaForeground[-X]];

    // Green component
    Result.G := AlphaBackground[Background.G];
    X := Foreground.G - Result.G;
    if X >= 0
     then Result.G := AlphaResult[AlphaForeground[X] + Result.G]
     else Result.G := AlphaResult[Result.G - AlphaForeground[-X]];

    // Blue component
    Result.B := AlphaBackground[Background.B];
    X := Foreground.B - Result.B;
    if X >= 0
     then Result.B := AlphaResult[AlphaForeground[X] + Result.B]
     else Result.B := AlphaResult[Result.B - AlphaForeground[-X]];
   end;
{$ELSE}
asm
  // EAX <- F
  // EDX <- B

  // if F.A = 0 then
  TEST    EAX, $FF000000
  JZ      @CopyPixel

  // else if B.A = 255 then
  CMP     EDX, $FF000000
  JNC     @Blend

  // else if F.A = 255 then
  CMP     EAX, $FF000000
  JNC     @Done

  // else if B.A = 0 then
  TEST    EDX, $FF000000
  JZ      @Done

@4:
  PUSH    EBX
  PUSH    ESI
  PUSH    EDI
  ADD     ESP, -$0C
  MOV     [ESP + $04], EDX
  MOV     [ESP], EAX

  // AH <- F.A
  // DL, CL <- B.A
  SHR     EAX, 16
  AND     EAX, $0000FF00
  SHR     EDX, 24
  MOV     CL, DL
  NOP
  NOP
  NOP

  // EDI <- PF
  // EDX <- PB
  // ESI <- PR

  // PF := @DivTable[F.A];
  LEA     EDI, [EAX + DivTable]
  // PB := @DivTable[B.A];
  SHL     EDX, $08
  LEA     EDX, [EDX + DivTable]

  // Result.A := B.A + F.A - PB[F.A];
  SHR     EAX, 8
  ADD     ECX, EAX
  SUB     ECX, [EDX + EAX]
  MOV     [ESP + $0B], CL

  // PR := @RcTable[Result.A];
  SHL     ECX, $08
  AND     ECX, $FFFF
  LEA     ESI, [ECX + RcTable]

  { Red component }

  // Result.R := PB[B.R];
  XOR     EAX, EAX
  MOV     AL, [ESP + $06]
  MOV     CL, [EDX + EAX]
  MOV     [ESP + $0A], CL
  // X := F.R - Result.R;
  MOV     AL, [ESP + $02]
  XOR     EBX, EBX
  MOV     BL, CL
  SUB     EAX, EBX

  // if X >= 0 then
  JL      @5
  // Result.R := PR[PF[X] + Result.R]
  MOVZX   EAX, BYTE PTR[EDI + EAX]
  AND     ECX, $FF
  ADD     EAX, ECX
  MOV     AL, [EDI + EAX]
  MOV     [ESP + $0A], AL
  JMP     @6
@5:
  // Result.R := PR[Result.R - PF[-X]];
  NEG     EAX
  MOVZX   EAX, BYTE PTR[EDI + EAX]
  XOR     ECX, ECX
  MOV     CL, [ESP + $0A]
  SUB     ECX, EAX
  MOV     AL, [ESI + ECX]
  MOV     [ESP + $0A], AL


  { Green component }

@6:
  // Result.G := PB[B.G];
  XOR     EAX, EAX
  MOV     AL, [ESP + $05]
  MOV     CL, [EDX + EAX]
  MOV     [ESP + $09], CL
  // X := F.G - Result.G;
  MOV     AL, [ESP + $01]
  XOR     EBX, EBX
  MOV     BL, CL
  SUB     EAX, EBX
  // if X >= 0 then
  JL @7
  // Result.G := PR[PF[X] + Result.G]
  MOVZX   EAX, BYTE PTR[EDI + EAX]
  AND     ECX, $FF
  ADD     EAX, ECX
  MOV     AL, [ESI + EAX]
  MOV     [ESP + $09], AL
  JMP     @8
@7:
  // Result.G := PR[Result.G - PF[-X]];
  NEG     EAX
  MOVZX   EAX, BYTE PTR[EDI + EAX]
  XOR     ECX, ECX
  MOV     CL, [ESP + $09]
  SUB     ECX, EAX
  MOV     AL, [ESI + ECX]
  MOV     [ESP + $09], AL


  { Blue component }

@8:
  // Result.B := PB[B.B];
  XOR     EAX, EAX
  MOV     AL, [ESP + $04]
  MOV     CL, [EDX + EAX]
  MOV     [ESP + $08], CL
  // X := F.B - Result.B;
  MOV     AL, [ESP]
  XOR     EDX, EDX
  MOV     DL, CL
  SUB     EAX, EDX
  // if X >= 0 then
  JL      @9
  // Result.B := PR[PF[X] + Result.B]
  MOVZX   EAX, BYTE PTR[EDI + EAX]
  XOR     EDX, EDX
  MOV     DL, CL
  ADD     EAX, EDX
  MOV     AL, [ESI + EAX]
  MOV     [ESP + $08], AL
  JMP     @10
@9:
  // Result.B := PR[Result.B - PF[-X]];
  NEG     EAX
  MOVZX   EAX, BYTE PTR[EDI + EAX]
  XOR     EDX, EDX
  MOV     DL, CL
  SUB     EDX, EAX
  MOV     AL, [ESI + EDX]
  MOV     [ESP + $08],AL

@10:
  // EAX <- Result
  MOV     EAX, [ESP + $08]

  // end;
  ADD     ESP, $0C
  POP     EDI
  POP     ESI
  POP     EBX
  RET

@Blend:
  CALL DWORD PTR [BlendPixel]
  OR   EAX, $FF000000
  RET
@CopyPixel:
  MOV EAX, EDX
@Done:
{$ENDIF}
end;

procedure MergePixelInplaceNative(Foreground: TPixel32; var Background: TPixel32);
begin
 Background := MergePixelNative(Foreground, Background);
end;

procedure MergeLineNative(Source, Destination: PPixel32; Count: Cardinal);
begin
 while Count > 0 do
  begin
   Destination^ := MergePixel(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
end;


{ MMX Functions }

{$IFNDEF PUREPASCAL}

procedure EMMSMMX;
asm
  EMMS
end;

function BlendPixelMMX(Foreground, Background: TPixel32): TPixel32;
asm
  MOVD      MM0, EAX
  PXOR      MM3, MM3
  MOVD      MM2, EDX
  PUNPCKLBW MM0, MM3
  MOV       ECX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [ECX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      EAX, MM2
end;

procedure BlendPixelInplaceMMX(Foreground: TPixel32; var Background: TPixel32);
asm
  TEST      EAX, $FF000000
  JZ        @Done
  CMP       EAX, $FF000000
  JNC       @Copy

  PXOR      MM3, MM3
  MOVD      MM0, EAX
  MOVD      MM2, [EDX]
  PUNPCKLBW MM0, MM3
  MOV       ECX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [ECX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      [EDX], MM2

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
end;

procedure BlendLineMMX(Source, Destination: PPixel32; Count: Integer);
asm
  TEST      ECX, ECX
  JS        @Done

  PUSH      ESI
  PUSH      EDI

  MOV       ESI, EAX
  MOV       EDI, EDX

@LoopStart:
  MOV       EAX, [ESI]
  TEST      EAX, $FF000000
  JZ        @NextPixel
  CMP       EAX, $FF000000
  JNC       @CopyPixel

  MOVD      MM0, EAX
  PXOR      MM3, MM3
  MOVD      MM2, [EDI]
  PUNPCKLBW MM0, MM3
  MOV       EAX, BiasPointer
  PUNPCKLBW MM2, MM3
  MOVQ      MM1, MM0
  PUNPCKHWD MM1, MM1
  PSUBW     MM0, MM2
  PUNPCKHDQ MM1, MM1
  PSLLW     MM2, 8
  PMULLW    MM0, MM1
  PADDW     MM2, [EAX]
  PADDW     MM2, MM0
  PSRLW     MM2, 8
  PACKUSWB  MM2, MM3
  MOVD      EAX, MM2

@CopyPixel:
  MOV       [EDI], EAX

@NextPixel:
  ADD       ESI, 4
  ADD       EDI, 4

  DEC       ECX
  JNZ       @LoopStart

  POP       EDI
  POP       ESI

@Done:
  RET
end;

function CombinePixelMMX(ForeGround, Background: TPixel32; Weight: TPixel32): TPixel32;
asm
  MOVD      MM1, EAX
  PXOR      MM0, MM0
  SHL       ECX, 4

  MOVD      MM2, EDX
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  ADD       ECX, AlphaPointer

  PSUBW     MM1, MM2
  PMULLW    MM1, [ECX]
  PSLLW     MM2, 8

  MOV       ECX, BiasPointer

  PADDW     MM2, [ECX]
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      EAX, MM1
end;

procedure CombinePixelInplaceMMX(F: TPixel32; var B: TPixel32; W: TPixel32);
asm
  JCXZ      @Done
  CMP       ECX, $FF
  JZ        @Copy

  MOVD      MM1, EAX
  PXOR      MM0, MM0

  SHL       ECX, 4

  MOVD      MM2, [EDX]
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  ADD       ECX, AlphaPointer

  PSUBW     MM1, MM2
  PMULLW    MM1, [ECX]
  PSLLW     MM2, 8

  MOV       ECX, BiasPointer

  PADDW     MM2, [ECX]
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      [EDX], MM1

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
end;

procedure CombineLineMMX(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
asm
  TEST      ECX, ECX
  JS        @Done

  PUSH      EBX
  MOV       EBX, Weight

  TEST      EBX, EBX
  JZ        @LoopEnd        // weight is zero

  CMP       EBX, $FF
  JZ        @DoneMove       // weight = 255  =>  copy src to dst

  SHL       EBX, 4
  ADD       EBX, AlphaPointer
  MOVQ      MM3, [EBX]
  MOV       EBX, BiasPointer
  MOVQ      MM4, [EBX]

@LoopStart:
  MOVD      MM1, [EAX]
  PXOR      MM0, MM0
  MOVD      MM2, [EDX]
  PUNPCKLBW MM1, MM0
  PUNPCKLBW MM2, MM0

  PSUBW     MM1, MM2
  PMULLW    MM1, MM3
  PSLLW     MM2, 8

  PADDW     MM2, MM4
  PADDW     MM1, MM2
  PSRLW     MM1, 8
  PACKUSWB  MM1, MM0
  MOVD      [EDX], MM1

  ADD       EAX, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart
@LoopEnd:
  POP       EBX
  POP       EBP
@Done:
  RET       $0004

@DoneMove:
  SHL       ECX, 2
  CALL      Move
  POP       EBX
end;


{ SSE2 }

function BlendPixelSSE2(Foreground, Background: TPixel32): TPixel32;
asm
  MOVD      XMM0, EAX
  PXOR      XMM3, XMM3
  MOVD      XMM2, EDX
  PUNPCKLBW XMM0, XMM3
  MOV       ECX, BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [ECX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      EAX, XMM2
end;

procedure BlendPixelInplaceSSE2(Foreground: TPixel32; var Background: TPixel32);
asm
  TEST      EAX, $FF000000
  JZ        @Done
  CMP       EAX, $FF000000
  JNC       @Copy

  PXOR      XMM3, XMM3
  MOVD      XMM0, EAX
  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM0, XMM3
  MOV       ECX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [ECX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      [EDX], XMM2

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
end;

procedure BlendLineSSE2(Source, Destination: PPixel32; Count: Integer);
asm
  TEST      ECX, ECX
  JS        @Done

  PUSH      ESI
  PUSH      EDI

  MOV       ESI, EAX
  MOV       EDI, EDX

@LoopStart:
  MOV       EAX, [ESI]
  TEST      EAX, $FF000000
  JZ        @NextPixel
  CMP       EAX, $FF000000
  JNC       @CopyPixel

  MOVD      XMM0, EAX
  PXOR      XMM3, XMM3
  MOVD      XMM2, [EDI]
  PUNPCKLBW XMM0, XMM3
  MOV       EAX,  BiasPointer
  PUNPCKLBW XMM2, XMM3
  MOVQ      XMM1, XMM0
  PUNPCKLBW XMM1, XMM3
  PUNPCKHWD XMM1, XMM1
  PSUBW     XMM0, XMM2
  PUNPCKHDQ XMM1, XMM1
  PSLLW     XMM2, 8
  PMULLW    XMM0, XMM1
  PADDW     XMM2, [EAX]
  PADDW     XMM2, XMM0
  PSRLW     XMM2, 8
  PACKUSWB  XMM2, XMM3
  MOVD      EAX,  XMM2

@CopyPixel:
  MOV       [EDI], EAX

@NextPixel:
  ADD       ESI, 4
  ADD       EDI, 4

  DEC       ECX
  JNZ       @LoopStart

  POP       EDI
  POP       ESI

@Done:
  RET
end;

function CombinePixelSSE2(ForeGround, Background: TPixel32; Weight: TPixel32): TPixel32;
asm
  MOVD      XMM1, EAX
  PXOR      XMM0, XMM0
  SHL       ECX, 4

  MOVD      XMM2, EDX
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  ADD       ECX, AlphaPointer

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, [ECX]
  PSLLW     XMM2, 8

  MOV       ECX, BiasPointer

  PADDW     XMM2, [ECX]
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      EAX, XMM1
end;

procedure CombinePixelInplaceSSE2(F: TPixel32; var B: TPixel32; W: TPixel32);
asm
  JCXZ      @Done
  CMP       ECX, $FF
  JZ        @Copy

  MOVD      XMM1, EAX
  PXOR      XMM0, XMM0

  SHL       ECX, 4

  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  ADD       ECX, AlphaPointer

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, [ECX]
  PSLLW     XMM2, 8

  MOV       ECX, BiasPointer

  PADDW     XMM2, [ECX]
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      [EDX], XMM1

@Done:
  RET

@Copy:
  MOV       [EDX], EAX
end;

procedure CombineLineSSE2(Source, Destination: PPixel32; Count: Integer;
  Weight: Cardinal);
asm
  TEST      ECX, ECX
  JS        @Done

  PUSH      EBX
  MOV       EBX, Weight

  TEST      EBX, EBX
  JZ        @LoopEnd

  CMP       EBX, $FF
  JZ        @DoneMove

  SHL       EBX, 4
  ADD       EBX, AlphaPointer
  MOVQ      XMM3, [EBX]
  MOV       EBX, BiasPointer
  MOVQ      XMM4, [EBX]

@LoopStart:
  MOVD      XMM1, [EAX]
  PXOR      XMM0, XMM0
  MOVD      XMM2, [EDX]
  PUNPCKLBW XMM1, XMM0
  PUNPCKLBW XMM2, XMM0

  PSUBW     XMM1, XMM2
  PMULLW    XMM1, XMM3
  PSLLW     XMM2, 8

  PADDW     XMM2, XMM4
  PADDW     XMM1, XMM2
  PSRLW     XMM1, 8
  PACKUSWB  XMM1, XMM0
  MOVD      [EDX], XMM1

  ADD       EAX, 4
  ADD       EDX, 4

  DEC       ECX
  JNZ       @LoopStart
@LoopEnd:
  POP       EBX
  POP       EBP
@Done:
  RET       $0004

@DoneMove:
  SHL       ECX, 2
  CALL      Move
  POP       EBX
end;


{$ENDIF}


{ Global Functions }

procedure CreateTables;
var
  I, J : Integer;
{$IFNDEF PUREPASCAL}
  L    : Longword;
  P    : ^Longword;
{$ENDIF}
const
  COne256th : Double = 1 / 255;
begin
 for J := 0 to 255 do
  for I := 0 to 255 do
   begin
    DivTable[I, J] := Round(I * J * COne256th);
    if I > 0
     then RcTable[I, J] := Round(J * 255 / I)
     else RcTable[I, J] := 0;
   end;

 {$IFNDEF PUREPASCAL}
 GetAlignedMemory(AlphaPointer, 256 * 4 * SizeOf(Cardinal));

 P := AlphaPointer;
 for I := 0 to 255 do
  begin
   L := I + I shl 16;
   P^ := L;
   Inc(P);
   P^ := L;
   Inc(P);
   P^ := L;
   Inc(P);
   P^ := L;
   Inc(P);
  end;
 BiasPointer := Pointer(Integer(AlphaPointer) + $FF * 4 * SizeOf(Cardinal));
 Assert(PCardinal(BiasPointer)^ = $00FF00FF);
 {$ENDIF}
end;

procedure FreeTables;
begin
 {$IFNDEF PUREPASCAL}
 BiasPointer := nil;
 FreeAlignedMemory(AlphaPointer);
 {$ENDIF}
end;

procedure BindFunctions;
begin
 // create function binding list for 32-bit float conversions
 BindingBlend := TFunctionBindingList.Create;

 // create function binding for EMMS procedure
 BindingEMMS := TFunctionBinding.Create(@@EMMS, @EMMSNative);
 BindingBlend.AddBinding(BindingEMMS);
 with BindingEMMS do
  begin
   Add(@EMMSNative);
   {$IFNDEF PUREPASCAL}
   Add(@EMMSMMX, [pfMMX]);
   Add(@EMMSNative, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend register
 BindingBlendPixel := TFunctionBinding.Create(
   @@BlendPixel, @BlendPixelNative);
 BindingBlend.AddBinding(BindingBlendPixel);
 with BindingBlendPixel do
  begin
   Add(@BlendPixelNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendPixelMMX, [pfMMX]);
   Add(@BlendPixelSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend memory
 BindingBlendPixelInplace := TFunctionBinding.Create(
   @@BlendPixelInplace, @BlendPixelInplaceNative);
 BindingBlend.AddBinding(BindingBlendPixelInplace);
 with BindingBlendPixelInplace do
  begin
   Add(@BlendPixelInplaceNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendPixelInplaceMMX, [pfMMX]);
   Add(@BlendPixelInplaceSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend line
 BindingBlendLine := TFunctionBinding.Create(
   @@BlendLine, @BlendLineNative);
 BindingBlend.AddBinding(BindingBlendLine);
 with BindingBlendLine do
  begin
   Add(@BlendLineNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendLineMMX, [pfMMX]);
   Add(@BlendLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine register
 BindingCombinePixel := TFunctionBinding.Create(
   @@CombinePixel, @CombinePixelNative);
 BindingBlend.AddBinding(BindingCombinePixel);
 with BindingCombinePixel do
  begin
   Add(@CombinePixelNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombinePixelMMX, [pfMMX]);
   Add(@CombinePixelSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine memory
 BindingCombinePixelInplace := TFunctionBinding.Create(
   @@CombinePixelInplace, @CombinePixelInplaceNative);
 BindingBlend.AddBinding(BindingCombinePixelInplace);
 with BindingCombinePixelInplace do
  begin
   Add(@CombinePixelInplaceNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombinePixelInplaceMMX, [pfMMX]);
   Add(@CombinePixelInplaceSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine line
 BindingCombineLine := TFunctionBinding.Create(
   @@CombineLine, @CombineLineNative);
 BindingBlend.AddBinding(BindingCombineLine);
 with BindingCombineLine do
  begin
   Add(@CombineLineNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombineLineMMX, [pfMMX]);
   Add(@CombineLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine register
 BindingMergePixel := TFunctionBinding.Create(
   @@MergePixel, @MergePixelNative);
 BindingBlend.AddBinding(BindingMergePixel);
 with BindingMergePixel do
  begin
   Add(@MergePixelNative);
   {$IFNDEF PUREPASCAL}
//   Add(@MergePixelMMX, [pfMMX]);
//   Add(@MergePixelSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for Merge memory
 BindingMergePixelInplace := TFunctionBinding.Create(
   @@MergePixelInplace, @MergePixelInplaceNative);
 BindingBlend.AddBinding(BindingMergePixelInplace);
 with BindingMergePixelInplace do
  begin
   Add(@MergePixelInplaceNative);
   {$IFNDEF PUREPASCAL}
//   Add(@MergePixelInplaceMMX, [pfMMX]);
//   Add(@MergePixelInplaceSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for Merge line
 BindingMergeLine := TFunctionBinding.Create(
   @@MergeLine, @MergeLineNative);
 BindingBlend.AddBinding(BindingMergeLine);
 with BindingMergeLine do
  begin
   Add(@MergeLineNative);
   {$IFNDEF PUREPASCAL}
//   Add(@MergeLineMMX, [pfMMX]);
//   Add(@MergeLineSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // processor specific rebind
 BindingBlend.RebindProcessorSpecific;
end;

procedure UnbindFunctions;
begin
 BindingBlend.Free;
 BindingBlend := nil;
end;

initialization
  CreateTables;
  BindFunctions;

finalization
  FreeTables;
  UnbindFunctions;

end.
