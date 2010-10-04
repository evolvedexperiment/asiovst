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
//  Several optimizations has been copied from the Graphics32 project which   //
//  is under the same license as this project.                                //
//  Please check the file GR32_Blend.pas at http://graphics32.org for         //
//  further copyright information!                                            //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
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
  TBlendMergePixelLine    = procedure(Foreground: TPixel32; Destination: PPixel32; Count: Cardinal);
  TBlendMergeLine         = procedure(Source, Destination: PPixel32; Count: Cardinal);
  TCombinePixel           = function(Foreground, Background: TPixel32; Weight: Cardinal): TPixel32;
  TCombinePixelInplace    = procedure(Foreground: TPixel32; var Background: TPixel32; Weight: Integer);
  TCombinePixelLine       = procedure(Foreground: TPixel32; Destination: PPixel32; Count: Integer; Weight: Cardinal);
  TCombineLine            = procedure(Source, Destination: PPixel32; Count: Integer; Weight: Cardinal);


{ Function Pointers }

var
  BlendPixel          : TBlendMergePixel;
  BlendPixelInplace   : TBlendMergePixelInplace;
  BlendPixelLine      : TBlendMergePixelLine;
  BlendLine           : TBlendMergeLine;
  CombinePixel        : TCombinePixel;
  CombinePixelInplace : TCombinePixelInplace;
  CombinePixelLine    : TCombinePixelLine;
  CombineLine         : TCombineLine;
  EMMS                : procedure;
  MergePixel          : TBlendMergePixel;
  MergePixelInplace   : TBlendMergePixelInplace;
  MergePixelLine      : TBlendMergePixelLine;
  MergeLine           : TBlendMergeLine;



{ Binding Function Pointers }

var
  BindingBlendPixel          : TFunctionBinding;
  BindingBlendPixelInplace   : TFunctionBinding;
  BindingBlendPixelLine      : TFunctionBinding;
  BindingBlendLine           : TFunctionBinding;
  BindingCombinePixel        : TFunctionBinding;
  BindingCombinePixelInplace : TFunctionBinding;
  BindingCombinePixelLine    : TFunctionBinding;
  BindingCombineLine         : TFunctionBinding;
  BindingEMMS                : TFunctionBinding;
  BindingMergePixel          : TFunctionBinding;
  BindingMergePixelInplace   : TFunctionBinding;
  BindingMergePixelLine      : TFunctionBinding;
  BindingMergeLine           : TFunctionBinding;


{ Binding List }

var
  BindingBlend : TFunctionBindingList;

implementation

uses
  DAV_Common, DAV_MemoryUtils;

{$IFNDEF PUREPASCAL}
var
  BiasPointer: Pointer;
  AlphaPointer: Pointer;
{$ENDIF}

const
  CBias = $00800080; // with this value the error is distributed equally

function BlendPixelNative(Foreground, Background: TPixel32): TPixel32;
{$IFDEF PUREPASCAL}
var
  Alpha : Byte;
begin
 if Foreground.A =   0 then Result := Background else
 if Foreground.A = $FF then Result := Foreground else
  begin
   Alpha := (ForeGround.ARGB shr 24);
   ForeGround.ARGB := ((((Alpha * (ForeGround.ARGB and $00FF00FF)) + CBias)
     and $FF00FF00) shr 8) or ((Alpha * ((ForeGround.ARGB and $FF00FF00)
     shr 8) + CBias) and $FF00FF00);

   Alpha := Alpha xor $FF;

   Background.ARGB := ((((Alpha * (Background.ARGB and $00FF00FF)) + CBias)
     and $FF00FF00) shr 8) or ((Alpha * ((Background.ARGB and $FF00FF00)
     shr 8) + CBias) and $FF00FF00);

   Result.ARGB := (Background.ARGB + Foreground.ARGB) or $FF000000;
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
  Alpha : Byte;
begin
 if Foreground.A = 0 then Exit;

 if Foreground.A = $FF then
  begin
   Background := Foreground;
   Exit;
  end;

 Alpha := (ForeGround.ARGB shr 24);
 ForeGround.ARGB := ((((Alpha * (ForeGround.ARGB and $00FF00FF)) + CBias)
   and $FF00FF00) shr 8) or ((Alpha * ((ForeGround.ARGB and $FF00FF00)
   shr 8) + CBias) and $FF00FF00);

 Alpha := Alpha xor $FF;

 Background.ARGB := ((((Alpha * (Background.ARGB and $00FF00FF)) + CBias)
   and $FF00FF00) shr 8) or ((Alpha * ((Background.ARGB and $FF00FF00)
   shr 8) + CBias) and $FF00FF00);

 Background.ARGB := (Background.ARGB + Foreground.ARGB) or $FF000000;
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

procedure BlendPixelLineNative(Foreground: TPixel32; Destination: PPixel32; Count: Integer);
{$IFDEF PUREPASCAL}
begin
 while Count > 0 do
  begin
   BlendPixelInplace(Foreground, Destination^);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
  TEST    ECX, ECX
  JS      @Done

  TEST    EAX, $FF000000
  JZ      @Done

  PUSH    EBX
  PUSH    ESI
  PUSH    EDI

  MOV     EDI, EDX

  MOV     ESI, EAX
  SHR     ESI, 24

  CMP     ESI, $FF
  JZ      @CopyPixel

  MOV     EBX, EAX
  AND     EAX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EAX, ESI
  SHR     EBX, 8
  IMUL    EBX, ESI
  ADD     EAX, CBias
  AND     EAX, $FF00FF00
  SHR     EAX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EAX, EBX
  XOR     ESI, $000000FF

@LoopStart:

  MOV     EDX, [EDI]
  MOV     EBX, EDX
  AND     EDX, $00FF00FF
  AND     EBX, $FF00FF00
  IMUL    EDX, ESI
  SHR     EBX, 8
  IMUL    EBX, ESI
  ADD     EDX, CBias
  AND     EDX, $FF00FF00
  SHR     EDX, 8
  ADD     EBX, CBias
  AND     EBX, $FF00FF00
  OR      EBX, EDX

  ADD     EBX, EAX

  OR      EBX, $FF000000
  MOV     [EDI], EBX

@NextPixel:
  ADD     EDI, 4

  DEC     ECX
  JNZ     @LoopStart

  POP     EDI
  POP     ESI
  POP     EBX

@Done:
  RET

@CopyPixel:
  MOV     [EDI], EAX
  ADD     EDI, 4

  DEC     ECX
  JNZ     @CopyPixel

  POP     EDI
  POP     ESI
  POP     EBX
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
  Temp            : TPixel32;
begin
 if Weight = 0 then Result := Background else
 if Weight >= $FF then Result := ForeGround else
  begin
   ForeGround.ARGB := (((Weight * ((ForeGround.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (ForeGround.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Weight := Weight xor $000000FF;
   Background.ARGB := (((Weight * ((Background.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (Background.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Result.ARGB := Background.ARGB + Foreground.ARGB;
  end;
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
  Temp            : TPixel32;
begin
 if Weight = 0 then Exit else
 if Weight >= $FF then Background := ForeGround else
  begin
   ForeGround.ARGB := (((Weight * ((ForeGround.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (ForeGround.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Weight := Weight xor $000000FF;
   Background.ARGB := (((Weight * ((Background.ARGB and $FF00FF00) shr 8)) +
     CBias) and $FF00FF00) + ((((Weight * (Background.ARGB and $00FF00FF)) +
     CBias) and $FF00FF00) shr 8);

   Background.ARGB := Background.ARGB + Foreground.ARGB;
  end;
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

procedure CombinePixelLineNative(Foreground: TPixel32; Destination: PPixel32;
  Count: Integer; Weight: Cardinal);
begin
 while Count > 0 do
  begin
   CombinePixelInplace(Foreground, Destination^, Weight);
   Inc(Destination);
   Dec(Count);
  end;
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

function MergePixelNative(Foreground, Background: TPixel32): TPixel32;
var
  Temp  : Integer;
  Scale : Integer;
begin
 if Foreground.A = $FF then Result := Foreground else
 if Foreground.A = $0  then Result := Background else
 if Background.A = $0  then Result := Foreground else
 if Background.A = $FF
  then Result := BlendPixel(Foreground, Background)
  else
   begin
    Temp := Sqr($FF) - (Foreground.A xor $FF) * (Background.A xor $FF);
    Result.A := (Temp + $80) shr 8;
    Scale := (Sqr($FF) * Foreground.A) div Temp;
    Result.R := Background.R + (Scale * ($FF + Foreground.R - Background.R) + $7F) shr 8 + 1 - Scale;
    Result.G := Background.G + (Scale * ($FF + Foreground.G - Background.G) + $7F) shr 8 + 1 - Scale;
    Result.B := Background.B + (Scale * ($FF + Foreground.B - Background.B) + $7F) shr 8 + 1 - Scale;
   end;
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

procedure EMMSNative;
begin
 // dummy
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

(*
function MergePixelMMX(Foreground, Background: TPixel32): TPixel32;
asm
  TEST      EAX, $FF000000  // foreground completely transparent =>
  JZ        @CopyPixel      // result = background
  CMP       EAX, $FF000000  // foreground completely opaque =>
  JNC       @Done           // result = foreground
  TEST      EDX, $FF000000  // background completely transparent =>
  JZ        @Done           // result = foreground

  PXOR      MM7, MM7        // MM7  <-  00 00 00 00 00 00 00 00
  MOVD      MM0, EAX        // MM0  <-  00 00 00 00 Fa Fr Fg Fb
  MOVD      MM1, EDX        // MM1  <-  00 00 00 00 Ba Br Bg Bb
  SHR       EAX, $18        // EAX  <-  00 00 00 Fa
  SHR       EDX, $18        // EDX  <-  00 00 00 Ba
  MOV       ECX, EDX        // ECX  <-  00 00 00 Ba
//  IMUL

  // Fa + Ba - Fa * Ba
  // Fa * (1 - Ba) + Ba

{
  ROR       EDX, 24         // EDX  <-  Br Bg Bb Ba
  MOVZX     ECX, DL         // ECX  <-  00 00 00 Ba
  PUNPCKLBW MM0, MM7        // MM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
  SUB       EAX, $FF        // EAX  <-  (Fa - 1)
  XOR       ECX, $FF        // ECX  <-  (1 - Ba)
  IMUL      ECX, EAX        // ECX  <-  (Fa - 1) * (1 - Ba)  =  Ra - 1
  IMUL      ECX, $8081      // ECX  <-  Xa 00 00 00
  ADD       ECX, $8081*$FF*$FF
  SHR       ECX, 15         // ECX  <-  Ra
  MOV       DL, CH          // EDX  <-  Br Bg Bb Ra
  ROR       EDX, 8          // EDX  <-  Ra Br Bg Bb
  MOVD      MM1, EDX        // MM1  <-  Ra Br Bg Bb
  PUNPCKLBW MM1, MM7        // MM1  <-  00 Ra 00 Br 00 Bg 00 Bb
  SHL       EAX, 20         // EAX  <-  Fa 00 00
  PSUBW     MM0, MM1        // MM0  <-  ** Da ** Dr ** Dg ** Db
  ADD       EAX, $0FF01000
  PSLLW     MM0, 4
  XOR       EDX, EDX        // EDX  <-  00
  DIV       EAX, ECX        // EAX  <-  Fa / Ra  =  Wa
  MOVD      MM4, EAX        // MM3  <-  Wa
//  PSHUFLW   MM4, MM4, $C0   // MM3  <-  00 00 ** Wa ** Wa ** Wa
  PMULHW    MM0, MM4        // MM0  <-  00 00 ** Pr ** Pg ** Pb
  PADDW     MM0, MM1        // MM0  <-  00 Ra 00 Rr 00 Rg 00 Rb
  PACKUSWB  MM0, MM7        // MM0  <-  Ra Rr Rg Rb
  MOVD      EAX, MM0
}

  RET

@CopyPixel:
  MOV       EAX, EDX

@Done:
end;
*)


{ SSE2 }

function BlendPixelSSE2(Foreground, Background: TPixel32): TPixel32;
asm
  MOVD      XMM0, EAX
  PXOR      XMM3, XMM3
  MOVD      XMM2, EDX
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

function MergePixelSSE2(Foreground, Background: TPixel32): TPixel32;
asm
  TEST      EAX, $FF000000  // foreground completely transparent =>
  JZ        @CopyPixel      // result = background
  CMP       EAX, $FF000000  // foreground completely opaque =>
  JNC       @Done           // result = foreground
  TEST      EDX, $FF000000  // background completely transparent =>
  JZ        @Done           // result = foreground

  PXOR      XMM7, XMM7      // XMM7  <-  00 00 00 00 00 00 00 00
  MOVD      XMM0, EAX       // XMM0  <-  00 00 00 00 Fa Fr Fg Fb
  SHR       EAX, 24         //  EAX  <-  00 00 00 Fa
  ROR       EDX, 24
  MOVZX     ECX, DL         //  ECX  <-  00 00 00 Ba
  PUNPCKLBW XMM0, XMM7      // XMM0  <-  00 Fa 00 Fr 00 Fg 00 Fb
  SUB       EAX, $FF        //  EAX  <-  (Fa - 1)
  XOR       ECX, $FF        //  ECX  <-  (1 - Ba)
  IMUL      ECX, EAX        //  ECX  <-  (Fa - 1) * (1 - Ba)  =  Ra - 1
  IMUL      ECX, $8081      //  ECX  <-  Xa 00 00 00
  ADD       ECX, $8081*$FF*$FF
  SHR       ECX, 15         //  ECX  <-  Ra
  MOV       DL, CH          //  EDX  <-  Br Bg Bb Ra
  ROR       EDX, 8          //  EDX  <-  Ra Br Bg Bb
  MOVD      XMM1, EDX       // XMM1  <-  Ra Br Bg Bb
  PUNPCKLBW XMM1, XMM7      // XMM1  <-  00 Ra 00 Br 00 Bg 00 Bb
  SHL       EAX, 20         //  EAX  <-  Fa 00 00
  PSUBW     XMM0, XMM1      // XMM0  <-  ** Da ** Dr ** Dg ** Db
  ADD       EAX, $0FF01000
  PSLLW     XMM0, 4
  XOR       EDX, EDX        //  EDX  <-  00
  DIV       EAX, ECX        //  EAX  <-  Fa / Ra  =  Wa
  MOVD      XMM4, EAX       // XMM3  <-  Wa
  PSHUFLW   XMM4, XMM4, $C0 // XMM3  <-  00 00 ** Wa ** Wa ** Wa
  PMULHW    XMM0, XMM4      // XMM0  <-  00 00 ** Pr ** Pg ** Pb
  PADDW     XMM0, XMM1      // XMM0  <-  00 Ra 00 Rr 00 Rg 00 Rb
  PACKUSWB  XMM0, XMM7      // XMM0  <-  Ra Rr Rg Rb
  MOVD      EAX, XMM0

  RET

@CopyPixel:
  MOV       EAX, EDX

@Done:
end;


{$ENDIF}


{ Global Functions }

procedure CreateTables;
{$IFNDEF PUREPASCAL}
var
  I : Integer;
  L : Longword;
  P : ^Longword;
{$ENDIF}
begin
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
 BindingBlendPixelLine := TFunctionBinding.Create(
   @@BlendPixelLine, @BlendPixelLineNative);
 BindingBlend.AddBinding(BindingBlendPixelLine);
 with BindingBlendPixelLine do
  begin
   Add(@BlendPixelLineNative);
   {$IFNDEF PUREPASCAL}
//   Add(@BlendPixelLineMMX, [pfMMX]);
//   Add(@BlendPixelLineSSE2, [pfSSE2]);
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

 // create function binding for combine memory
 BindingCombinePixelLine := TFunctionBinding.Create(
   @@CombinePixelLine, @CombinePixelLineNative);
 BindingBlend.AddBinding(BindingCombinePixelLine);
 with BindingCombinePixelLine do
  begin
   Add(@CombinePixelLineNative);
   {$IFNDEF PUREPASCAL}
//   Add(@CombinePixelLineMMX, [pfMMX]);
//   Add(@CombinePixelLineSSE2, [pfSSE2]);
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
