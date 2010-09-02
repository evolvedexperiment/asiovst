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
  TBlendRegister    = function(Foreground, Background: TPixel32): TPixel32;
  TBlendMemory      = procedure(Foreground: TPixel32; var Background: TPixel32);
  TBlendLine        = procedure(Source, Destination: PPixel32; Count: Cardinal);
  TCombineRegister  = function(Foreground, Background: TPixel32; Weight: Cardinal): TPixel32;
  TCombineMemory    = procedure(Foreground: TPixel32; var Background: TPixel32; Weight: Integer);
  TCombineLine      = procedure(Source, Destination: PPixel32; Count: Integer; Weight: Cardinal);


{ Function Pointers }

var
  BlendRegister    : TBlendRegister;
  BlendMemory      : TBlendMemory;
  BlendLine        : TBlendLine;
  CombineRegister  : TCombineRegister;
  CombineMemory    : TCombineMemory;
  CombineLine      : TCombineLine;
  EMMS             : procedure;



{ Binding Function Pointers }

var
  BindingBlendRegister    : TFunctionBinding;
  BindingBlendMemory      : TFunctionBinding;
  BindingBlendLine        : TFunctionBinding;
  BindingCombineRegister  : TFunctionBinding;
  BindingCombineMemory    : TFunctionBinding;
  BindingCombineLine      : TFunctionBinding;
  BindingEMMS             : TFunctionBinding;


{ Binding List }

var
  BindingBlend   : TFunctionBindingList;

function Lighten(C: TPixel32; Amount: Integer): TPixel32;

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
  CBias = $00800080;

function BlendRegisterNative(Foreground, Background: TPixel32): TPixel32;
{$IFDEF PUREPASCAL}
var
  AlphaForeground : PByteArray;
  AlphaBackground : PByteArray;
begin
 if Foreground.A = 0 then
  begin
   Result := Background;
   Exit;
  end;

 if Foreground.A = $FF then
  begin
   Result := Foreground;
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
 Result := Background;
{$ELSE}
asm
  // test foreground alpha = 255 ?
  CMP     EAX, $FF000000   // Foreground = 255 ? => Result = EAX
  JNC     @Done

  // test foreground alpha = 0 ?
  TEST    EAX, $FF000000   // Foreground = 0 ?   => Result = EDX
  JZ      @CopyPixel

  // get weight W = (foreground alpha) * M
  MOV     ECX, EAX         // ECX  <-  Fa Fr Fg Fb
  SHR     ECX, 24          // ECX  <-  00 00 00 Fa

  PUSH    EBX

  // P = W * F
  MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
  AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
  AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
  IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
  SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
  IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
  ADD     EAX, CBias
  AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
  SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
  OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
  XOR     ECX, $000000FF   // ECX  <-  1 - ECX
  MOV     EBX, EDX         // EBX  <-  Ba Br Bg Bb
  AND     EDX, $00FF00FF   // EDX  <-  00 Br 00 Bb
  AND     EBX, $FF00FF00   // EBX  <-  Ba 00 Bg 00
  IMUL    EDX, ECX         // EDX  <-  Qr ** Qb **
  SHR     EBX, 8           // EBX  <-  00 Ba 00 Bg
  IMUL    EBX, ECX         // EBX  <-  Qa ** Qg **
  ADD     EDX, CBias
  AND     EDX, $FF00FF00   // EDX  <-  Qr 00 Qb 00
  SHR     EDX, 8           // EDX  <-  00 Qr ** Qb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
  OR      EBX, EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
  ADD     EAX, EBX         // EAX  <-  Za Zr Zg Zb

  POP     EBX
  RET

@CopyPixel:
  MOV     EAX, EDX

@Done:     RET
{$ENDIF}
end;

procedure BlendMemoryNative(Foreground: TPixel32; var Background: TPixel32);
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
  // Test Fa = 0 ?
  TEST    EAX, $FF000000   // Fa = 0 ?   => do not write
  JZ      @Done

  // Get weight W = Fa * M
  MOV     ECX, EAX         // ECX  <-  Fa Fr Fg Fb
  SHR     ECX, 24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
  CMP     ECX, $FF
  JZ      @CopyPixel

  PUSH    EBX
  PUSH    ESI

  // P = W * F
  MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
  AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
  AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
  IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
  SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
  IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
  ADD     EAX, CBias
  AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
  SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
  OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
  MOV     ESI, [EDX]
  XOR     ECX, $000000FF   // ECX  <-  1 - ECX
  MOV     EBX, ESI         // EBX  <-  Ba Br Bg Bb
  AND     ESI, $00FF00FF   // ESI  <-  00 Br 00 Bb
  AND     EBX, $FF00FF00   // EBX  <-  Ba 00 Bg 00
  IMUL    ESI, ECX         // ESI  <-  Qr ** Qb **
  SHR     EBX, 8           // EBX  <-  00 Ba 00 Bg
  IMUL    EBX, ECX         // EBX  <-  Qa ** Qg **
  ADD     ESI, CBias
  AND     ESI, $FF00FF00   // ESI  <-  Qr 00 Qb 00
  SHR     ESI, 8           // ESI  <-  00 Qr ** Qb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
  OR      EBX, ESI         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
  ADD     EAX, EBX         // EAX  <-  Za Zr Zg Zb
  MOV     [EDX], EAX

  POP     ESI
  POP     EBX
  RET

@CopyPixel:
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
   BlendMemory(Source^, Destination^);
   Inc(Source);
   Inc(Destination);
   Dec(Count);
  end;
{$ELSE}
asm
  // test the counter for zero or negativity
  TEST    ECX,ECX
  JS      @Done

  PUSH    EBX
  PUSH    ESI
  PUSH    EDI

  MOV     ESI, EAX         // ESI <- Src
  MOV     EDI, EDX         // EDI <- Dst

@LoopStart:
  MOV     EAX, [ESI]
  TEST    EAX, $FF000000
  JZ      @NextPixel

  PUSH    ECX

  // Get weight W = Fa * M
  MOV     ECX, EAX         // ECX  <-  Fa Fr Fg Fb
  SHR     ECX, 24          // ECX  <-  00 00 00 Fa

  // Test Fa = 255 ?
  CMP     ECX, $FF
  JZ      @CopyPixel

  // P = W * F
  MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
  AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
  AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
  IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
  SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
  IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
  ADD     EAX, BiasPointer
  AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
  SHR     EAX, 8           // EAX  <-  00 Pr ** Pb
  ADD     EBX, BiasPointer
  AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
  OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * B
  MOV     EDX, [EDI]
  XOR     ECX, $000000FF   // ECX  <-  1 - ECX
  MOV     EBX, EDX         // EBX  <-  Ba Br Bg Bb
  AND     EDX, $00FF00FF   // ESI  <-  00 Br 00 Bb
  AND     EBX, $FF00FF00   // EBX  <-  Ba 00 Bg 00
  IMUL    EDX, ECX         // ESI  <-  Qr ** Qb **
  SHR     EBX, 8           // EBX  <-  00 Ba 00 Bg
  IMUL    EBX, ECX         // EBX  <-  Qa ** Qg **
  ADD     EDX, BiasPointer
  AND     EDX, $FF00FF00   // ESI  <-  Qr 00 Qb 00
  SHR     EDX, 8           // ESI  <-  00 Qr ** Qb
  ADD     EBX, BiasPointer
  AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
  OR      EBX, EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
  ADD     EAX, EBX         // EAX  <-  Za Zr Zg Zb
@CopyPixel:
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

function CombineRegisterNative(ForeGround, Background: TPixel32; Weight: Cardinal): TPixel32;
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
  // combine RGBA channels of colors X and Y with the weight of X given in W
  // Result Z = W * X + (1 - W) * Y (all channels are combined, including alpha)
  // EAX <- X
  // EDX <- Y
  // ECX <- W

  // W = 0 or $FF?
  JCXZ    @Copy            // CX = 0 ?  => Result := EDX
  CMP     ECX, $FF         // CX = $FF ?  => Result := EAX
  JE      @Done

  PUSH    EBX

  // P = W * X
  MOV     EBX, EAX         // EBX  <-  Xa Xr Xg Xb
  AND     EAX, $00FF00FF   // EAX  <-  00 Xr 00 Xb
  AND     EBX, $FF00FF00   // EBX  <-  Xa 00 Xg 00
  IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
  SHR     EBX, 8           // EBX  <-  00 Xa 00 Xg
  IMUL    EBX, ECX         // EBX  <-  Pa ** Pg **
  ADD     EAX, CBias
  AND     EAX, $FF00FF00   // EAX  <-  Pa 00 Pg 00
  SHR     EAX, 8           // EAX  <-  00 Pr 00 Pb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
  OR      EAX, EBX         // EAX  <-  Pa Pr Pg Pb

  // W = 1 - W; Q = W * Y
  XOR     ECX, $000000FF   // ECX  <-  1 - ECX
  MOV     EBX, EDX         // EBX  <-  Ya Yr Yg Yb
  AND     EDX, $00FF00FF   // EDX  <-  00 Yr 00 Yb
  AND     EBX, $FF00FF00   // EBX  <-  Ya 00 Yg 00
  IMUL    EDX, ECX         // EDX  <-  Qr ** Qb **
  SHR     EBX, 8           // EBX  <-  00 Ya 00 Yg
  IMUL    EBX, ECX         // EBX  <-  Qa ** Qg **
  ADD     EDX, CBias
  AND     EDX, $FF00FF00   // EDX  <-  Qr 00 Qb 00
  SHR     EDX, 8           // EDX  <-  00 Qr ** Qb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
  OR      EBX, EDX         // EBX  <-  Qa Qr Qg Qb

  // Z = P + Q (assuming no overflow at each byte)
  ADD     EAX, EBX         // EAX  <-  Za Zr Zg Zb

  POP     EBX
  RET

@Copy:
  MOV     EAX, EDX
@Done:
  RET
{$ENDIF}
end;

procedure CombineMemoryNative(ForeGround: TPixel32; var Background: TPixel32; Weight: Cardinal);
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
  // EAX <- F
  // [EDX] <- B
  // ECX <- W

  // Check W
  JCXZ    @Done            // W = 0 ?  => write nothing
  CMP     ECX, $FF         // W = 255? => write F
  JZ      @Copy

  PUSH    EBX
  PUSH    ESI

  // P = W * F
  MOV     EBX, EAX         // EBX  <-  Fa Fr Fg Fb
  AND     EAX, $00FF00FF   // EAX  <-  00 Fr 00 Fb
  AND     EBX, $FF00FF00   // EBX  <-  Fa 00 Fg 00
  IMUL    EAX, ECX         // EAX  <-  Pr ** Pb **
  SHR     EBX, 8           // EBX  <-  00 Fa 00 Fg
  IMUL    EBX, ECX         // EBX  <-  00 00 Pg **
  ADD     EAX, CBias
  AND     EAX, $FF00FF00   // EAX  <-  Pr 00 Pb 00
  SHR     EAX, 8           // EAX  <-  00 Pr 00 Pb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Pa 00 Pg 00
  OR      EAX, EBX         // EAX  <-  00 Pr Pg Pb

// W = 1 - W; Q = W * B
  MOV     ESI, [EDX]
  XOR     ECX, $000000FF   // ECX  <-  1 - ECX
  MOV     EBX, ESI         // EBX  <-  Ba Br Bg Bb
  AND     ESI, $00FF00FF   // ESI  <-  00 Br 00 Bb
  AND     EBX, $FF00FF00   // EBX  <-  Ba 00 Bg 00
  IMUL    ESI, ECX         // ESI  <-  Qr ** Qb **
  SHR     EBX, 8           // EBX  <-  00 Ba 00 Bg
  IMUL    EBX, ECX         // EBX  <-  Qa 00 Qg **
  ADD     ESI, CBias
  AND     ESI, $FF00FF00   // ESI  <-  Qr 00 Qb 00
  SHR     ESI, 8           // ESI  <-  00 Qr ** Qb
  ADD     EBX, CBias
  AND     EBX, $FF00FF00   // EBX  <-  Qa 00 Qg 00
  OR      EBX, ESI         // EBX  <-  00 Qr Qg Qb

// Z = P + Q (assuming no overflow at each byte)
  ADD     EAX, EBX         // EAX  <-  00 Zr Zg Zb

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
begin
 while Count > 0 do
  begin
   CombineMemory(Source^, Destination^, Weight);
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

function BlendRegisterMMX(Foreground, Background: TPixel32): TPixel32;
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

procedure BlendMemoryMMX(Foreground: TPixel32; var Background: TPixel32);
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

function CombineRegisterMMX(ForeGround, Background: TPixel32; Weight: TPixel32): TPixel32;
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

procedure CombineMemoryMMX(F: TPixel32; var B: TPixel32; W: TPixel32);
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

  CMP       EDX, $FF
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
  CALL      Move
  POP       EBX
end;


{ SSE2 }

function BlendRegisterSSE2(Foreground, Background: TPixel32): TPixel32;
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

procedure BlendMemorySSE2(Foreground: TPixel32; var Background: TPixel32);
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

function CombineRegisterSSE2(ForeGround, Background: TPixel32; Weight: TPixel32): TPixel32;
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

procedure CombineMemorySSE2(F: TPixel32; var B: TPixel32; W: TPixel32);
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

  CMP       EDX, $FF
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
  CALL      Move
  POP       EBX
end;


{$ENDIF}


{ Misc. }

function Lighten(C: TPixel32; Amount: Integer): TPixel32;
begin
 Result := C;
 Inc(Result.R, Amount);
 Inc(Result.G, Amount);
 Inc(Result.B, Amount);

 if Result.R > 255 then Result.R := 255 else if Result.R < 0 then Result.R := 0;
 if Result.G > 255 then Result.G := 255 else if Result.G < 0 then Result.G := 0;
 if Result.B > 255 then Result.B := 255 else if Result.B < 0 then Result.B := 0;
end;


{ Global Functions }

procedure CreateTables;
var
  I, J : Integer;
  L    : Longword;
  P    : ^Longword;
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
 BiasPointer := Pointer(Integer(AlphaPointer) + $80 * 4 * SizeOf(Cardinal));
 Assert(PCardinal(BiasPointer)^ = $00800080);
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
 BindingBlendRegister := TFunctionBinding.Create(
   @@BlendRegister, @BlendRegisterNative);
 BindingBlend.AddBinding(BindingBlendRegister);
 with BindingBlendRegister do
  begin
   Add(@BlendRegisterNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendRegisterMMX, [pfMMX]);
   Add(@BlendRegisterSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for blend memory
 BindingBlendMemory := TFunctionBinding.Create(
   @@BlendMemory, @BlendMemoryNative);
 BindingBlend.AddBinding(BindingBlendMemory);
 with BindingBlendMemory do
  begin
   Add(@BlendMemoryNative);
   {$IFNDEF PUREPASCAL}
   Add(@BlendMemoryMMX, [pfMMX]);
   Add(@BlendMemorySSE2, [pfSSE2]);
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
 BindingCombineRegister := TFunctionBinding.Create(
   @@CombineRegister, @CombineRegisterNative);
 BindingBlend.AddBinding(BindingCombineRegister);
 with BindingCombineRegister do
  begin
   Add(@CombineRegisterNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombineRegisterMMX, [pfMMX]);
   Add(@CombineRegisterSSE2, [pfSSE2]);
   {$ENDIF}
   RebindProcessorSpecific;
  end;

 // create function binding for combine memory
 BindingCombineMemory := TFunctionBinding.Create(
   @@CombineMemory, @CombineMemoryNative);
 BindingBlend.AddBinding(BindingCombineMemory);
 with BindingCombineMemory do
  begin
   Add(@CombineMemoryNative);
   {$IFNDEF PUREPASCAL}
   Add(@CombineMemoryMMX, [pfMMX]);
   Add(@CombineMemorySSE2, [pfSSE2]);
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
