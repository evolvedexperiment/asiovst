unit DAV_BlockArithmetrics;

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

{$I DAV_Compiler.inc}

var
  BlockAdditionInplace32 : procedure(Destination, Source: PSingle; Count: Integer);
  BlockAdditionInplace64 : procedure(Destination, Source: PDouble; Count: Integer);

  BlockSubtractInplace32 : procedure(Destination, Source: PSingle; Count: Integer);
  BlockSubtractInplace64 : procedure(Destination, Source: PDouble; Count: Integer);

  BlockReverseSubtractInplace32 : procedure(Destination, Source: PSingle; Count: Integer);
  BlockReverseSubtractInplace64 : procedure(Destination, Source: PDouble; Count: Integer);

  BlockOffsetInplace32 : procedure(Destination: PSingle; Value: Single; Count: Integer);
  BlockOffsetInplace64 : procedure(Destination: PDouble; Value: Double; Count: Integer);

  BlockMultiplyInplace32 : procedure(Destination, Source: PSingle; Count: Integer);
  BlockMultiplyInplace64 : procedure(Destination, Source: PDouble; Count: Integer);

  BlockDivideInplace32 : procedure(Destination, Source: PSingle; Count: Integer);
  BlockDivideInplace64 : procedure(Destination, Source: PDouble; Count: Integer);

  BlockReverseDivideInplace32 : procedure(Destination, Source: PSingle; Count: Integer);
  BlockReverseDivideInplace64 : procedure(Destination, Source: PDouble; Count: Integer);

  BlockScaleInplace32 : procedure(Destination: PSingle; Value: Single; Count: Integer);
  BlockScaleInplace64 : procedure(Destination: PDouble; Value: Double; Count: Integer);

implementation

uses
  DAV_Bindings;

procedure BlockAdditionInplace32Native(Destination, Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ + Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD  [Destination + 4 * ecx - 4].Single
   FADD [Source      + 4 * ecx - 4].Single
   FSTP [Destination + 4 * ecx - 4].Single
 LOOP @start
{$ENDIF}
end;

procedure BlockAdditionInplace64Native(Destination, Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ + Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD  [Destination + 8 * ecx - 8].Double
   FADD [Source      + 8 * ecx - 8].Double
   FSTP [Destination + 8 * ecx - 8].Double
 LOOP @start
{$ENDIF}
end;

procedure BlockSubtractInplace32Native(Destination, Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ - Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD  [Destination + 4 * ecx - 4].Single
   FSUB [Source      + 4 * ecx - 4].Single
   FSTP [Destination + 4 * ecx - 4].Single
 LOOP @start
{$ENDIF}
end;

procedure BlockSubtractInplace64Native(Destination, Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ - Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD   [Destination + 8 * ecx - 8].Double
   FSUB  [Source      + 8 * ecx - 8].Double
   FSTP  [Destination + 8 * ecx - 8].Double
 LOOP    @start
{$ENDIF}
end;

procedure BlockReverseSubtractInplace32Native(Destination, Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^ - Destination^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @done

 @start:
 FLD    [Source      + 4 * ecx - 4].Single
 FSUB   [Destination + 4 * ecx - 4].Single
 FSTP   [Destination + 4 * ecx - 4].Single
 LOOP   @start

 @done:
{$ENDIF}
end;

procedure BlockReverseSubtractInplace64Native(Destination, Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^ - Destination^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @Done

 @Start:
 FLD     [Source      + 8 * ecx - 8].Double
 FSUB    [Destination + 8 * ecx - 8].Double
 FSTP    [Destination + 8 * ecx - 8].Double
 LOOP    @Start

 @Done:
 {$ENDIF}
end;

procedure BlockOffsetInplace32Native(Destination: PSingle; Value: Single; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ + Value;
   Inc(Destination);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @Done
 FLD     Value.Single

 @Start:
 FLD     [Destination + 4 * ecx - 4].Single
 FADD    ST(0), ST(1)
 FSTP    [Destination + 4 * ecx - 4].Single
 LOOP    @Start

 FSTP    ST(0)
 @Done:
{$ENDIF}
end;

procedure BlockOffsetInplace64Native(Destination: PDouble; Value: Double; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ + Value;
   Inc(Destination);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @Done
 FLD     Value.Double

 @start:
 FLD     [Destination + 8 * ecx - 8].Double
 FADD    ST(0), ST(1)
 FSTP    [Destination + 8 * ecx - 8].Double
 LOOP    @start

 FSTP    ST(0)
 @Done:
{$ENDIF}
end;

procedure BlockMultiplyInplace32Native(Destination, Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ * Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @done

 @start:
 FLD     [Destination + 4 * ecx - 4].Single
 FMUL    [Source      + 4 * ecx - 4].Single
 FSTP    [Destination + 4 * ecx - 4].Single
 LOOP    @start

 @done:
{$ENDIF}
end;

procedure BlockMultiplyInplace64Native(Destination, Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ * Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @done

 @start:
 FLD     [Destination + 8 * ecx - 8].Double
 FMUL    [Source      + 8 * ecx - 8].Double
 FSTP    [Destination + 8 * ecx - 8].Double
 LOOP    @start

 @done:
{$ENDIF}
end;

procedure BlockDivideInplace32Native(Destination, Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ / Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 AND     ECX, ECX
 JZ      @done

 @start:
 FLD     [Destination + 4 * ecx - 4].Single
 FDIV    [Source      + 4 * ecx - 4].Single
 FSTP    [Destination + 4 * ecx - 4].Single
 LOOP    @start

 @done:
{$ENDIF}
end;

procedure BlockDivideInplace64Native(Destination, Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ / Source^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD  [Destination + 8 * ecx - 8].Double
   FDIV [Source      + 8 * ecx - 8].Double
   FSTP [Destination + 8 * ecx - 8].Double
 LOOP @start
{$ENDIF}
end;

procedure BlockReverseDivideInplace32Native(Destination, Source: PSingle; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^ / Destination^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD  [Source      + 4 * ecx - 4].Single
   FDIV [Destination + 4 * ecx - 4].Single
   FSTP [Destination + 4 * ecx - 4].Single
 LOOP @start
{$ENDIF}
end;

procedure BlockReverseDivideInplace64Native(Destination, Source: PDouble; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Source^ / Destination^;
   Inc(Destination);
   Inc(Source);
  end;
{$ELSE}
asm
 @start:
   FLD  [Source      + 8 * ecx - 8].Double
   FDIV [Destination + 8 * ecx - 8].Double
   FSTP [Destination + 8 * ecx - 8].Double
 LOOP @start
{$ENDIF}
end;

procedure BlockScaleInplace32Native(Destination: PSingle; Value: Single; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ * Value;
   Inc(Destination);
  end;
{$ELSE}
asm
 AND    ECX, ECX
 JZ     @done
 FLD     Value.Double
 @start:
   FLD   [Destination + 4 * ecx - 4].Single
   FMUL  ST(0), ST(1)
   FSTP  [Destination + 4 * ecx - 4].Single
 LOOP    @start
 FSTP    ST(0)
 @done:
{$ENDIF}
end;

procedure BlockScaleInplace64Native(Destination: PDouble; Value: Double; Count: Integer);
{$IFDEF PUREPASCAL}
var
  Index: Integer;
begin
 for Index := Count - 1 downto 0 do
  begin
   Destination^ := Destination^ * Value;
   Inc(Destination);
  end;
{$ELSE}
asm
 AND    ECX, ECX
 JZ     @done
 FLD     Value.Double
 @start:
   FLD   [Destination + 8 * ecx - 8].Double
   FMUL  ST(0), ST(1)
   FSTP  [Destination + 8 * ecx - 8].Double
 LOOP    @start
 FSTP    ST(0)
 @done:
{$ENDIF}
end;

procedure BindFunctions;
begin
 // Block Inplace Addition Binding (32 bit)
 with TFunctionBinding.Create(@@BlockAdditionInplace32, @BlockAdditionInplace32Native) do
  begin
   Add(@BlockAdditionInplace32Native);
  end;

 // Block Inplace Addition Binding (64 bit)
 with TFunctionBinding.Create(@@BlockAdditionInplace64, @BlockAdditionInplace64Native) do
  begin
   Add(@BlockAdditionInplace64Native);
  end;

 // Block Inplace Subtraction Binding (32 bit)
 with TFunctionBinding.Create(@@BlockSubtractInplace32, @BlockSubtractInplace32Native) do
  begin
   Add(@BlockSubtractInplace32Native);
  end;

 // Block Inplace Subtraction Binding (64 bit)
 with TFunctionBinding.Create(@@BlockSubtractInplace64, @BlockSubtractInplace64Native) do
  begin
   Add(@BlockSubtractInplace64Native);
  end;

 // Block Inplace Reverse Subtraction Binding (32 bit)
 with TFunctionBinding.Create(@@BlockReverseSubtractInplace32, @BlockReverseSubtractInplace32Native) do
  begin
   Add(@BlockReverseSubtractInplace32Native);
  end;

 // Block Inplace Reverse Subtraction Binding (64 bit)
 with TFunctionBinding.Create(@@BlockReverseSubtractInplace64, @BlockReverseSubtractInplace64Native) do
  begin
   Add(@BlockReverseSubtractInplace64Native);
  end;

 // Block Inplace Offset Binding (32 bit)
 with TFunctionBinding.Create(@@BlockOffsetInplace32, @BlockOffsetInplace32Native) do
  begin
   Add(@BlockOffsetInplace32Native);
  end;

 // Block Inplace Offset Binding (64 bit)
 with TFunctionBinding.Create(@@BlockOffsetInplace64, @BlockOffsetInplace64Native) do
  begin
   Add(@BlockOffsetInplace64Native);
  end;

 // Block Inplace Multiply Binding (32 bit)
 with TFunctionBinding.Create(@@BlockMultiplyInplace32, @BlockMultiplyInplace32Native) do
  begin
   Add(@BlockMultiplyInplace32Native);
  end;

 // Block Inplace Multiply Binding (64 bit)
 with TFunctionBinding.Create(@@BlockMultiplyInplace64, @BlockMultiplyInplace64Native) do
  begin
   Add(@BlockMultiplyInplace64Native);
  end;

 // Block Inplace Divide Binding (32 bit)
 with TFunctionBinding.Create(@@BlockDivideInplace32, @BlockDivideInplace32Native) do
  begin
   Add(@BlockDivideInplace32Native);
  end;

 // Block Inplace Divide Binding (64 bit)
 with TFunctionBinding.Create(@@BlockDivideInplace64, @BlockDivideInplace64Native) do
  begin
   Add(@BlockDivideInplace64Native);
  end;

 // Block Inplace Reverse Divide Binding (32 bit)
 with TFunctionBinding.Create(@@BlockReverseDivideInplace32, @BlockReverseDivideInplace32Native) do
  begin
   Add(@BlockReverseDivideInplace32Native);
  end;

 // Block Inplace Reverse Divide Binding (64 bit)
 with TFunctionBinding.Create(@@BlockReverseDivideInplace64, @BlockReverseDivideInplace64Native) do
  begin
   Add(@BlockReverseDivideInplace64Native);
  end;

 // Block Inplace Scale Binding (32 bit)
 with TFunctionBinding.Create(@@BlockScaleInplace32, @BlockScaleInplace32Native) do
  begin
   Add(@BlockScaleInplace32Native);
  end;

 // Block Inplace Scale Binding (64 bit)
 with TFunctionBinding.Create(@@BlockScaleInplace64, @BlockScaleInplace64Native) do
  begin
   Add(@BlockScaleInplace64Native);
  end;

end;

initialization
  BindFunctions;

end.
