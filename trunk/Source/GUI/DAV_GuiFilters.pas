unit DAV_GuiFilters;

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

uses
  Graphics, Classes, SysUtils, DAV_Common, DAV_GuiCommon, DAV_GuiBlend,
  DAV_GuiPixelMap, DAV_GuiByteMap, DAV_DspFilter, DAV_DspFilterSimple,
  DAV_DspDelayLines;

type
  TGuiCustomFilter = class(TPersistent)
  public
    constructor Create; virtual; abstract;

    procedure Filter(ByteMap: TGuiCustomByteMap); overload; virtual; abstract;
    procedure Filter(PixelMap: TGuiCustomPixelMap); overload; virtual; abstract;
  end;

  TGuiCustomBlurFilter = class(TGuiCustomFilter)
  private
    FRadius           : Single;
    FRadiusReciprocal : Single;
    procedure SetRadius(const Value: Single);
  protected
    procedure RadiusChanged; virtual;
  public
    constructor Create; override;

    property Radius: Single read FRadius write SetRadius;
  end;

  TGuiBlurIIRFilter = class(TGuiCustomBlurFilter)
  private
    FIIRFilter : TFirstOrderLowpassFilter;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiBlurFractionalFIRFilter = class(TGuiCustomBlurFilter)
  private
    FDelayLine : TDelayLineFractional32;
  protected
    procedure RadiusChanged; override;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiBlurFIRFilter = class(TGuiCustomBlurFilter)
  private
    FBuffer       : PByteArray;
    FKernelSize   : Integer;
  protected
    procedure RadiusChanged; override;
    procedure ResizeBuffer; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure FilterHorizontal(Data: PByteArray; Count: Integer); overload; virtual;
    procedure FilterHorizontal(Data: PPixel32Array; Count: Integer); overload; virtual;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiStackBlurFilter = class(TGuiCustomBlurFilter)
  private
    FKernelSize : Integer;
  protected
    procedure RadiusChanged; override;
    procedure ResizeStack; virtual;

    property KernelSize: Integer read FKernelSize;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiSaturationFilter = class(TGuiCustomFilter)
  private
    FLookUpTable : array [Byte] of Byte;
    FValue       : Single;
    procedure SetValue(const Value: Single);
  protected
    procedure ValueChanged; virtual;
    procedure CalculateLookUpTable; virtual;
  public
    constructor Create; override;

    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;

    property Value: Single read FValue write SetValue;
  end;

  TGuiEmbossFilter = class(TGuiCustomFilter)
  public
    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

  TGuiInvertFilter = class(TGuiCustomFilter)
  public
    procedure Filter(ByteMap: TGuiCustomByteMap); override;
    procedure Filter(PixelMap: TGuiCustomPixelMap); override;
  end;

implementation

uses
  Math, DAV_Math;


{ TGuiCustomBlurFilter }

constructor TGuiCustomBlurFilter.Create;
begin
 inherited;
 FRadius := 1;
end;

procedure TGuiCustomBlurFilter.RadiusChanged;
begin
 FRadiusReciprocal := 1 / FRadius;
end;

procedure TGuiCustomBlurFilter.SetRadius(const Value: Single);
begin
 if FRadius <= 0
  then raise Exception.Create('Radius must be larger than zero!');

 if FRadius <> Value then
  begin
   FRadius := Value;
   RadiusChanged;
  end;
end;


{ TGuiBlurIIRFilter }

constructor TGuiBlurIIRFilter.Create;
begin
 FIIRFilter := TFirstOrderLowpassFilter.Create;
end;

destructor TGuiBlurIIRFilter.Destroy;
begin
 FreeAndNil(FIIRFilter);
 inherited;
end;

procedure TGuiBlurIIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y : Integer;
  Data : PByteArray;
begin
 with ByteMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := ScanLine[Y];
     FIIRFilter.Reset;
     for X := 0 to Width - 1
      do Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]));
     for X := Width - 1 downto 0
      do Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]));
    end;
  end;
end;

procedure TGuiBlurIIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  X, Y : Integer;
  Data : PPixel32Array;
begin
 raise Exception.Create('not yet implemented');

 with PixelMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := DataPointer;
     for X := 0 to Width - 1 do
      begin
//       Data^[X] := Round(FIIRFilter.ProcessSample32(Data^[X]);

      end;
    end;
  end;
end;


{ TGuiBlurFractionalFIRFilter }

constructor TGuiBlurFractionalFIRFilter.Create;
begin
 inherited;
 FDelayLine := TDelayLineFractional32.Create;
 FDelayLine.FractionalBufferSize := FRadius;
end;

destructor TGuiBlurFractionalFIRFilter.Destroy;
begin
 FreeAndNil(FDelayLine);
 inherited;
end;

procedure TGuiBlurFractionalFIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y    : Integer;
  Data    : PByteArray;
  Current : Single;
  Sum     : Single;
begin
 with ByteMap do
  begin
   for Y := 0 to Height - 1 do
    begin
     Data := ScanLine[Y];
     FDelayLine.ClearBuffer;
     Sum := 0;
     for X := 0 to Width - 1 do
      begin
       Current := Data^[X] * COne255th;
       Sum := Sum + Current;
       Current := FDelayLine.ProcessSample32(Current);
       Sum := Sum - Current;
       if Sum > 0
        then Data^[X] := Round($FF * Sum * FRadiusReciprocal)
        else Data^[X] := 0;
      end;
    end;

   Data := DataPointer;
   for X := 0 to Width - 1 do
    begin
     FDelayLine.ClearBuffer;
     Sum := 0;
     for Y := 0 to Height - 1 do
      begin
       Current := Data^[Y * Width + X] * COne255th;
       Sum := Sum + Current;
       Current := FDelayLine.ProcessSample32(Current);
       Sum := Sum - Current;
       if Sum > 0
        then Data^[Y * Width + X] := Round($FF * Sum * FRadiusReciprocal)
        else Data^[Y * Width + X] := 0;
      end;
    end;
  end;
end;

procedure TGuiBlurFractionalFIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
begin
 raise Exception.Create('not yet implemented');
end;

procedure TGuiBlurFractionalFIRFilter.RadiusChanged;
begin
 inherited;
 FDelayLine.FractionalBufferSize := FRadius;
end;


{ TGuiBlurFIRFilter }

constructor TGuiBlurFIRFilter.Create;
begin
 inherited;
 FKernelSize := 1;
 ResizeBuffer;
end;

destructor TGuiBlurFIRFilter.Destroy;
begin
 Dispose(FBuffer);
 inherited;
end;

procedure TGuiBlurFIRFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  X, Y    : Integer;
  Pos     : Integer;
  Data    : PByteArray;
  Sum     : Integer;
begin
 if FKernelSize <= 1 then Exit;

 with ByteMap do
  begin
   for Y := 0 to Height - 1
    do FilterHorizontal(ScanLine[Y], Width);

   Data := DataPointer;
   for X := 0 to Width - 1 do
    begin
     FillChar(FBuffer^, FKernelSize, 0);
     Pos := 0;
     Sum := 0;
     for Y := 0 to Height - 1 do
      begin
       Sum := Sum + Data^[Y * Width + X] - FBuffer^[Pos];
       FBuffer^[Pos] := Data^[Y * Width + X];
       Inc(Pos); if Pos >= FKernelSize then Pos := 0;
       Assert(Sum >= 0);
       Data^[Y * Width + X] := Sum div FKernelSize;
      end;
    end;
  end;
end;

procedure TGuiBlurFIRFilter.Filter(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiBlurFIRFilter.FilterHorizontal(Data: PPixel32Array; Count: Integer);
begin

end;

procedure TGuiBlurFIRFilter.FilterHorizontal(Data: PByteArray; Count: Integer);
var
  X   : Integer;
  Pos : Integer;
  Sum : Integer;
begin
 FillChar(FBuffer^, FKernelSize, 0);
 Pos := 0;
 Sum := 0;
 for X := 0 to Count - 1 do
  begin
   Sum := Sum + Data^[X] - FBuffer^[Pos];
   FBuffer^[Pos] := Data^[X];
   Inc(Pos); if Pos >= FKernelSize then Pos := 0;
   Data^[X] := Sum div FKernelSize;
  end;
end;

procedure TGuiBlurFIRFilter.RadiusChanged;
begin
 inherited;
 FKernelSize := Round(Radius) + 1;
 ResizeBuffer;
end;

procedure TGuiBlurFIRFilter.ResizeBuffer;
begin
 ReallocMem(FBuffer, FKernelSize);
end;

{ TGuiStackBlurFilter }

constructor TGuiStackBlurFilter.Create;
begin
 inherited;
 FKernelSize := Round(FRadius);
 ResizeStack;
end;

destructor TGuiStackBlurFilter.Destroy;
begin

 inherited;
end;

procedure TGuiStackBlurFilter.Filter(ByteMap: TGuiCustomByteMap);
type
  TArrayOfInteger = array of Integer;
var
  Height, Width   : Integer;
  HeightMinus1    : Integer;
  WidthMinus1     : Integer;
  RadiusWidth     : Integer;
  I, X, Y         : Integer;
  YIncrement      : Integer;
  CurrentLine     : PByteArray;

  ASumIn          : Integer;
  ASumOut, ASum   : Integer;

  KernelSizePlus1 : Integer;
  CurrentData     : Byte;
  Data            : PByteArray;

  DivisionLUT     : TArrayOfInteger;
  Divi, DivSum    : Integer;

  StackStart      : Integer;
  StackPos        : Integer;
  Stack           : array of Byte;
  VMin            : TArrayOfInteger;
begin
 // check whether blur needs to be calculated at all
 if (FKernelSize < 1) then Exit;

 // initialize local values
 Width        := ByteMap.Width;
 Height       := ByteMap.Height;
 WidthMinus1  := Width - 1;
 HeightMinus1 := Height - 1;
 RadiusWidth  := FKernelSize * Width;

 //
 SetLength(VMin, Max(Width, Height));

 Divi := 2 * FKernelSize + 1;

 // set up division LUT
 DivSum := (Divi + 1) shr 1;
 DivSum := Sqr(DivSum);

 SetLength(DivisionLUT, 256 * DivSum);
 for I := 0 to High(DivisionLUT)
  do DivisionLUT[I] := (I div DivSum);

 SetLength(Stack, Divi);

 KernelSizePlus1 := FKernelSize + 1;

 for Y := 0 to HeightMinus1 do
  begin
   // initialize sum
   ASumIn  := 0;
   ASumOut := 0;
   ASum    := 0;

   CurrentLine := ByteMap.ScanLine[Y];

   for I := -FKernelSize to FKernelSize do
    begin
     if I < 0 then CurrentData := CurrentLine^[0] else
     if I > WidthMinus1 then CurrentData := CurrentLine^[WidthMinus1]
      else CurrentData := CurrentLine^[I];

     Stack[I + FKernelSize] := CurrentData;

     ASum := ASum + CurrentData * (KernelSizePlus1 - Abs(I));
     if I > 0
      then Inc(ASumIn, CurrentData)
      else Inc(ASumOut, CurrentData);
    end;
   StackPos := FKernelSize;

   for X := 0 to WidthMinus1 do
    begin
     CurrentLine^[X] := DivisionLUT[ASum];

     Dec(ASum, ASumOut);
     StackStart := StackPos - FKernelSize + Divi;

     CurrentData := Stack[StackStart mod Divi];
     Dec(ASumOut, CurrentData);

     if Y = 0 then VMin[X] := Min(X + KernelSizePlus1, WidthMinus1);

     CurrentData := CurrentLine^[VMin[X]];
     Stack[StackStart mod Divi] := CurrentData;
     Inc(ASumIn, CurrentData);
     Inc(ASum, ASumIn);

     StackPos := (StackPos + 1) mod Divi;
     CurrentData := Stack[StackPos];

     Inc(ASumOut, CurrentData);
     Dec(ASumIn, CurrentData);
    end;
  end;

 Data := ByteMap.DataPointer;

 for X := 0 to WidthMinus1 do
  begin
   // initialize sum
   ASumIn := 0;
   ASumOut := 0;
   ASum := 0;

   YIncrement := -RadiusWidth;

   for I := -FKernelSize to FKernelSize do
    begin
     CurrentData := Data^[Max(0, YIncrement) + X];
     Stack[I + FKernelSize] := CurrentData;

     ASum := ASum + CurrentData * (KernelSizePlus1 - Abs(I));

     if I > 0 then Inc(ASumIn, CurrentData) else
      if I < HeightMinus1 then Inc(ASumOut, CurrentData);

     if I < HeightMinus1
      then Inc(YIncrement, Width);
    end;

   YIncrement := X;
   StackPos := FKernelSize;

   for Y := 0 to HeightMinus1 do
    begin
     Data^[YIncrement] := DivisionLUT[ASum];

     Dec(ASum, ASumOut);
     StackStart := StackPos - FKernelSize + Divi;

     CurrentData := Stack[StackStart mod Divi];
     Dec(ASumOut, CurrentData);

     if X = 0 then VMin[Y] := Min(Y + KernelSizePlus1, HeightMinus1) * Width;

     CurrentData := Data^[VMin[Y] + X];
     Stack[StackStart mod Divi] := CurrentData;
     Inc(ASumIn, CurrentData);
     Inc(ASum, ASumIn);

     StackPos := (StackPos + 1) mod Divi;
     CurrentData := Stack[StackPos];

     Inc(ASumOut, CurrentData);
     Dec(ASumIn, CurrentData);
     Inc(YIncrement, Width);
   end;
 end;
end;

procedure TGuiStackBlurFilter.Filter(PixelMap: TGuiCustomPixelMap);
begin

end;

procedure TGuiStackBlurFilter.RadiusChanged;
begin
 inherited;
 FKernelSize := Round(FRadius);
 ResizeStack;
end;

procedure TGuiStackBlurFilter.ResizeStack;
begin
end;


{ TGuiSaturationFilter }

constructor TGuiSaturationFilter.Create;
begin
 inherited;
 FValue := 0;
 CalculateLookUpTable;
end;

procedure TGuiSaturationFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  Index : Integer;
  Data  : PByteArray;
begin
 Data := ByteMap.DataPointer;
 for Index := 0 to (ByteMap.Width * ByteMap.Height) - 1
  do Data^[Index] := FLookUpTable[Data^[Index]];
end;

procedure TGuiSaturationFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
  Data  : PPixel32Array;
begin
 Data := PixelMap.DataPointer;
 for Index := 0 to (PixelMap.Width * PixelMap.Height) - 1 do
  begin
   Data^[Index].A := FLookUpTable[Data^[Index].A];
   Data^[Index].R := FLookUpTable[Data^[Index].R];
   Data^[Index].G := FLookUpTable[Data^[Index].G];
   Data^[Index].B := FLookUpTable[Data^[Index].B];
  end;
end;

procedure TGuiSaturationFilter.CalculateLookUpTable;
var
  ExpPos : Double;
  Index  : Integer;
begin
 ExpPos := 1 - (2 * (1 + Abs(Value)) / ((1 + Abs(Value) + Value)));
 for Index := 0 to Length(FLookUpTable) - 1
  do FLookUpTable[Index] := Round($FF * Index / (Index - ExpPos * (Index xor $FF)));
end;

procedure TGuiSaturationFilter.ValueChanged;
begin
 CalculateLookupTable;
end;

procedure TGuiSaturationFilter.SetValue(const Value: Single);
begin
 if FValue <> Value then
  begin
   FValue := Value;
   ValueChanged;
  end;
end;

{ TGuiEmbossFilter }

procedure TGuiEmbossFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  x, y   : Integer;
  p1, p2 : PByteArray;
begin
 for y := 0 to ByteMap.Height - 2 do
  begin
   p1 := ByteMap.Scanline[y];
   p2 := ByteMap.Scanline[y + 1];
   for x := 0 to ByteMap.Width - 2
    do p1[x] := (p1[x] + (p2[(x + 1)] xor $FF)) shr 1;
  end;
end;

procedure TGuiEmbossFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  x, y   : Integer;
  p1, p2 : PPixel32Array;
begin
 for y := 0 to PixelMap.Height - 2 do
  begin
   p1 := PixelMap.Scanline[y];
   p2 := PixelMap.Scanline[y + 1];
   for x := 0 to PixelMap.Width - 2 do
    begin
     p1[x].R := (p1[x].R + (p2[x + 1].R xor $FF)) shr 1;
     p1[x].G := (p1[x].G + (p2[x + 1].G xor $FF)) shr 1;
     p1[x].B := (p1[x].B + (p2[x + 1].B xor $FF)) shr 1;
    end;
  end;
end;


{ TGuiInvertFilter }

procedure TGuiInvertFilter.Filter(ByteMap: TGuiCustomByteMap);
var
  Index : Integer;
  Data  : PByteArray;
begin
 Data := ByteMap.DataPointer;
 for Index := 0 to ByteMap.Width * ByteMap.Height - 1
  do Data^[Index] := not Data^[Index];
end;

procedure TGuiInvertFilter.Filter(PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
  Data  : PPixel32Array;
begin
 Data := PixelMap.DataPointer;
 for Index := 0 to PixelMap.Width * PixelMap.Height - 1
  do Data^[Index].ARGB := not Data^[Index].ARGB;
end;

end.
