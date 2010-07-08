unit DAV_GuiPng;

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
//  Portions created by Christian-W. Budde are Copyright (C) 2010             //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.inc}

uses
  Windows, Classes, Graphics, SysUtils, zlib, DAV_Types, DAV_ChunkClasses,
  DAV_GuiCommon, DAV_GuiPngTypes, DAV_GuiPngClasses, DAV_GuiPngChunks;

type
  TChunkList = class(TObject)
  private
    FChunks : array of TCustomChunk;
    function GetCount: Integer;
  protected
    function GetChunk(Index: Integer): TCustomChunk;
  public
    destructor Destroy; override;

    procedure Add(Item: TCustomChunk);
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    function IndexOf(Item: TCustomChunk): Integer;
    procedure Remove(Item: TCustomChunk);

    property Count: Integer read GetCount;
    property Chunks[Index: Integer]: TCustomChunk read GetChunk; default;
  end;

  TTransferNonInterlaced = procedure (Source, Destination, Alpha: Pointer) of object;
  TTransferAdam7 = procedure (const Pass: Byte; Source, Destination, Alpha: Pointer) of object;

  TPortableNetworkGraphic = class(TGraphic)
  private
    FGammaTable        : array [Byte] of Byte;
    FInverseGammaTable : array [Byte] of Byte;

    procedure BuildGammaTable;
    function CalculateCRC(Stream: TStream): Cardinal;
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    procedure ReadImageDataChunk(Stream: TStream);
    procedure ReadUnknownChunk(Stream: TStream);
    procedure InterpreteChunks;

    procedure CopyPaletteToDIB(Palette: HPalette);
    procedure DisposeImageData;
    procedure DecodeInterlacedAdam7(Stream: TMemoryStream);
    procedure DecodeNonInterlaced(Stream: TMemoryStream);

    procedure DecodeImageData;

    procedure TransferNonInterlacedGrayscale2(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedGrayscale16(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedTrueColor8(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedTrueColor16(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedDirect(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedPalette2(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedGrayscaleAlpha8(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedGrayscaleAlpha16(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedTrueColorAlpha8(Source, Destination, Alpha: Pointer);
    procedure TransferNonInterlacedTrueColorAlpha16(Source, Destination, Alpha: Pointer);

    procedure TransferAdam7Grayscale2(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7Grayscale16(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7TrueColor8(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7TrueColor16(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7Palette(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7Palette2(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7GrayscaleAlpha8(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7GrayscaleAlpha16(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7TrueColorAlpha8(const Pass: Byte; Source, Destination, Alpha: Pointer);
    procedure TransferAdam7TrueColorAlpha16(const Pass: Byte; Source, Destination, Alpha: Pointer);

    procedure FilterSub(CurrentRow, PreviousRow: PByteArray; BytesPerRow,
      PixelByteSize: Integer);
    procedure FilterUp(CurrentRow, PreviousRow: PByteArray; BytesPerRow,
      PixelByteSize: Integer);
    procedure FilterAverage(CurrentRow, PreviousRow: PByteArray; BytesPerRow,
      PixelByteSize: Integer);
    procedure FilterPaeth(CurrentRow, PreviousRow: PByteArray; BytesPerRow,
      PixelByteSize: Integer);
    procedure TransferPalette;
  protected
    FImageHeader         : TChunkPngImageHeader;
    FPaletteChunk        : TChunkPngPalette;
    FGammaChunk          : TChunkPngGamma;
    FDataChunkList       : TChunkList;
    FAdditionalChunkList : TChunkList;

    FDeviceContext       : HDC;
    FHandle              : HBITMAP;
    FPalette             : HPALETTE;
    FBitmapInfo          : PBitmapInfo;
    FBytesPerRow         : Integer;

    FCanvas              : TCanvas;
    FImageData           : Pointer;
    FImageAlpha          : Pointer;

    function GetEmpty: Boolean; override;
    function GetHeight: Integer; override;
    function GetPalette: HPALETTE; override;
    function GetWidth: Integer; override;
    procedure SetHeight(Value: Integer); override;
    procedure SetPalette(Value: HPALETTE); override;
    procedure SetWidth(Value: Integer); override;

    function CreateGrayscalePalette(BitDepth: Integer): HPalette;
    procedure Clear; virtual;
    procedure BitmapSizeChanged; virtual;
    procedure HeightChanged; virtual;
    procedure WidthChanged; virtual;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function CanLoad(const FileName: TFileName): Boolean; overload;
    class function CanLoad(Stream: TStream): Boolean; overload;

    procedure Draw(ACanvas: TCanvas; const Rect: TRect); override;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: Cardinal;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: Cardinal;
      var APalette: HPALETTE); override;

    property Canvas: TCanvas read FCanvas;
  end;

implementation

uses
  Math, DAV_Common, DAV_GuiPngCoding, DAV_GuiPngResourceStrings;

resourcestring
  RCStrUnsupportedFilter = 'Unsupported Filter';
  RCStrUnsupportedFormat = 'Unsupported Format';
  RCStrPaletteMissing = 'Required palette is missing';

type
  TCrcTable = array [0..255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable : PCrcTable;

const
  CPngMagic = #$0D#$0A#$1A#$0A;

  CRowStart        : array[0..6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart     : array[0..6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement    : array[0..6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement : array[0..6] of Integer = (8, 8, 4, 4, 2, 2, 1);


{ TChunkList }

destructor TChunkList.Destroy;
begin
 Clear;
 inherited;
end;

procedure TChunkList.Add(Item: TCustomChunk);
begin
 SetLength(FChunks, Length(FChunks) + 1);
 FChunks[Length(FChunks) - 1] := Item;
end;

procedure TChunkList.Clear;
var
  Index : Integer;
begin
 for Index := 0 to Count - 1
  do FreeAndNil(FChunks[Index]);
 SetLength(FChunks, 0)
end;

procedure TChunkList.Delete(Index: Integer);
begin
 if (Index < 0) or (Index >= Count)
  then raise EPngError.Create(RCStrEmptyChunkList);
 FreeAndNil(FChunks[Index]);
 if Index < Count
  then System.Move(FChunks[Index + 1], FChunks[Index], (Count - Index) * SizeOf(Pointer));
 SetLength(FChunks, Length(FChunks) - 1);
end;

function TChunkList.GetChunk(Index: Integer): TCustomChunk;
begin
 if Cardinal(Index) >= Cardinal(Count)
  then raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index])
  else Result := FChunks[Index];
end;

function TChunkList.GetCount: Integer;
begin
 Result := Length(FChunks);
end;

function TChunkList.IndexOf(Item: TCustomChunk): Integer;
begin
 for Result := 0 to Count - 1 do
  if FChunks[Result] = Item
   then Exit;
 Result := -1;
end;

procedure TChunkList.Remove(Item: TCustomChunk);
begin
 Delete(IndexOf(Item));
end;


{ TPortableNetworkGraphic }

constructor TPortableNetworkGraphic.Create;
begin
 FImageHeader         := TChunkPngImageHeader.Create;
 FDataChunkList       := TChunkList.Create;
 FAdditionalChunkList := TChunkList.Create;
 FCanvas              := TCanvas.Create;
 FBytesPerRow         := 0;

 // allocate bitmap info
 GetMem(FBitmapInfo, SizeOf(TBitmapInfoHeader));
 with FBitmapInfo^ do
  begin
   bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
   bmiHeader.biPlanes := 1;
   bmiHeader.biBitCount := 24;
   bmiHeader.biCompression := BI_RGB;
   bmiHeader.biSizeImage := 0;
   bmiHeader.biClrUsed := 0;
   bmiHeader.biClrImportant := 0;
  end;
end;

destructor TPortableNetworkGraphic.Destroy;
begin
 FAdditionalChunkList.Clear;

 FreeAndNil(FAdditionalChunkList);
 FreeAndNil(FDataChunkList);
 FreeAndNil(FImageHeader);

 FreeAndNil(FCanvas);

 DisposeImageData;
 Dispose(FBitmapInfo);

 // free palette chunk
 if Assigned(FPaletteChunk)
  then FreeAndNil(FPaletteChunk);

 // free gamma chunk
 if Assigned(FGammaChunk)
  then FreeAndNil(FGammaChunk);

 inherited;
end;

procedure TPortableNetworkGraphic.DisposeImageData;
begin
 if FHandle <> 0  then DeleteObject(FHandle);
 if FDeviceContext <> 0  then DeleteDC(FDeviceContext);
 if FPalette <> 0 then DeleteObject(FPalette);
 if Assigned(FImageAlpha) then Dispose(FImageAlpha);

 FHandle := 0;
 FDeviceContext := 0;
 FPalette := 0;
 FImageAlpha := nil;
end;

procedure TPortableNetworkGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
 if Empty then Exit;

 SetStretchBltMode(ACanvas.Handle, COLORONCOLOR);
 StretchDiBits(ACanvas.Handle, Rect.Left, Rect.Top, Rect.Right - Rect.Left,
   Rect.Bottom - Rect.Top, 0, 0, Width, Height, FImageData,
   FBitmapInfo^, DIB_RGB_COLORS, SRCCOPY);

 // not implemented yet
end;

function TPortableNetworkGraphic.GetEmpty: Boolean;
begin
 Result := (FHandle = 0); // and (FDIBHandle = 0);
end;

function TPortableNetworkGraphic.GetHeight: Integer;
begin
 Result := Abs(FImageHeader.Height);
end;

function TPortableNetworkGraphic.GetPalette: HPALETTE;
begin
 Result := FPalette;
end;

function TPortableNetworkGraphic.GetWidth: Integer;
begin
 Result := Abs(FImageHeader.Width);
end;

procedure TPortableNetworkGraphic.SetPalette(Value: HPALETTE);
begin
 if FPalette <> Value then
  begin
   CopyPaletteToDIB(Value);

   SelectPalette(FDeviceContext, Value, False);
   RealizePalette(FDeviceContext);

   DeleteObject(FPalette);
   FPalette := Value;
  end;
end;

procedure TPortableNetworkGraphic.SetHeight(Value: Integer);
begin
 if Value <> Height then
  begin
   FImageHeader.Height := Value;
   HeightChanged;
  end;
end;

procedure TPortableNetworkGraphic.SetWidth(Value: Integer);
begin
 if Value <> Width then
  begin
   FImageHeader.Width := Value;
   WidthChanged;
  end;
end;





procedure TPortableNetworkGraphic.TransferNonInterlacedGrayscale16(Source,
  Destination, Alpha: Pointer);
var
  Index   : Integer;
  SrcPtr  : PWord absolute Source;
  DestPtr : PByte absolute Destination;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^ := SrcPtr^ shr 8;
   Inc(DestPtr);
   Inc(SrcPtr, 2);
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedGrayscale2(Source,
  Destination, Alpha: Pointer);
var
  Index   : Integer;
  SrcPtr  : PByte absolute Source;
  DestPtr : PByte absolute Destination;
begin
 for Index := 0 to FImageHeader.BytesPerRow - 1 do
  begin
   DestPtr^ := ((SrcPtr^ shr 2) and $F) or ( SrcPtr^        and $F0); Inc(DestPtr);
   DestPtr^ := ((SrcPtr^ shl 2) and $F) or ((SrcPtr^ shl 4) and $F0); Inc(DestPtr);
   Inc(SrcPtr);
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedTrueColor8(Source,
  Destination, Alpha: Pointer);
var
  Index   : Integer;
  SrcPtr  : PRGB24Array absolute Source;
  DestPtr : PRGB24Array absolute Destination;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^[Index].B := FGammaTable[SrcPtr^[Index].R shr 8];
   DestPtr^[Index].G := FGammaTable[SrcPtr^[Index].G shr 8];
   DestPtr^[Index].R := FGammaTable[SrcPtr^[Index].B shr 8];
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedTrueColor16(Source,
  Destination, Alpha: Pointer);
var
  Index     : Integer;
  SourcePtr : PRGB24WordArray absolute Source;
  DestPtr   : PRGB24Array absolute Destination;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^[Index].B := FGammaTable[SourcePtr^[Index].R shr 8];
   DestPtr^[Index].G := FGammaTable[SourcePtr^[Index].G shr 8];
   DestPtr^[Index].R := FGammaTable[SourcePtr^[Index].B shr 8];
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedDirect(Source,
  Destination, Alpha: Pointer);
begin
 // data needs no further transformation
 Move(Source^, Destination^, FImageHeader.BytesPerRow);
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedPalette2(Source,
  Destination, Alpha: Pointer);
var
  Index   : Integer;
  SrcPtr  : PByte absolute Source;
  DestPtr : PByte absolute Destination;
begin
 for Index := 0 to FImageHeader.BytesPerRow - 1 do
  begin
   DestPtr^ := ((SrcPtr^ shr 4) and $3) or ((SrcPtr^ shr 2) and $30); Inc(DestPtr);
   DestPtr^ := ( SrcPtr^        and $3) or ((SrcPtr^ shl 2) and $30); Inc(DestPtr);
   Inc(SrcPtr);
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedGrayscaleAlpha8(Source,
  Destination, Alpha: Pointer);
var
  Index    : Integer;
  SrcPtr   : PByte absolute Source;
  DestPtr  : PByte absolute Destination;
  AlphaPtr : PByte absolute Alpha;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^  := SrcPtr^;  Inc(SrcPtr);
   AlphaPtr^ := SrcPtr^;  Inc(SrcPtr);
   Inc(DestPtr);
   Inc(AlphaPtr);
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedGrayscaleAlpha16(Source,
  Destination, Alpha: Pointer);
var
  Index    : Integer;
  SrcPtr   : PByte absolute Source;
  DestPtr  : PByte absolute Destination;
  AlphaPtr : PByte absolute Alpha;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^  := SrcPtr^; Inc(SrcPtr, 2);
   AlphaPtr^ := SrcPtr^; Inc(SrcPtr, 2);
   Inc(DestPtr);
   Inc(AlphaPtr);
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedTrueColorAlpha8(Source,
  Destination, Alpha: Pointer);
var
  Index    : Integer;
  SrcPtr   : PRGB32Array absolute Source;
  DestPtr  : PRGB24Array absolute Destination;
  AlphaPtr : PByteArray absolute Alpha;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^[Index].B := FGammaTable[SrcPtr^[Index].R];
   DestPtr^[Index].G := FGammaTable[SrcPtr^[Index].G];
   DestPtr^[Index].R := FGammaTable[SrcPtr^[Index].B];
   AlphaPtr^[Index] := SrcPtr^[Index].A;
  end;
end;

procedure TPortableNetworkGraphic.TransferNonInterlacedTrueColorAlpha16(Source,
  Destination, Alpha: Pointer);
var
  Index    : Integer;
  SrcPtr   : PRGB32WordArray absolute Source;
  DestPtr  : PRGB24Array absolute Destination;
  AlphaPtr : PByteArray absolute Alpha;
begin
 for Index := 0 to Width - 1 do
  begin
   DestPtr^[Index].B := FGammaTable[SrcPtr^[Index].R shr 8];
   DestPtr^[Index].G := FGammaTable[SrcPtr^[Index].G shr 8];
   DestPtr^[Index].R := FGammaTable[SrcPtr^[Index].B shr 8];
   AlphaPtr^[Index] := SrcPtr^[Index].A;
  end;
end;







procedure TPortableNetworkGraphic.TransferAdam7Grayscale2(const Pass: Byte;
  Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7Grayscale16(
  const Pass: Byte; Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7TrueColor8(const Pass: Byte;
  Source, Destination, Alpha: Pointer);
var
  Index   : Integer;
  SrcPtr  : PRGB24 absolute Source;
  DestPtr : PByte absolute Destination;
begin
 Index := CColumnStart[Pass];
 Inc(DestPtr, Index * 3);
 repeat
  DestPtr^ := FGammaTable[SrcPtr^.B]; Inc(DestPtr);
  DestPtr^ := FGammaTable[SrcPtr^.G]; Inc(DestPtr);
  DestPtr^ := FGammaTable[SrcPtr^.R]; Inc(DestPtr);

  Inc(SrcPtr);
  Inc(DestPtr, CColumnIncrement[Pass] * 3 - 3);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= Width;
end;

procedure TPortableNetworkGraphic.TransferAdam7TrueColor16(
  const Pass: Byte; Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7Palette(const Pass: Byte;
  Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7Palette2(const Pass: Byte;
  Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7GrayscaleAlpha8(
  const Pass: Byte; Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7GrayscaleAlpha16(
  const Pass: Byte; Source, Destination, Alpha: Pointer);
begin

end;

procedure TPortableNetworkGraphic.TransferAdam7TrueColorAlpha8(
  const Pass: Byte; Source, Destination, Alpha: Pointer);
var
  Index    : Integer;
  SrcPtr   : PRGB32 absolute Source;
  DestPtr  : PByte absolute Destination;
  AlphaPtr : PByte absolute Alpha;
begin
 Index := CColumnStart[Pass];
 Inc(DestPtr, Index * 3);
 repeat
  DestPtr^ := FGammaTable[SrcPtr^.R]; Inc(DestPtr);
  DestPtr^ := FGammaTable[SrcPtr^.G]; Inc(DestPtr);
  DestPtr^ := FGammaTable[SrcPtr^.B]; Inc(DestPtr);
  AlphaPtr^ := SrcPtr^.A;

  Inc(SrcPtr);
  Inc(AlphaPtr);
  Inc(DestPtr, CColumnIncrement[Pass] * 3 - 3);
  Inc(Index, CColumnIncrement[Pass]);
 until Index >= Width;
end;

procedure TPortableNetworkGraphic.TransferAdam7TrueColorAlpha16(
  const Pass: Byte; Source, Destination, Alpha: Pointer);
begin

end;







procedure TPortableNetworkGraphic.CopyPaletteToDIB(Palette: HPalette);
var
  Index, Entries : Integer;
  MaxLogPalette  : TMaxLogPalette;
begin
 FillChar(MaxLogPalette, SizeOf(MaxLogPalette), 0);

 // get number of entries
 Entries := GetPaletteEntries(Palette, 0, 256, MaxLogPalette.palPalEntry[0]);

 // reallocate bitmap info
 ReallocMem(FBitmapInfo, SizeOf(TBitmapInfo) + Entries * SizeOf(TRGBQuad));

 with FBitmapInfo^ do
  begin
   bmiHeader.biClrUsed := Entries;

   for Index := 0 to bmiHeader.biClrUsed - 1 do
    begin
     bmiColors[Index].rgbBlue  := MaxLogPalette.palPalEntry[Index].peBlue;
     bmiColors[Index].rgbRed   := MaxLogPalette.palPalEntry[Index].peRed;
     bmiColors[Index].rgbGreen := MaxLogPalette.palPalEntry[Index].peGreen;
    end;
  end;
end;

function TPortableNetworkGraphic.CreateGrayscalePalette(BitDepth: Integer): HPalette;
var
  Index: Integer;
  MaxLogPalette: TMaxLogPalette;
begin
 // Prepares and fills the strucutre
 if BitDepth = 16 then BitDepth := 8;

 FillChar(MaxLogPalette, SizeOf(MaxLogPalette), 0);
 MaxLogPalette.palVersion := $300;
 MaxLogPalette.palNumEntries := 1 shl Bitdepth;

 // Fill it with grayscale colors
 for Index := 0 to MaxLogPalette.palNumEntries - 1 do
  with MaxLogPalette do
   begin
    palPalEntry[Index].peRed  := FGammaTable[MulDiv(Index, 255, palNumEntries - 1)];
    palPalEntry[Index].peGreen := palPalEntry[Index].peRed;
    palPalEntry[Index].peBlue := palPalEntry[Index].peRed;
   end;

 // Creates and returns the palette
 Result := CreatePalette(pLogPalette(@MaxLogPalette)^);
end;

procedure TPortableNetworkGraphic.BitmapSizeChanged;
begin
 DisposeImageData;

 FDeviceContext := CreateCompatibleDC(0);
 Canvas.Handle := FDeviceContext;

 // allocate alpha image data
 with FImageHeader do
  if ColorType in [ctTrueColorAlpha, ctGrayscaleAlpha] then
   begin
    GetMem(FImageAlpha, Integer(FImageHeader.Width) * Integer(Height));
    FillChar(FImageAlpha^, Integer(Width) * Integer(Height), 0);
   end;

 with FBitmapInfo^.bmiHeader do
  begin
   biWidth := FImageHeader.Width;
   biHeight := -FImageHeader.Height;
   case FImageHeader.ColorType of
    ctGrayscale, ctIndexedColor, ctGrayscaleAlpha :
      case FImageHeader.BitDepth of
        1, 4, 8 : biBitCount := FImageHeader.BitDepth;
        2       : biBitCount := 4;
        16      : biBitCount := 8;
      end;
    ctTrueColor, ctTrueColorAlpha: biBitCount := 24;
   end;

   FBytesPerRow := ((FImageHeader.Width * biBitCount + $1F) and not $1F) shr 3;
  end;

 if FImageHeader.HasPalette then
  begin
   if FImageHeader.ColorType = ctIndexedColor
    then FPalette := CreateHalfTonePalette(FDeviceContext)
    else FPalette := CreateGrayscalePalette(FImageHeader.BitDepth);

   with FBitmapInfo^ do
    begin
     ResizePalette(FPalette, 1 shl bmiHeader.biBitCount);
     bmiHeader.biClrUsed := 1 shl bmiHeader.biBitCount;
    end;
   SelectPalette(FDeviceContext, FPalette, False);
   RealizePalette(FDeviceContext);
   CopyPaletteToDIB(FPalette);
  end
 else FBitmapInfo^.bmiHeader.biClrUsed := 0;

 FHandle := CreateDIBSection(FDeviceContext, FBitmapInfo^, DIB_RGB_COLORS,
   FImageData, 0, 0);
 SelectObject(FDeviceContext, FHandle);

 FillChar(FImageData^, FBytesPerRow * Integer(Height), 0);
end;

procedure TPortableNetworkGraphic.HeightChanged;
begin
 BitmapSizeChanged;
end;

procedure TPortableNetworkGraphic.WidthChanged;
begin
 BitmapSizeChanged;
end;

class function TPortableNetworkGraphic.CanLoad(const FileName: TFileName): Boolean;
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   Result := CanLoad(FileStream);
  finally
   Free;
  end;
end;

class function TPortableNetworkGraphic.CanLoad(Stream: TStream): Boolean;
var
  ChunkID : TChunkName;
begin
 Result := Stream.Size >= 4;

 if Result then
  begin
   Stream.Read(ChunkID, 4);
   Stream.Seek(-4, soFromCurrent);
   Result := ChunkID = '‰PNG';
  end;
end;

procedure TPortableNetworkGraphic.LoadFromClipboardFormat(AFormat: Word; AData: Cardinal;
  APalette: HPALETTE);
begin
 with TBitmap.Create do
  try
   LoadFromClipboardFormat(AFormat, AData, APalette);
  finally
   Free;
  end;
end;

procedure TPortableNetworkGraphic.SaveToClipboardFormat(var AFormat: Word;
  var AData: Cardinal; var APalette: HPALETTE);
begin
 with TBitmap.Create do
  try
   Width := Self.Width;
   Height := Self.Height;
   Self.Draw(Canvas, Rect(0, 0, Width, Height));
   SaveToClipboardFormat(AFormat, AData, APalette);
  finally
   Free;
  end;
end;

procedure TPortableNetworkGraphic.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Integer;
  ChunkCRC     : Cardinal;
  ChunkClass   : TCustomChunkPngWithHeaderClass;
  Chunk        : TCustomChunkPngWithHeader;
  MemoryStream : TMemoryStream;
begin
 with Stream do
  begin
   Clear;

   // check for minimum file size
   if Size < 8
    then EPngError.Create(RCStrNotAValidPNGFile);

   // read chunk ID
   Read(ChunkName, 4);
   if ChunkName <> '‰PNG'
    then EPngError.Create(RCStrNotAValidPNGFile);

   // read PNG magic
   Read(ChunkName, 4);
   if ChunkName <> CPngMagic
    then EPngError.Create(RCStrNotAValidPNGFile);

   MemoryStream := TMemoryStream.Create;
   try
    // read image header chunk size
    ChunkSize := ReadSwappedCardinal(Stream);
    if ChunkSize > Stream.Size - 12
     then EPngError.Create(RCStrNotAValidPNGFile);

    // read image header chunk ID
    Read(ChunkName, 4);
    if ChunkName <> 'IHDR'
     then EPngError.Create(RCStrNotAValidPNGFile);

    // reset position to the chunk start and copy stream to memory
    Seek(-8, soCurrent);
    MemoryStream.CopyFrom(Stream, ChunkSize + 8);
    MemoryStream.Seek(0, soFromBeginning);

    // load image header
    FImageHeader.LoadFromStream(MemoryStream);

    // read image header chunk size
    Read(ChunkCRC, 4);
    {$IFDEF CheckCRC}
    if not CheckCRC(MemoryStream, Swap32(ChunkCRC))
     then raise EPngError.Create(RCStrCRCError);
    {$ENDIF}

    while Stream.Position < Stream.Size do
     begin
      // read image header chunk size
      ChunkSize := ReadSwappedCardinal(Stream);
      if ChunkSize > Stream.Size - Stream.Position - 4
       then EPngError.Create(RCStrNotAValidPNGFile);

      // read chunk ID
      Read(ChunkName, 4);

      // check for stream end
      if ChunkName = 'IEND' then
       begin
        // read image header chunk size
        Read(ChunkCRC, 4);

        Break;
       end;

      // reset position to the chunk start and copy stream to memory
      Seek(-8, soCurrent);
      MemoryStream.Clear;
      MemoryStream.CopyFrom(Stream, ChunkSize + 8);
      MemoryStream.Seek(0, soFromBeginning);

      if ChunkName = 'IHDR'
       then EPngError.Create(RCStrNotAValidPNGFile) else
      if ChunkName = 'IDAT'
       then ReadImageDataChunk(MemoryStream) else
      if ChunkName = 'gAMA' then
       begin
        if Assigned(FGammaChunk)
         then raise EPngError.Create(RCStrSeveralGammaChunks);
        FGammaChunk := TChunkPngGamma.Create(FImageHeader);
        FGammaChunk.LoadFromStream(MemoryStream);
       end else
      if ChunkName = 'PLTE' then
       begin
        if Assigned(FPaletteChunk)
         then raise EPngError.Create(RCStrSeveralPaletteChunks);
        FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
        FPaletteChunk.LoadFromStream(MemoryStream);
       end
      else
       begin
        ChunkClass := FindPngChunkByChunkName(ChunkName);
        if ChunkClass <> nil then
         begin
          Chunk := ChunkClass.Create(FImageHeader);
          Chunk.LoadFromStream(MemoryStream);
          FAdditionalChunkList.Add(Chunk);
         end
        else
         begin
          // check if chunk is ancillary
          if (Byte(ChunkName[0]) and $80) = 0
           then ReadUnknownChunk(MemoryStream)
           else raise EPngError.Create(RCStrAncillaryUnknownChunk);
         end;
       end;

      // read image header chunk size
      Read(ChunkCRC, 4);
      {$IFDEF CheckCRC}
      if not CheckCRC(MemoryStream, Swap32(ChunkCRC))
       then raise EPngError.Create(RCStrCRCError);
      {$ENDIF}

     end;
   finally
    FreeAndNil(MemoryStream);
   end;
  end;

 InterpreteChunks;
end;

procedure TPortableNetworkGraphic.SaveToStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  CRC          : Cardinal;
  MemoryStream : TMemoryStream;
  Index        : Integer;
begin
 with Stream do
  begin
   // write chunk ID
   ChunkName := '‰PNG';
   Write(ChunkName, 4);

   // write PNG magic
   ChunkName := CPngMagic;
   Write(ChunkName, 4);

   MemoryStream := TMemoryStream.Create;
   try
    // save image header to memory stream
    FImageHeader.SaveToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Seek(0, soFromBeginning);
    Stream.CopyFrom(MemoryStream, MemoryStream.Size);

    // calculate and write CRC
    CRC := CalculateCRC(MemoryStream);
    Write(CRC, SizeOf(Cardinal));

    for Index := 0 to FAdditionalChunkList.Count - 1 do
     begin
      MemoryStream.Clear;
      TCustomChunkPng(FAdditionalChunkList[Index]).SaveToStream(MemoryStream);

      // copy memory stream to stream
      MemoryStream.Seek(0, soFromBeginning);
      Stream.CopyFrom(MemoryStream, MemoryStream.Size);

      // calculate and write CRC
      CRC := CalculateCRC(MemoryStream);
      Write(CRC, SizeOf(Cardinal));
     end;
   finally
    FreeAndNil(MemoryStream);
   end;
  end;
end;

procedure TPortableNetworkGraphic.ReadUnknownChunk(Stream: TStream);
var
  UnknownChunk : TUnknownPngChunk;
begin
 UnknownChunk := TUnknownPngChunk.Create;
 UnknownChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TPortableNetworkGraphic.ReadImageDataChunk(Stream: TStream);
var
  ImageDataChunk : TChunkPngImageData;
begin
 ImageDataChunk := TChunkPngImageData.Create(FImageHeader);
 ImageDataChunk.LoadFromStream(Stream);
 FDataChunkList.Add(ImageDataChunk);
end;

procedure TPortableNetworkGraphic.BuildGammaTable;
var
  Index        : Integer;
  PreCalcGamma : Extended;
const
  COne255th : Extended = 1 / 255;
begin
 if Assigned(FGammaChunk) and (FGammaChunk.Gamma <> 0) then
  begin
   PreCalcGamma := (FGammaChunk.Gamma * 2.2E-5);
   for Index := 0 to 255 do
    begin
     FGammaTable[Index] := Round(Power((Index * COne255th), 1 / PreCalcGamma) * 255);
     FInverseGammaTable[Round(Power((Index * COne255th), 1 / PreCalcGamma) * 255)] := Index;
    end;
  end else
 for Index := 0 to 255 do
  begin
   FGammaTable[Index] := Index;
   FInverseGammaTable[Index] := Index;
  end;
end;

function TPortableNetworkGraphic.CalculateCRC(Stream: TStream): Cardinal;
var
  CrcValue : Cardinal;
  Value    : Byte;
begin
 with Stream do
  begin
   Seek(4, soFromBeginning);

   // initialize CRC
   CrcValue := $FFFFFFFF;

   while Position < Size do
    begin
     Read(Value, 1);

     CrcValue := GCrcTable^[(CrcValue xor Value) and $FF] xor (CrcValue shr 8);
    end;

   Result := (CrcValue xor $FFFFFFFF);

   Seek(0, soFromBeginning);
  end;
end;

function TPortableNetworkGraphic.CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
begin
 Result := CalculateCRC(Stream) = CRC;
end;

procedure TPortableNetworkGraphic.InterpreteChunks;
begin
 BitmapSizeChanged;
 BuildGammaTable;
 if FImageHeader.HasPalette
  then TransferPalette;
 DecodeImageData;
end;

procedure TPortableNetworkGraphic.TransferPalette;
var
  Index         : Integer;
  PaletteEntry  : TRGB24;
  MaxLogPalette : TMaxLogPalette;
  OldPalette    : HPALETTE;
begin
 if not Assigned(FPaletteChunk)
  then raise EPngError.Create(RCStrPaletteMissing);

 FillChar(MaxLogPalette, SizeOf(TMaxLogPalette), 0);
 MaxLogPalette.palVersion := $300;
 MaxLogPalette.palNumEntries := FPaletteChunk.Count;

 for Index := 0 to FPaletteChunk.Count - 1 do
  with MaxLogPalette.palPalEntry[Index] do
   begin
    PaletteEntry := FPaletteChunk.PaletteEntry[Index];
    peRed  :=  FGammaTable[PaletteEntry.B];
    peGreen := FGammaTable[PaletteEntry.G];
    peBlue :=  FGammaTable[PaletteEntry.R];
    peFlags := 0;
   end;

 OldPalette := FPalette;
 FPalette := CreatePalette(PLogPalette(@MaxLogPalette)^);

 CopyPaletteToDIB(FPalette);

 SelectPalette(FDeviceContext, FPalette, False);
 RealizePalette(FDeviceContext);
 DeleteObject(OldPalette);
end;

procedure TPortableNetworkGraphic.DecodeImageData;
var
  DataIndex   : Integer;
  DataStream  : TMemoryStream;
  ZStream     : TDecompressionStream;
  DecodedData : TMemoryStream;
begin
 DataStream := TMemoryStream.Create;
 DecodedData := nil;
 try
  // combine all data chunks first
  for DataIndex := 0 to FDataChunkList.Count - 1 do
   begin
    // make sure the chunk is inded an image data chunk
    Assert(FDataChunkList[DataIndex] is TChunkPngImageData);

    // concat current chunk to data stream
    with TChunkPngImageData(FDataChunkList[DataIndex]) do
     begin
      Data.Seek(0, soFromBeginning);
      DataStream.CopyFrom(Data, Data.Size);
     end;
   end;

  // check whether compression method is supported
  if FImageHeader.CompressionMethod <> 0
   then raise EPngError.Create(RCStrUnsupportedCompressionMethod);

  // reset data stream position to zero
  DataStream.Seek(0, soFromBeginning);

  // create z decompression stream on data stream
  ZStream := TZDecompressionStream.Create(DataStream);
  try
   // create raw data buffer
   DecodedData := TMemoryStream.Create;

   // decode z-stream data to decoded data stream
   DecodedData.CopyFrom(ZStream, ZStream.Size);
  finally
   FreeAndNil(ZStream);
  end;

  // reset decoded data position
  DecodedData.Seek(0, soFromBeginning);

  // decode the data further, depending on the interlace method
  case FImageHeader.InterlaceMethod of
   imNone  : DecodeNonInterlaced(DecodedData);
   imAdam7 : DecodeInterlacedAdam7(DecodedData);
  end;

 finally
  FreeAndNil(DataStream);
  if Assigned(DecodedData)
   then FreeAndNil(DecodedData);
 end;
end;

procedure TPortableNetworkGraphic.FilterSub(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + CurrentRow[Index - PixelByteSize]) and $FF;
end;

procedure TPortableNetworkGraphic.FilterUp(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index]) and $FF;
end;

procedure TPortableNetworkGraphic.FilterAverage(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to PixelByteSize
  do CurrentRow[Index] := (CurrentRow[Index] + PreviousRow[Index] shr 1) and $FF;

 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] + (CurrentRow[Index - PixelByteSize] + PreviousRow[Index]) shr 1) and $FF;
end;

function PaethPredictor(a, b, c: Byte): Byte;
var
  DistA, DistB, DistC: Integer;
begin
 DistA := Abs(b - c);
 DistB := Abs(a - c);
 DistC := Abs(a + b - c * 2);

 if (DistA <= DistB) and (DistA <= DistC) then Result := a else
 if DistB <= DistC
  then Result := b
  else Result := c;
end;

procedure TPortableNetworkGraphic.FilterPaeth(CurrentRow, PreviousRow: PByteArray;
  BytesPerRow, PixelByteSize: Integer);
var
  Index : Integer;
begin
 for Index := 1 to PixelByteSize
  do CurrentRow[Index] := (CurrentRow[Index] +
       PaethPredictor(0, PreviousRow[Index], 0)) and $FF;

 for Index := PixelByteSize + 1 to BytesPerRow
  do CurrentRow[Index] := (CurrentRow[Index] +
       PaethPredictor(CurrentRow[Index - PixelByteSize], PreviousRow[Index],
         PreviousRow[Index - PixelByteSize])) and $FF;
end;

procedure TPortableNetworkGraphic.DecodeNonInterlaced(Stream: TMemoryStream);
var
  Index         : Integer;
  ImageDataPtr  : PByte;
  RowBuffer     : array [0..1] of PByteArray;
  RowUsed       : Integer;
  RowByteSize   : Integer;
  BytesPerRow   : Integer;
  PixelByteSize : Integer;
  TransferProc  : TTransferNonInterlaced;
begin
 // assign transfer procedure
 case FImageHeader.ColorType of
  ctGrayscale  :
   case FImageHeader.BitDepth of
    1, 4, 8: TransferProc := TransferNonInterlacedDirect;
    2      : TransferProc := TransferNonInterlacedGrayscale2;
    16     : TransferProc := TransferNonInterlacedGrayscale16;
   end;
  ctTrueColor :
   case FImageHeader.BitDepth of
     8 : TransferProc := TransferNonInterlacedTrueColor8;
    16 : TransferProc := TransferNonInterlacedTrueColor16;
   end;
  ctIndexedColor :
   case FImageHeader.BitDepth of
    1, 4, 8: TransferProc := TransferNonInterlacedDirect;
    2      : TransferProc := TransferNonInterlacedPalette2
   end;
  ctGrayscaleAlpha :
    case FImageHeader.BitDepth of
      8  : TransferProc := TransferNonInterlacedGrayscaleAlpha8;
     16  : TransferProc := TransferNonInterlacedGrayscaleAlpha16;
    end;
  ctTrueColorAlpha :
    case FImageHeader.BitDepth of
      8  : TransferProc := TransferNonInterlacedTrueColorAlpha8;
     16  : TransferProc := TransferNonInterlacedTrueColorAlpha16;
    end;
 end;

 // check whether a transfer function has been assigned
 if not Assigned(TransferProc)
  then raise EPngError.Create(RCStrUnsupportedFormat);

 // initialize variables
 RowUsed := 0;
 ImageDataPtr := FImageData;
 BytesPerRow := FImageHeader.BytesPerRow;
 RowByteSize := BytesPerRow + 1;
 PixelByteSize := FImageHeader.PixelByteSize;

 try
  GetMem(RowBuffer[0], RowByteSize);
  GetMem(RowBuffer[1], RowByteSize);
  FillChar(RowBuffer[1 - RowUsed]^[0], RowByteSize, 0);

  for Index := 0 to Height - 1 do
   begin
    if Stream.Read(RowBuffer[RowUsed][0], RowByteSize) <> RowByteSize
     then raise EPngError.Create('Data not complete');

    case TAdaptiveFilterMethod(RowBuffer[RowUsed]^[0]) of
     afmNone    : ; // do nothing
     afmSub     : FilterSub(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], BytesPerRow, PixelByteSize);
     afmUp      : FilterUp(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], BytesPerRow, PixelByteSize);
     afmAverage : FilterAverage(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], BytesPerRow, PixelByteSize);
     afmPaeth   : FilterPaeth(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], BytesPerRow, PixelByteSize);
     else raise EPngError.Create(RCStrUnsupportedFilter);
    end;

    TransferProc(@RowBuffer[RowUsed][1], ImageDataPtr, FImageAlpha);

    Inc(ImageDataPtr, FBytesPerRow);
    RowUsed := 1 - RowUsed;
   end;
 finally
  if Assigned(RowBuffer[0]) then Dispose(RowBuffer[0]);
  if Assigned(RowBuffer[1]) then Dispose(RowBuffer[1]);
 end;
end;

procedure TPortableNetworkGraphic.DecodeInterlacedAdam7(Stream: TMemoryStream);
var
  ImageDataPtr  : PByte;
  ImageAlphaPtr : PByte;
  RowBuffer     : array [0..1] of PByteArray;
  RowUsed       : Integer;
  RowByteSize   : Integer;
  PixelPerRow   : Integer;
  PixelByteSize : Integer;
  CurrentPass   : Integer;
  CurrentRow    : Integer;
  TransferProc  : TTransferAdam7;
begin
 // assign transfer procedure
 case FImageHeader.ColorType of
  ctGrayscale  :
   case FImageHeader.BitDepth of
    1, 4, 8: TransferProc := TransferAdam7Palette;
    2      : TransferProc := TransferAdam7Grayscale2;
    16     : TransferProc := TransferAdam7Grayscale16;
   end;
  ctTrueColor :
   case FImageHeader.BitDepth of
     8 : TransferProc := TransferAdam7TrueColor8;
    16 : TransferProc := TransferAdam7TrueColor16;
   end;
  ctIndexedColor :
   case FImageHeader.BitDepth of
    1, 4, 8: TransferProc := TransferAdam7Palette;
    2      : TransferProc := TransferAdam7Palette2
   end;
  ctGrayscaleAlpha :
    case FImageHeader.BitDepth of
      8  : TransferProc := TransferAdam7GrayscaleAlpha8;
     16  : TransferProc := TransferAdam7GrayscaleAlpha16;
    end;
  ctTrueColorAlpha :
    case FImageHeader.BitDepth of
      8  : TransferProc := TransferAdam7TrueColorAlpha8;
     16  : TransferProc := TransferAdam7TrueColorAlpha16;
    end;
 end;

 // check whether a transfer function has been assigned
 if not Assigned(TransferProc)
  then raise EPngError.Create(RCStrUnsupportedFormat);

 // initialize variables
 RowUsed := 0;
 PixelByteSize := FImageHeader.PixelByteSize;

 try
  // allocate row buffer memory
  GetMem(RowBuffer[0], FImageHeader.BytesPerRow + 1);
  GetMem(RowBuffer[1], FImageHeader.BytesPerRow + 1);

  // The Adam7 interlacer uses 7 passes to create the complete image
  for CurrentPass := 0 to 6 do
   begin
    // calculate some intermediate variables
    PixelPerRow := (Width - CColumnStart[CurrentPass] + CColumnIncrement[CurrentPass] - 1) div CColumnIncrement[CurrentPass];
    RowByteSize := PixelByteSize * PixelPerRow;
    CurrentRow := CRowStart[CurrentPass];
    ImageDataPtr := FImageData;
    Inc(ImageDataPtr, CurrentRow * FBytesPerRow);
    ImageAlphaPtr := Ptr(Longint(FImageAlpha) + Width * CurrentRow);

    // clear previous row
    FillChar(RowBuffer[1 - RowUsed]^[0], RowByteSize, 0);

    // check whether there are any bytes to process in this pass.
    if RowByteSize > 0 then
     while CurrentRow < Height do
      begin
       // get interlaced row data
       if Stream.Read(RowBuffer[RowUsed][0], RowByteSize + 1) <> (RowByteSize + 1)
        then raise EPngError.Create('Data not complete');

       // apply filter
       case TAdaptiveFilterMethod(RowBuffer[RowUsed]^[0]) of
        afmNone    : ; // do nothing
        afmSub     : FilterSub(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], RowByteSize, PixelByteSize);
        afmUp      : FilterUp(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], RowByteSize, PixelByteSize);
        afmAverage : FilterAverage(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], RowByteSize, PixelByteSize);
        afmPaeth   : FilterPaeth(RowBuffer[RowUsed], RowBuffer[1 - RowUsed], RowByteSize, PixelByteSize);
        else raise EPngError.Create(RCStrUnsupportedFilter);
       end;

       // transfer and deinterlace image data
       TransferProc(CurrentPass, @RowBuffer[RowUsed][1], ImageDataPtr, FImageAlpha);

       // prepare for the next pass
       Inc(ImageDataPtr, CRowIncrement[CurrentPass] * FBytesPerRow);
       Inc(CurrentRow, CRowIncrement[CurrentPass]);
       RowUsed := 1 - RowUsed;
      end;
   end;
 finally
  if Assigned(RowBuffer[0]) then Dispose(RowBuffer[0]);
  if Assigned(RowBuffer[1]) then Dispose(RowBuffer[1]);
 end;
end;

procedure TPortableNetworkGraphic.Clear;
begin
 // clear chunk lists
 FDataChunkList.Clear;
 FAdditionalChunkList.Clear;

 // reset image header to default
 FImageHeader.ResetToDefault;

 // free palette chunk
 if Assigned(FPaletteChunk)
  then FreeAndNil(FPaletteChunk);

 // free gamma chunk
 if Assigned(FGammaChunk)
  then FreeAndNil(FGammaChunk);
end;

procedure BuildCrcTable(Polynomial: Cardinal);
var
  c    : Cardinal;
  n, k : Integer;
begin
 // allocate CRC table memory
 GetMem(GCrcTable, 256 * SizeOf(Cardinal));

 // fill CRC table
 for n := 0 to 255 do
  begin
   c := n;
   for k := 0 to 7 do
    begin
     if (c and 1) <> 0
      then c := Polynomial xor (c shr 1)
      else c := c shr 1;
    end;
   GCrcTable^[n] := c;
  end;
end;

initialization
  BuildCrcTable($EDB88320);
  TPicture.RegisterFileFormat('PNG', 'Portable Network Graphics', TPortableNetworkGraphic);

finalization
  Dispose(GCrcTable);
  TPicture.UnregisterGraphicClass(TPortableNetworkGraphic);

end.
