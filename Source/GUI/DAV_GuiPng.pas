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
  protected
    FImageHeader         : TChunkPngImageHeader;
    FPaletteChunk        : TChunkPngPalette;
    FGammaChunk          : TChunkPngGamma;
    FDataChunkList       : TChunkList;
    FAdditionalChunkList : TChunkList;

    FDeviceContext       : HDC;
    FHandle              : HBITMAP;
    FPalette             : HPALETTE;
    FBitmapInfo          : TBitmapInfo;

    FCanvas              : TCanvas;
    FImageData           : Pointer;

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
  Math, DAV_Common, DAV_GuiPngResourceStrings;

type
  TCrcTable = array [0..255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable : PCrcTable;

const
  CPngMagic = #$0D#$0A#$1A#$0A;


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

 with FBitmapInfo do
  begin
   bmiHeader.biSize := SizeOf(TBitmapInfoHeader);
   bmiHeader.biPlanes := 1;
   bmiHeader.biBitCount := 32;
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

 inherited;
end;

procedure TPortableNetworkGraphic.DisposeImageData;
begin
 if FHandle <> 0  then DeleteObject(FHandle);
 if FDeviceContext <> 0  then DeleteDC(FDeviceContext);
 if FPalette <> 0 then DeleteObject(FPalette);

 FHandle := 0;
 FDeviceContext := 0;
 FPalette := 0;
end;

procedure TPortableNetworkGraphic.Draw(ACanvas: TCanvas; const Rect: TRect);
begin
 if Empty then Exit;

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

procedure TPortableNetworkGraphic.CopyPaletteToDIB(Palette: HPalette);
var
  Index         : Integer;
  MaxLogPalette : TMaxLogPalette;
begin
 FillChar(MaxLogPalette, SizeOf(MaxLogPalette), 0);
 with FBitmapInfo do
  begin
   bmiHeader.biClrUsed := GetPaletteEntries(Palette, 0, 256, MaxLogPalette.palPalEntry[0]);
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

 if FImageHeader.HasPalette then
  begin
   if FImageHeader.ColourType = ctIndexedColour
    then FPalette := CreateHalfTonePalette(FDeviceContext)
    else FPalette := CreateGrayscalePalette(FImageHeader.BitDepth);

   with FBitmapInfo do
    begin
     ResizePalette(FPalette, 1 shl bmiHeader.biBitCount);
     bmiHeader.biClrUsed := 1 shl bmiHeader.biBitCount;
    end;
   SelectPalette(FDeviceContext, FPalette, False);
   RealizePalette(FDeviceContext);
   CopyPaletteToDIB(FPalette);
  end
 else FBitmapInfo.bmiHeader.biClrUsed := 0;

 with FBitmapInfo.bmiHeader do
  begin
   biWidth := FImageHeader.Width;
   biHeight := FImageHeader.Height;
  end;

 FHandle := CreateDIBSection(FDeviceContext, PBitmapInfo(@FBitmapInfo)^,
   DIB_RGB_COLORS, FImageData, 0, 0);
 SelectObject(FDeviceContext, FHandle);

 FillChar(FImageData^, FImageHeader.BytesPerRow * Integer(Height), 0);
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
var
  DataIndex  : Integer;
  DataStream : TMemoryStream;
  ZStream    : TDecompressionStream;
begin
 BitmapSizeChanged;
 BuildGammaTable;

 DataStream := TMemoryStream.Create;
 try
  for DataIndex := 0 to FDataChunkList.Count - 1 do
   begin
    Assert(FDataChunkList[DataIndex] is TChunkPngImageData);

    with TChunkPngImageData(FDataChunkList[DataIndex]) do
     begin
      Data.Seek(0, soFromBeginning);
      DataStream.CopyFrom(Data, Data.Size);
     end;

    ZStream := TZDecompressionStream.Create(DataStream);
    try
     case FImageHeader.InterlaceMethod of
      imNone  : ;
      imAdam7 : ;
     end;

//     DeinterlaceStream(ZStream);
    finally
     FreeAndNil(ZStream);
    end;
   end;
 finally
  FreeAndNil(DataStream);
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
 GetMem(GCrcTable, 256 * SizeOf(Cardinal));
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
