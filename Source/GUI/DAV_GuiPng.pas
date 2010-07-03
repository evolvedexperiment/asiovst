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

{$DEFINE CheckCRC}

uses
  Windows, Classes, Graphics, SysUtils, zlib, DAV_Types, DAV_ChunkClasses,
  DAV_GuiCommon, DAV_GuiPngTypes;

type
  EPngError = class(Exception);

  TCustomChunkPng = class(TDefinedChunk)
  public
    constructor Create; override;
  end;

  TChunkPngImageHeader = class(TCustomChunkPng)
  private
    FWidth             : Integer;
    FHeight            : Integer;
    FBitDepth          : Byte;
    FColourType        : TColourType;
    FCompressionMethod : Byte;
    FFilterMethod      : Byte;
    FInterlaceMethod   : Byte;
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngImageData = class(TCustomChunkPng)
  private
    FData : TMemoryStream;
  public
    constructor Create; override;
    destructor Destroy; override;
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngPalette = class(TCustomChunkPng)
  private
    FPaletteEntries : array [0..255] of TRGB24;
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngGamma = class(TCustomChunkPng)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
    procedure SetGammaAsSingle(const Value: Single);
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle write SetGammaAsSingle;
  end;

  TChunkPngTime = class(TCustomChunkPng)
  private
    FYear   : Word;
    FMonth  : Byte;
    FDay    : Byte;
    FHour   : Byte;
    FMinute : Byte;
    FSecond : Byte;
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomChunkPng)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

  TCustomChunkPngTextChunk = class(TCustomChunkPng)
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
  public
    property Keyword: AnsiString read FKeyword write FKeyword;
    property Text: AnsiString read FText write FText;
  end;

  TChunkPngTextChunk = class(TCustomChunkPngTextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngCompressedTextChunk = class(TCustomChunkPngTextChunk)
  private
    FCompressionMethod : Byte;
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TChunkPngInternationalTextChunk = class(TCustomChunkPngTextChunk)
  private
    FCompressionMethod : Byte;
    FCompressionFlag   : Byte;
    FLanguageString    : AnsiString;
    FTranslatedKeyword : string;
  public
    class function GetClassChunkName : TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property CompressionFlag: Byte read FCompressionFlag write FCompressionFlag;
    property LanguageString: AnsiString read FLanguageString write FLanguageString;
    property TranslatedKeyword: string read FTranslatedKeyword write FTranslatedKeyword;
  end;

  TUnknownPngChunk = class(TUnknownChunk)
  public
    constructor Create; override;
  end;

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


  TPngImage = class(TGraphic)
  private
    procedure ReadUnknownChunk(Stream: TStream);
    procedure ReadPaletteChunk(Stream: TStream);
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    function CalculateCRC(Stream: TStream): Cardinal;
    procedure ReadPhysicalPixelDimensionChunk(Stream: TStream);
    procedure ReadTextChunk(Stream: TStream);
    procedure ReadImageDataChunk(Stream: TStream);
    procedure ReadGammaChunk(Stream: TStream);
    procedure ReadTimeChunk(Stream: TStream);
  protected
    FImageHeader         : TChunkPngImageHeader;
    FAdditionalChunkList : TChunkList;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function CanLoad(const FileName: TFileName): Boolean; overload;
    class function CanLoad(Stream: TStream): Boolean; overload;

    procedure LoadFromClipboardFormat(AFormat: Word; AData: Cardinal;
      APalette: HPALETTE); override;
    procedure SaveToClipboardFormat(var AFormat: Word; var AData: Cardinal;
      var APalette: HPALETTE); override;
  end;

implementation

uses
  DAV_Common;

resourcestring
  RCStrNotAValidPNGFile = 'Not a valid PNG file';
  RCStrWrongBitdepth = 'Wrong Bitdepth';
  RCStrUnsupportedCompressMethod = 'Unsupported compression method';
  RCStrUnsupportedFilterMethod = 'Unsupported filter method';
  RCStrUnsupportedInterlaceMethod = 'Unsupported interlace method';
  RCStrChunkSizeTooSmall = 'Chunk size too small!';
  {$IFDEF CheckCRC}
  RCStrCRCError = 'CRC Error';
  RCStrNotYetImplemented = 'Not yet implemented';
  RCStrIndexOutOfBounds = 'Index out of bounds (%d)';
  {$ENDIF}

type
  TCrcTable = array [0..255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable : PCrcTable;

{ TCustomChunkPng }

constructor TCustomChunkPng.Create;
begin
 inherited;
 ChunkFlags := [cfSizeFirst, cfReversedByteOrder];
end;


{ TChunkPngImageHeader }

class function TChunkPngImageHeader.GetClassChunkName: TChunkName;
begin
 Result := 'IHDR';
end;

procedure TChunkPngImageHeader.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 13
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read width
   Read(FWidth, 4);
   Flip32(FWidth);

   // read height
   Read(FHeight, 4);
   Flip32(FHeight);

   // read bit depth
   Read(FBitDepth, 1);

   // read colour type
   Read(FColourType, 1);

   // check consistency between colour type and bit depth
   case FColourType of
    ctGreyscale :
      if not (FBitDepth in [1, 2, 4, 8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
    ctTrueColour,
    ctGreyscaleAlpha,
    ctTrueColourAlpha :
      if not (FBitDepth in [8, 16]) then raise EPngError.Create(RCStrWrongBitdepth);
    ctIndexedColour :
      if not (FBitDepth in [1, 2, 4, 8]) then raise EPngError.Create(RCStrWrongBitdepth);
   end;

   // read compression method
   Read(FCompressionMethod, 1);

   // check for compression method
   if FCompressionMethod <> 0
    then raise EPngError.Create(RCStrUnsupportedCompressMethod);

   // read filter method
   Read(FFilterMethod, 1);

   // check for filter method
   if FFilterMethod <> 0
    then raise EPngError.Create(RCStrUnsupportedFilterMethod);

   // read interlace method
   Read(FInterlaceMethod, 1);

   // check for interlace method
   if not (FInterlaceMethod in [0, 1])
    then raise EPngError.Create(RCStrUnsupportedInterlaceMethod);
  end;
end;

procedure TChunkPngImageHeader.SaveToStream(Stream: TStream);
var
  TempInt : Integer;
begin
 inherited;

 with Stream do
  begin
   // write width
   TempInt := Swap32(FWidth);
   Write(TempInt, 4);

   // write height
   TempInt := Swap32(FHeight);
   Write(TempInt, 4);

   // write bit depth
   Write(FBitDepth, 1);

   // write colour type
   Write(FColourType, 1);

   // write compression method
   Write(FCompressionMethod, 1);

   // write filter method
   Write(FFilterMethod, 1);

   // write interlace method
   Write(FInterlaceMethod, 1);
  end;
end;


{ TChunkPngPalette }

class function TChunkPngPalette.GetClassChunkName: TChunkName;
begin
 Result := 'PLTE';
end;

procedure TChunkPngPalette.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < SizeOf(FPaletteEntries) + 8
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   Read(FPaletteEntries[0], SizeOf(FPaletteEntries));
  end;
end;

procedure TChunkPngPalette.SaveToStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Write(FPaletteEntries[0], SizeOf(FPaletteEntries));
  end;
end;


{ TChunkPngPhysicalPixelDimensions }

class function TChunkPngPhysicalPixelDimensions.GetClassChunkName: TChunkName;
begin
 Result := 'pHYs';
end;

procedure TChunkPngPhysicalPixelDimensions.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 13
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read pixels per unit, X axis
   Read(FPixelsPerUnitX, 4);
   Flip32(FPixelsPerUnitX);

   // read pixels per unit, Y axis
   Read(FPixelsPerUnitY, 4);
   Flip32(FPixelsPerUnitY);

   // read unit
   Read(FUnit, 1);
  end;
end;

procedure TChunkPngPhysicalPixelDimensions.SaveToStream(Stream: TStream);
var
  Temp : Cardinal;
begin
 FChunkSize := 13;

 inherited;

 with Stream do
  begin
   if Size < 13
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // write pixels per unit, X axis
   Temp := Swap32(FPixelsPerUnitX);
   Write(Temp, 4);

   // write pixels per unit, Y axis
   Temp := Swap32(FPixelsPerUnitY);
   Write(Temp, 4);

   // write unit
   Write(FUnit, 1);
  end;
end;


{ TChunkPngTextChunk }

class function TChunkPngTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'tEXt';
end;

procedure TChunkPngTextChunk.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read text
   Index := 1;
   SetLength(FText, Size - Position);
   while (Position < Size) do
    begin
     Read(FText[Index], SizeOf(Byte));
     Inc(Index);
    end;
  end;
end;

procedure TChunkPngTextChunk.SaveToStream(Stream: TStream);
var
  Temp  : Byte;
begin
 FChunkSize := Length(FKeyword) + Length(FText) + 1;

 inherited;

 with Stream do
  begin
   // write keyword
   Write(FKeyword[1], Length(FKeyword));

   // write separator
   Temp := 0;
   Write(Temp, 1);

   // write text
   Write(FText[1], Length(FText));
  end;
end;


{ TChunkPngCompressedTextChunk }

class function TChunkPngCompressedTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'zTXt';
end;

procedure TChunkPngCompressedTextChunk.LoadFromStream(Stream: TStream);
var
  Index    : Integer;
  DataIn   : Pointer;
  DataOut  : Pointer;
  DataSize : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression method
   Read(FCompressionMethod, SizeOf(Byte));

   // read text
   if FCompressionMethod = 0 then
    begin
     GetMem(DataIn, Size - Position);
     try
      Read(DataIn^, Size - Position);
      ZDecompress(DataIn, Size - Position, DataOut, DataSize);
      SetLength(FText, DataSize);
      Move(DataOut^, FText[1], DataSize);
     finally
      Dispose(DataIn);
     end;
    end;
  end;
end;

procedure TChunkPngCompressedTextChunk.SaveToStream(Stream: TStream);
var
  DataOut  : Pointer;
  DataSize : Integer;
  Temp     : Byte;
begin
 // compress text
 ZCompress(@FText[1], Length(FText), DataOut, DataSize);

 try
  // calculate chunk size
  FChunkSize := Length(FKeyword) + DataSize + 1;

  inherited;

  with Stream do
   begin
    // write keyword
    Write(FKeyword[1], Length(FKeyword));

    // write separator
    Temp := 0;
    Write(Temp, 1);

    // write text
    Write(FText[1], Length(FText));

    // write compression method
    Write(FCompressionMethod, SizeOf(Byte));

    // write text
    Write(DataOut^, DataSize);
   end;
 finally
  Dispose(DataOut);
 end;
end;


{ TChunkPngInternationalTextChunk }

class function TChunkPngInternationalTextChunk.GetClassChunkName: TChunkName;
begin
 Result := 'iTXt';
end;

procedure TChunkPngInternationalTextChunk.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FKeyword, 80);
   while (Position < Size) do
    begin
     Read(FKeyword[Index], SizeOf(Byte));
     if FKeyword[Index] = #0 then
      begin
       SetLength(FKeyword, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression flag
   Read(FCompressionFlag, SizeOf(Byte));

   // read compression method
   Read(FCompressionMethod, SizeOf(Byte));

   // read language string
   Index := 1;
   SetLength(FLanguageString, 10);
   while (Position < Size) do
    begin
     Read(FLanguageString[Index], SizeOf(Byte));
     if FLanguageString[Index] = #0 then
      begin
       SetLength(FLanguageString, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // yet todo!
   Exit;
  end;
end;

procedure TChunkPngInternationalTextChunk.SaveToStream(Stream: TStream);
begin
 raise EPngError.Create(RCStrNotYetImplemented);
end;


{ TChunkPngImageData }

constructor TChunkPngImageData.Create;
begin
 inherited;
 FData := TMemoryStream.Create;
end;

destructor TChunkPngImageData.Destroy;
begin
 FreeAndNil(FData);
 inherited;
end;

class function TChunkPngImageData.GetClassChunkName: TChunkName;
begin
 Result := 'IDAT';
end;

procedure TChunkPngImageData.LoadFromStream(Stream: TStream);
begin
 inherited;

 FData.CopyFrom(Stream, Stream.Size - Stream.Position);
end;

procedure TChunkPngImageData.SaveToStream(Stream: TStream);
begin
 FChunkSize := FData.Size;
 inherited;

 Stream.CopyFrom(FData, FChunkSize);
end;


{ TChunkPngTime }

class function TChunkPngTime.GetClassChunkName: TChunkName;
begin
 Result := 'tIME';
end;

procedure TChunkPngTime.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 7
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read year
   Read(FYear, SizeOf(word));
   Flip16(FYear);

   // read month
   Read(FMonth, SizeOf(Byte));

   // read day
   Read(FDay, SizeOf(Byte));

   // read hour
   Read(FHour, SizeOf(Byte));

   // read minute
   Read(FMinute, SizeOf(Byte));

   // read second
   Read(FSecond, SizeOf(Byte));
  end;
end;

procedure TChunkPngTime.SaveToStream(Stream: TStream);
var
  TempWord : Word;
begin
 FChunkSize := 7;

 inherited;

 with Stream do
  begin
   // write year
   TempWord := Swap16(FYear);
   Write(TempWord, SizeOf(word));

   // write month
   Write(FMonth, SizeOf(Byte));

   // write day
   Write(FDay, SizeOf(Byte));

   // write hour
   Write(FHour, SizeOf(Byte));

   // write minute
   Write(FMinute, SizeOf(Byte));

   // write second
   Write(FSecond, SizeOf(Byte));
  end;
end;


{ TChunkPngGamma }

class function TChunkPngGamma.GetClassChunkName: TChunkName;
begin
 Result := 'gAMA';
end;

function TChunkPngGamma.GetGammaAsSingle: Single;
begin
 Result := FGamma * 1E-6
end;

procedure TChunkPngGamma.SetGammaAsSingle(const Value: Single);
begin
 FGamma := Round(Value * 1E6);
end;

procedure TChunkPngGamma.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 4
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read gamma
   Read(FGamma, SizeOf(Integer));
   Flip32(FGamma);
  end;
end;

procedure TChunkPngGamma.SaveToStream(Stream: TStream);
var
  TempInt : Integer;
begin
 inherited;

 with Stream do
  begin
   // write gamma
   TempInt := Swap32(FGamma);
   Write(TempInt, SizeOf(Integer));
  end;
end;


{ TUnknownPngChunk }

constructor TUnknownPngChunk.Create;
begin
 inherited;
 ChunkFlags := [cfSizeFirst, cfReversedByteOrder];
end;


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
var
  Temp: Pointer;
begin
 if (Index < 0) or (Index >= Count)
  then raise EPngError.Create('Chunk list is empty');
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


{ TPngImage }

constructor TPngImage.Create;
begin
 FImageHeader         := TChunkPngImageHeader.Create;
 FAdditionalChunkList := TChunkList.Create;
end;

destructor TPngImage.Destroy;
begin
 FAdditionalChunkList.Clear;

 FreeAndNil(FAdditionalChunkList);
 FreeAndNil(FImageHeader);

 inherited;
end;

class function TPngImage.CanLoad(const FileName: TFileName): Boolean;
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

class function TPngImage.CanLoad(Stream: TStream): Boolean;
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

procedure TPngImage.LoadFromClipboardFormat(AFormat: Word; AData: Cardinal;
  APalette: HPALETTE);
begin
 with TBitmap.Create do
  try
   LoadFromClipboardFormat(AFormat, AData, APalette);
  finally
   Free;
  end;
end;

procedure TPngImage.SaveToClipboardFormat(var AFormat: Word;
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

procedure TPngImage.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Integer;
  ChunkCRC     : Cardinal;
  MemoryStream : TMemoryStream;
begin
 with Stream do
  begin
   // check for minimum file size
   if Size < 8
    then EPngError.Create(RCStrNotAValidPNGFile);

   // read chunk ID
   Read(ChunkName, 4);
   if ChunkName <> '‰PNG'
    then EPngError.Create(RCStrNotAValidPNGFile);

   // read PNG magic
   Read(ChunkName, 4);
   if ChunkName <> #$0D#$0A#$1A#$0A
    then EPngError.Create(RCStrNotAValidPNGFile);

   MemoryStream := TMemoryStream.Create;
   try
    // read image header chunk size
    Read(ChunkSize, 4);
    Flip32(ChunkSize);
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
      Read(ChunkSize, 4);
      Flip32(ChunkSize);
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
      if ChunkName = 'PLTE'
       then ReadPaletteChunk(MemoryStream) else
      if ChunkName = 'tEXt'
       then ReadTextChunk(MemoryStream) else
      if ChunkName = 'tIME'
       then ReadTimeChunk(MemoryStream) else
      if ChunkName = 'gAMA'
       then ReadGammaChunk(MemoryStream) else
      if ChunkName = 'pHYs'
       then ReadPhysicalPixelDimensionChunk(MemoryStream)
       else ReadUnknownChunk(MemoryStream);

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
end;

procedure TPngImage.SaveToStream(Stream: TStream);
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
   ChunkName := #$0D#$0A#$1A#$0A;
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

procedure TPngImage.ReadUnknownChunk(Stream: TStream);
var
  UnknownChunk : TUnknownPngChunk;
begin
 UnknownChunk := TUnknownPngChunk.Create;
 UnknownChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TPngImage.ReadImageDataChunk(Stream: TStream);
var
  ImageDataChunk : TChunkPngImageData;
begin
 ImageDataChunk := TChunkPngImageData.Create;
 ImageDataChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(ImageDataChunk);
end;

procedure TPngImage.ReadGammaChunk(Stream: TStream);
var
  GammaChunk : TChunkPngGamma;
begin
 GammaChunk := TChunkPngGamma.Create;
 GammaChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(GammaChunk);
end;

procedure TPngImage.ReadTimeChunk(Stream: TStream);
var
  TimeChunk : TChunkPngTime;
begin
 TimeChunk := TChunkPngTime.Create;
 TimeChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(TimeChunk);
end;

procedure TPngImage.ReadPaletteChunk(Stream: TStream);
var
  PaletteChunk : TChunkPngPalette;
begin
 PaletteChunk := TChunkPngPalette.Create;
 PaletteChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(PaletteChunk);
end;

procedure TPngImage.ReadPhysicalPixelDimensionChunk(Stream: TStream);
var
  PhysicalPixelDimensionChunk : TChunkPngPhysicalPixelDimensions;
begin
 PhysicalPixelDimensionChunk := TChunkPngPhysicalPixelDimensions.Create;
 PhysicalPixelDimensionChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(PhysicalPixelDimensionChunk);
end;

procedure TPngImage.ReadTextChunk(Stream: TStream);
var
  TextChunk : TChunkPngTextChunk;
begin
 TextChunk := TChunkPngTextChunk.Create;
 TextChunk.LoadFromStream(Stream);
 FAdditionalChunkList.Add(TextChunk);
end;

function TPngImage.CalculateCRC(Stream: TStream): Cardinal;
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

function TPngImage.CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
begin
 Result := CalculateCRC(Stream) = CRC;
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

finalization
  Dispose(GCrcTable);

end.
