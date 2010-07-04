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
    function CalculateCRC(Stream: TStream): Cardinal;
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
    procedure ReadImageDataChunk(Stream: TStream);
    procedure ReadUnknownChunk(Stream: TStream);
    procedure InterpreteChunks;
  protected
    FImageHeader         : TChunkPngImageHeader;
    FPaletteChunk        : TChunkPngPalette;
    FDataChunkList       : TChunkList;
    FAdditionalChunkList : TChunkList;
    procedure Clear; virtual;
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
  DAV_Common, DAV_GuiPngResourceStrings;

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
end;

destructor TPortableNetworkGraphic.Destroy;
begin
 FAdditionalChunkList.Clear;

 FreeAndNil(FAdditionalChunkList);
 FreeAndNil(FDataChunkList);
 FreeAndNil(FImageHeader);

 inherited;
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
         end else ReadUnknownChunk(MemoryStream);
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
