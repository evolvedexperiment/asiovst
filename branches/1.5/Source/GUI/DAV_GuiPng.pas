{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_GuiPng;

interface

{$I ..\DAV_Compiler.inc}
{$IFDEF CPUx86_64}
{$DEFINE PUREPASCAL}
{$ENDIF}

uses
{$IFDEF FPC} LCLIntf, {$ELSE} Windows, {$ENDIF} Classes, Graphics, SysUtils,
  DAV_Types, DAV_ChunkClasses, DAV_GuiCommon, DAV_GuiPixelMap,
  DAV_GuiPngTypes, DAV_GuiPngClasses, DAV_GuiFileFormats, DAV_GuiPngChunks;

type
  TTransferNonInterlaced = procedure(Source, Destination, Alpha: Pointer)
    of object;
  TTransferAdam7 = procedure(const Pass: Byte;
    Source, Destination, Alpha: Pointer) of object;

  TPortableNetworkGraphic = class(TGuiCustomFileFormat)
  private
    FCompressionLevel: Byte;
    function GetBitDepth: Byte;
    function GetColorType: TColorType;
    function GetCompressionMethod: Byte;
    function GetFilterMethod: TFilterMethod;
    function GetInterlaceMethod: TInterlaceMethod;
    function GetPaletteEntry(index: Integer): TRGB24;
    function GetPaletteEntryCount: Integer;
    function GetGamma: Single;
    function GetModifiedTime: TDateTime;
    function GetPixelsPerUnitX: Cardinal;
    function GetPixelsPerUnitY: Cardinal;
    function GetPixelUnit: Byte;
    procedure SetPixelsPerUnitX(const Value: Cardinal);
    procedure SetPixelsPerUnitY(const Value: Cardinal);
    procedure SetPixelUnit(const Value: Byte);
    procedure SetBitDepth(const Value: Byte);
    procedure SetChromaChunk(const Value: TChunkPngPrimaryChromaticities);
    procedure SetColorType(const Value: TColorType);
    procedure SetCompressionMethod(const Value: Byte);
    procedure SetCompressionLevel(const Value: Byte);
    procedure SetFilterMethod(const Value: TFilterMethod);
    procedure SetGamma(const Value: Single);
    procedure SetModifiedTime(const Value: TDateTime);
    procedure SetImageHeader(const Value: TChunkPngImageHeader);
    procedure SetInterlaceMethod(const Value: TInterlaceMethod);
    procedure SetGammaChunk(const Value: TChunkPngGamma);
    procedure SetPaletteChunk(const Value: TChunkPngPalette);
    procedure SetPhysicalDimensions(const Value
      : TChunkPngPhysicalPixelDimensions);
    procedure SetSignificantBits(const Value: TChunkPngSignificantBits);
    procedure SetTimeChunk(const Value: TChunkPngTime);

    function CalculateCRC(Buffer: PByte; Count: Cardinal): Cardinal; overload;
    function CalculateCRC(Stream: TStream): Cardinal; overload;
{$IFDEF CheckCRC}
    function CheckCRC(Stream: TStream; CRC: Cardinal): Boolean;
{$ENDIF}
    procedure ReadImageDataChunk(Stream: TStream);
    procedure ReadUnknownChunk(Stream: TStream);
  protected
    FImageHeader: TChunkPngImageHeader;
    FPaletteChunk: TChunkPngPalette;
    FGammaChunk: TChunkPngGamma;
    FTimeChunk: TChunkPngTime;
    FSignificantBits: TChunkPngSignificantBits;
    FPhysicalDimensions: TChunkPngPhysicalPixelDimensions;
    FChromaChunk: TChunkPngPrimaryChromaticities;
    FDataChunkList: TPngChunkList;
    FAdditionalChunkList: TPngChunkList;

    function GetHeight: Integer; override;
    function GetWidth: Integer; override;
    procedure SetWidth(const Value: Integer); override;
    procedure SetHeight(const Value: Integer); override;

    procedure Clear; virtual;

    procedure CopyImageData(Stream: TStream);
    procedure StoreImageData(Stream: TStream);
    procedure DecompressImageDataToStream(Stream: TStream);
    procedure CompressImageDataFromStream(Stream: TStream);

    procedure CompressionLevelChanged; virtual;

    class function CanHandleExtension(const FileName: TFileName)
      : Boolean; override;

    property ImageHeader: TChunkPngImageHeader read FImageHeader
      write SetImageHeader;
    property PaletteChunk: TChunkPngPalette read FPaletteChunk
      write SetPaletteChunk;
    property GammaChunk: TChunkPngGamma read FGammaChunk write SetGammaChunk;
    property TimeChunk: TChunkPngTime read FTimeChunk write SetTimeChunk;
    property PhysicalPixelDimensionsChunk: TChunkPngPhysicalPixelDimensions
      read FPhysicalDimensions write SetPhysicalDimensions;
  public
    constructor Create; override;
    destructor Destroy; override;

    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    class function CanLoad(const FileName: TFileName): Boolean;
      overload; override;
    class function CanLoad(Stream: TStream): Boolean; overload; override;

    function HasPhysicalPixelDimensionsInformation: Boolean;
    function HasGammaInformation: Boolean;
    function HasModifiedTimeInformation: Boolean;
    procedure RemovePhysicalPixelDimensionsInformation;
    procedure RemoveGammaInformation;
    procedure RemoveModifiedTimeInformation;
    procedure Assign(Source: TPersistent); override;
    procedure AssignTo(Dest: TPersistent); override;

    property Width: Integer read GetWidth write SetWidth;
    property Height: Integer read GetHeight write SetHeight;
    property BitDepth: Byte read GetBitDepth write SetBitDepth;
    property ColorType: TColorType read GetColorType write SetColorType;
    property CompressionMethod: Byte read GetCompressionMethod
      write SetCompressionMethod;
    property CompressionLevel: Byte read FCompressionLevel
      write SetCompressionLevel default 9;
    property FilterMethod: TFilterMethod read GetFilterMethod
      write SetFilterMethod;
    property InterlaceMethod: TInterlaceMethod read GetInterlaceMethod
      write SetInterlaceMethod;
    property PaletteEntry[index: Integer]: TRGB24 read GetPaletteEntry;
    property PaletteEntryCount: Integer read GetPaletteEntryCount;
    property Gamma: Single read GetGamma write SetGamma;
    property ModifiedTime: TDateTime read GetModifiedTime write SetModifiedTime;
    property PixelsPerUnitX: Cardinal read GetPixelsPerUnitX
      write SetPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read GetPixelsPerUnitY
      write SetPixelsPerUnitY;
    property PixelUnit: Byte read GetPixelUnit write SetPixelUnit;

    property SignificantBitsChunk: TChunkPngSignificantBits
      read FSignificantBits write SetSignificantBits;
    property PrimaryChromaticitiesChunk: TChunkPngPrimaryChromaticities
      read FChromaChunk write SetChromaChunk;
  end;

  TPngStorageFormat = (psfAuto, psfGrayscale, psfPalette, psfRGB, psfRGBA);

  TPortableNetworkGraphicPixel32 = class(TPortableNetworkGraphic)
  private
    FStorageFormat: TPngStorageFormat;
    procedure AssignPropertiesFromPixelMap(PixelMap: TGuiCustomPixelMap);
    procedure SetStorageFormat(const Value: TPngStorageFormat);
  protected
    procedure ReadData(Stream: TStream); virtual;
    procedure WriteData(Stream: TStream); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function PixelmapScanline(Bitmap: TObject; Y: Integer): Pointer; virtual;
  public
    constructor Create; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure Assign(Source: TPersistent); override;

    procedure DrawToPixelMap(PixelMap: TGuiCustomPixelMap); virtual;

    property StorageFormat: TPngStorageFormat read FStorageFormat
      write SetStorageFormat default psfAuto;
  end;

implementation

uses
  DAV_Common, DAV_GuiPngCoder, DAV_GuiPngResourceStrings;

type
  TCrcTable = array [0 .. 255] of Cardinal;
  PCrcTable = ^TCrcTable;

var
  GCrcTable: PCrcTable;

const
  CPngMagic = #$0D#$0A#$1A#$0A;

  CRowStart: array [0 .. 6] of Integer = (0, 0, 4, 0, 2, 0, 1);
  CColumnStart: array [0 .. 6] of Integer = (0, 4, 0, 2, 0, 1, 0);
  CRowIncrement: array [0 .. 6] of Integer = (8, 8, 8, 4, 4, 2, 2);
  CColumnIncrement: array [0 .. 6] of Integer = (8, 8, 4, 4, 2, 2, 1);

  { TPortableNetworkGraphic }

constructor TPortableNetworkGraphic.Create;
begin
  FImageHeader := TChunkPngImageHeader.Create;
  FDataChunkList := TPngChunkList.Create;
  FAdditionalChunkList := TPngChunkList.Create;
  FCompressionLevel := 9;
  inherited;
end;

destructor TPortableNetworkGraphic.Destroy;
begin
  FAdditionalChunkList.Clear;

  FreeAndNil(FAdditionalChunkList);
  FreeAndNil(FDataChunkList);
  FreeAndNil(FImageHeader);

  // free palette chunk
  if Assigned(FPaletteChunk) then
    FreeAndNil(FPaletteChunk);

  // free gamma chunk
  if Assigned(FGammaChunk) then
    FreeAndNil(FGammaChunk);

  // free time chunk
  if Assigned(FTimeChunk) then
    FreeAndNil(FTimeChunk);

  // free time chunk
  if Assigned(FSignificantBits) then
    FreeAndNil(FSignificantBits);

  // free physical pixel dimensions chunk
  if Assigned(FPhysicalDimensions) then
    FreeAndNil(FPhysicalDimensions);

  // free primary chromaticities chunk
  if Assigned(FChromaChunk) then
    FreeAndNil(FChromaChunk);

  inherited;
end;

procedure TPortableNetworkGraphic.SetPaletteChunk
  (const Value: TChunkPngPalette);
begin
  if Assigned(FPaletteChunk) then
    if Assigned(Value) then
      FPaletteChunk.Assign(Value)
    else
      FreeAndNil(FPaletteChunk)
  else if Assigned(Value) then
  begin
    FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
    FPaletteChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetPhysicalDimensions
  (const Value: TChunkPngPhysicalPixelDimensions);
begin
  if Assigned(FPhysicalDimensions) then
    if Assigned(Value) then
      FPhysicalDimensions.Assign(Value)
    else
      FreeAndNil(FPhysicalDimensions)
  else if Assigned(Value) then
  begin
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create
      (FImageHeader);
    FPhysicalDimensions.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetSignificantBits
  (const Value: TChunkPngSignificantBits);
begin
  if Assigned(FSignificantBits) then
    if Assigned(Value) then
      FSignificantBits.Assign(Value)
    else
      FreeAndNil(FSignificantBits)
  else if Assigned(Value) then
  begin
    FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
    FSignificantBits.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetTimeChunk(const Value: TChunkPngTime);
begin
  if Assigned(FTimeChunk) then
    if Assigned(Value) then
      FTimeChunk.Assign(Value)
    else
      FreeAndNil(FTimeChunk)
  else if Assigned(Value) then
  begin
    FTimeChunk := TChunkPngTime.Create(FImageHeader);
    FTimeChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitX(const Value: Cardinal);
begin
  if Value = 0 then
    raise EPngError.Create(RCStrWrongPixelPerUnit);

  if not Assigned(FPhysicalDimensions) then
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create
      (FImageHeader);

  FPhysicalDimensions.PixelsPerUnitX := Value;
end;

procedure TPortableNetworkGraphic.SetPixelsPerUnitY(const Value: Cardinal);
begin
  if Value = 0 then
    raise EPngError.Create(RCStrWrongPixelPerUnit);

  if not Assigned(FPhysicalDimensions) then
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create
      (FImageHeader);

  FPhysicalDimensions.PixelsPerUnitY := Value;
end;

procedure TPortableNetworkGraphic.SetPixelUnit(const Value: Byte);
begin
  if Value > 1 then
    raise EPngError.Create(RCStrUnspecifiedPixelUnit);

  if not Assigned(FPhysicalDimensions) then
    FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create
      (FImageHeader);

  FPhysicalDimensions.PixelUnit := Value;
end;

procedure TPortableNetworkGraphic.SetChromaChunk(const Value
  : TChunkPngPrimaryChromaticities);
begin
  if Assigned(FChromaChunk) then
    if Assigned(Value) then
      FChromaChunk.Assign(Value)
    else
      FreeAndNil(FChromaChunk)
  else if Assigned(Value) then
  begin
    FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
    FChromaChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetGammaChunk(const Value: TChunkPngGamma);
begin
  if Assigned(FGammaChunk) then
    if Assigned(Value) then
      FGammaChunk.Assign(Value)
    else
      FreeAndNil(FGammaChunk)
  else if Assigned(Value) then
  begin
    FGammaChunk := TChunkPngGamma.Create(FImageHeader);
    FGammaChunk.Assign(Value);
  end;
end;

procedure TPortableNetworkGraphic.SetImageHeader(const Value
  : TChunkPngImageHeader);
begin
  if not Assigned(Value) then
    raise EPngError.Create(RCStrNewHeaderError)
  else
    FImageHeader.Assign(Value);
end;

procedure TPortableNetworkGraphic.SetBitDepth(const Value: Byte);
begin
  raise EPngError.Create(RCStrBitDepthTranscodingError);
end;

procedure TPortableNetworkGraphic.SetColorType(const Value: TColorType);
begin
  raise EPngError.Create(RCStrColorTypeTranscodingError);
end;

procedure TPortableNetworkGraphic.SetCompressionLevel(const Value: Byte);
begin
  if not(Value in [1 .. 9]) then
    raise EPngError.Create(RCStrInvalidCompressionLevel);

  if FCompressionLevel <> Value then
  begin
    FCompressionLevel := Value;
    CompressionLevelChanged;
  end;
end;

procedure TPortableNetworkGraphic.SetCompressionMethod(const Value: Byte);
begin
  raise EPngError.Create(RCStrDirectCompressionMethodSetError);
end;

procedure TPortableNetworkGraphic.SetFilterMethod(const Value: TFilterMethod);
begin
  raise EPngError.Create(RCStrDirectFilterMethodSetError);
end;

procedure TPortableNetworkGraphic.SetWidth(const Value: Integer);
begin
  raise EPngError.Create(RCStrDirectWidthSetError);
end;

procedure TPortableNetworkGraphic.SetInterlaceMethod
  (const Value: TInterlaceMethod);
begin
  raise EPngError.Create(RCStrDirectInterlaceMethodSetError);
end;

procedure TPortableNetworkGraphic.SetModifiedTime(const Value: TDateTime);
begin
  if Assigned(FTimeChunk) then
    FTimeChunk.ModifiedDateTime := Value;
end;

procedure TPortableNetworkGraphic.SetGamma(const Value: Single);
begin
  raise EPngError.Create(RCStrDirectGammaSetError);
end;

procedure TPortableNetworkGraphic.SetHeight(const Value: Integer);
begin
  raise EPngError.Create(RCStrDirectHeightSetError);
end;

procedure TPortableNetworkGraphic.CopyImageData(Stream: TStream);
var
  DataIndex: Integer;
begin
  // combine all data chunks first
  for DataIndex := 0 to FDataChunkList.Count - 1 do
  begin
    // make sure the chunk is inded an image data chunk
    Assert(FDataChunkList[DataIndex] is TChunkPngImageData);

    // concat current chunk to data stream
    with TChunkPngImageData(FDataChunkList[DataIndex]) do
    begin
      Data.Seek(0, soFromBeginning);
      Stream.CopyFrom(Data, Data.Size);
    end;
  end;
end;

procedure TPortableNetworkGraphic.StoreImageData(Stream: TStream);
var
  DataChunk: TChunkPngImageData;
  ChunkSize: Integer;
begin
  // delete old image data
  FDataChunkList.Clear;

  ChunkSize := Stream.Size;
  while Stream.Position < Stream.Size do
  begin
    DataChunk := TChunkPngImageData.Create(ImageHeader);

    if (Stream.Size - Stream.Position) < ChunkSize then
      ChunkSize := (Stream.Size - Stream.Position);

    // copy data to IDAT chunk
    DataChunk.Data.CopyFrom(Stream, ChunkSize);

    // add data chunk to data chunk list
    FDataChunkList.Add(DataChunk);
  end;
end;

procedure TPortableNetworkGraphic.DecompressImageDataToStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // copy image data from all data chunks to one continous data stream
    CopyImageData(DataStream);

    // check whether compression method is supported
    if FImageHeader.CompressionMethod <> 0 then
      raise EPngError.Create(RCStrUnsupportedCompressionMethod);

    // reset data stream position to zero
    DataStream.Seek(0, soFromBeginning);

    // decompress z-stream
    ZDecompress(DataStream, Stream);
  finally
    FreeAndNil(DataStream);
  end;
end;

procedure TPortableNetworkGraphic.CompressImageDataFromStream(Stream: TStream);
var
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // set compression method
    FImageHeader.CompressionMethod := 0;

    // compress Stream to DataStream
    if Stream is TMemoryStream then
      ZCompress(TMemoryStream(Stream), DataStream, FCompressionLevel)
    else
      raise EPngError.Create(RCStrNotYetImplemented);

    // reset data stream position to zero
    DataStream.Seek(0, soFromBeginning);

    // copy image data from all data chunks to one continous data stream
    StoreImageData(DataStream);
  finally
    FreeAndNil(DataStream);
  end;
end;

class function TPortableNetworkGraphic.CanLoad(const FileName: TFileName): Boolean;
var
  FileStream: TFileStream;
begin
  FileStream := TFileStream.Create(FileName, fmOpenRead);
  with FileStream do
    try
      Result := CanLoad(FileStream);
    finally
      Free;
    end;
end;

class function TPortableNetworkGraphic.CanHandleExtension(const FileName: TFileName): Boolean;
begin
  Result := Pos('png', FileName) >= 0;
end;

class function TPortableNetworkGraphic.CanLoad(Stream: TStream): Boolean;
var
  ChunkID: TChunkName;
begin
  Result := Stream.Size >= 4;

  if Result then
  begin
    Stream.Read(ChunkID, 4);
    Stream.Seek(-4, soFromCurrent);
    Result := ChunkID = '‰PNG';
  end;
end;

procedure TPortableNetworkGraphic.LoadFromStream(Stream: TStream);
var
  ChunkName: TChunkName;
  ChunkSize: Integer;
  ChunkCRC: Cardinal;
  ChunkClass: TCustomChunkPngWithHeaderClass;
  Chunk: TCustomChunkPngWithHeader;
  MemoryStream: TMemoryStream;
begin
  with Stream do
  begin
    Clear;

    // check for minimum file size
    if Size < 8 then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // read chunk ID
    Read(ChunkName, 4);
    if ChunkName <> '‰PNG' then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    // read PNG magic
    Read(ChunkName, 4);
    if ChunkName <> CPngMagic then
      raise EPngError.Create(RCStrNotAValidPNGFile);

    MemoryStream := TMemoryStream.Create;
    try
      // read image header chunk size
      ChunkSize := ReadSwappedCardinal(Stream);
      if ChunkSize > Stream.Size - 12 then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      // read image header chunk ID
      Read(ChunkName, 4);
      if ChunkName <> 'IHDR' then
        raise EPngError.Create(RCStrNotAValidPNGFile);

      // reset position to the chunk start and copy stream to memory
      Seek(-8, soCurrent);
      MemoryStream.CopyFrom(Stream, ChunkSize + 8);
      MemoryStream.Seek(0, soFromBeginning);

      // load image header
      FImageHeader.LoadFromStream(MemoryStream);

      // read image header chunk size
      Read(ChunkCRC, 4);
{$IFDEF CheckCRC}
      if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
        raise EPngError.Create(RCStrCRCError);
{$ENDIF}
      while Stream.Position < Stream.Size do
      begin
        // read image header chunk size
        ChunkSize := ReadSwappedCardinal(Stream);
        if ChunkSize > Stream.Size - Stream.Position - 4 then
          raise EPngError.Create(RCStrNotAValidPNGFile);

        // read chunk ID
        Read(ChunkName, 4);

        // check for stream end
        if ChunkName = 'IEND' then
        begin
          // read image header chunk size
          Read(ChunkCRC, 4);

{$IFDEF CheckCRC}
          if ChunkCRC <> 2187346606 then
            raise EPngError.Create(RCStrCRCError);
{$ENDIF}
          Break;
        end;

{$IFNDEF LinearStream}
        // reset position to the chunk start and copy stream to memory
        Seek(-8, soCurrent);
{$ENDIF}
        MemoryStream.Clear;
{$IFDEF LinearStream}
        WriteSwappedCardinal(MemoryStream, ChunkSize);
        MemoryStream.Write(ChunkName, 4);
        MemoryStream.CopyFrom(Stream, ChunkSize);
{$ELSE}
        MemoryStream.CopyFrom(Stream, ChunkSize + 8);
{$ENDIF}
        // reset memory stream to beginning of the chunk
        MemoryStream.Seek(0, soFromBeginning);

        if ChunkName = 'IHDR' then
          raise EPngError.Create(RCStrNotAValidPNGFile)
        else if ChunkName = 'IDAT' then
          ReadImageDataChunk(MemoryStream)
        else if ChunkName = 'gAMA' then
        begin
          if Assigned(FGammaChunk) then
            raise EPngError.Create(RCStrSeveralGammaChunks);
          FGammaChunk := TChunkPngGamma.Create(FImageHeader);
          FGammaChunk.LoadFromStream(MemoryStream);
        end
        else if ChunkName = 'cHRM' then
        begin
          if Assigned(FChromaChunk) then
            raise EPngError.Create(RCStrSeveralChromaChunks);
          FChromaChunk := TChunkPngPrimaryChromaticities.Create(FImageHeader);
          FChromaChunk.LoadFromStream(MemoryStream);
        end
        else if ChunkName = 'tIME' then
        begin
          if Assigned(FTimeChunk) then
            raise EPngError.Create(RCStrSeveralTimeChunks);
          FTimeChunk := TChunkPngTime.Create(FImageHeader);
          FTimeChunk.LoadFromStream(MemoryStream);
        end
        else if ChunkName = 'sBIT' then
        begin
          if Assigned(FSignificantBits) then
            raise EPngError.Create(RCStrSeveralSignificantBitsChunksFound);
          FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
          FSignificantBits.LoadFromStream(MemoryStream);
        end
        else if ChunkName = 'pHYs' then
        begin
          if Assigned(FPhysicalDimensions) then
            raise EPngError.Create(RCStrSeveralPhysicalPixelDimensionChunks);
          FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create
            (FImageHeader);
          FPhysicalDimensions.LoadFromStream(MemoryStream);
        end
        else if ChunkName = 'PLTE' then
        begin
          if Assigned(FPaletteChunk) then
            raise EPngError.Create(RCStrSeveralPaletteChunks);
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
            if (Byte(ChunkName[0]) and $80) = 0 then
              ReadUnknownChunk(MemoryStream)
            else
              raise EPngError.Create(RCStrAncillaryUnknownChunk);
          end;
        end;

        // read & check CRC
        Read(ChunkCRC, 4);
{$IFDEF CheckCRC}
        if not CheckCRC(MemoryStream, Swap32(ChunkCRC)) then
          raise EPngError.Create(RCStrCRCError);
{$ENDIF}
      end;
    finally
      FreeAndNil(MemoryStream);
    end;
  end;
end;

procedure TPortableNetworkGraphic.SaveToStream(Stream: TStream);
var
  ChunkName: TChunkName;
  CRC: Cardinal;
  MemoryStream: TMemoryStream;
  index: Integer;

  procedure SaveChunkToStream(Chunk: TDavCustomChunk);
  begin
    MemoryStream.Clear;
    Chunk.SaveToStream(MemoryStream);

    // copy memory stream to stream
    MemoryStream.Seek(0, soFromBeginning);
    Stream.CopyFrom(MemoryStream, MemoryStream.Size);

    // calculate and write CRC
    CRC := Swap32(CalculateCRC(MemoryStream));
    Write(CRC, SizeOf(Cardinal));
  end;

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
      CRC := Swap32(CalculateCRC(MemoryStream));
      Write(CRC, SizeOf(Cardinal));

      // eventually save physical pixel dimensions chunk
      if Assigned(FPhysicalDimensions) then
      begin
        MemoryStream.Clear;
        FPhysicalDimensions.SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;

      // eventually save significant bits chunk
      if Assigned(FSignificantBits) then
      begin
        MemoryStream.Clear;
        FSignificantBits.SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;

      // eventually save gamma chunk
      if Assigned(FGammaChunk) then
      begin
        MemoryStream.Clear;
        FGammaChunk.SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;

      // eventually save chroma chunk
      if Assigned(FChromaChunk) then
      begin
        MemoryStream.Clear;
        FChromaChunk.SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;

      // eventually save palette chunk
      if Assigned(FPaletteChunk) then
      begin
        MemoryStream.Clear;
        FPaletteChunk.SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;

      // store additional chunks
      for Index := 0 to FAdditionalChunkList.Count - 1 do
      begin
        MemoryStream.Clear;
        TDavDefinedChunk(FAdditionalChunkList[Index])
          .SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;

      // save data streams
      for Index := 0 to FDataChunkList.Count - 1 do
      begin
        MemoryStream.Clear;
        TDavDefinedChunk(FDataChunkList[Index]).SaveToStream(MemoryStream);

        // copy memory stream to stream
        MemoryStream.Seek(0, soFromBeginning);
        Stream.CopyFrom(MemoryStream, MemoryStream.Size);

        // calculate and write CRC
        CRC := Swap32(CalculateCRC(MemoryStream));
        Write(CRC, SizeOf(Cardinal));
      end;
    finally
      FreeAndNil(MemoryStream);
    end;

    // write chunk size
    WriteSwappedCardinal(Stream, 0);

    // write chunk ID
    ChunkName := 'IEND';
    Write(ChunkName, 4);

    // write CRC
    CRC := 2187346606;
    Write(CRC, 4);
  end;
end;

procedure TPortableNetworkGraphic.ReadUnknownChunk(Stream: TStream);
var
  UnknownChunk: TUnknownPngChunk;
begin
  UnknownChunk := TUnknownPngChunk.Create;
  UnknownChunk.LoadFromStream(Stream);
  FAdditionalChunkList.Add(UnknownChunk);
end;

procedure TPortableNetworkGraphic.RemoveGammaInformation;
begin
  if Assigned(FGammaChunk) then
    FreeAndNil(FGammaChunk);
end;

procedure TPortableNetworkGraphic.RemoveModifiedTimeInformation;
begin
  if Assigned(FTimeChunk) then
    FreeAndNil(FTimeChunk);
end;

procedure TPortableNetworkGraphic.RemovePhysicalPixelDimensionsInformation;
begin
  if Assigned(FPhysicalDimensions) then
    FreeAndNil(FPhysicalDimensions);
end;

procedure TPortableNetworkGraphic.CompressionLevelChanged;
var
  TempStream: TMemoryStream;
begin
  TempStream := TMemoryStream.Create;
  try
    DecompressImageDataToStream(TempStream);
    TempStream.Seek(0, soFromBeginning);
    CompressImageDataFromStream(TempStream);
  finally
    FreeAndNil(TempStream);
  end;
end;

procedure TPortableNetworkGraphic.ReadImageDataChunk(Stream: TStream);
var
  ImageDataChunk: TChunkPngImageData;
begin
  ImageDataChunk := TChunkPngImageData.Create(FImageHeader);
  ImageDataChunk.LoadFromStream(Stream);
  FDataChunkList.Add(ImageDataChunk);
end;

procedure TPortableNetworkGraphic.Assign(Source: TPersistent);
begin
  if Source is TPortableNetworkGraphic then
    with TPortableNetworkGraphic(Source) do
    begin
      if Assigned(Self.FImageHeader) then
        Self.FImageHeader.Assign(FImageHeader);

      // assign palette chunk
      if Assigned(Self.FPaletteChunk) then
        if Assigned(FPaletteChunk) then
          Self.FPaletteChunk.Assign(FPaletteChunk)
        else
          FreeAndNil(Self.FPaletteChunk)
      else if Assigned(FPaletteChunk) then
      begin
        Self.FPaletteChunk := TChunkPngPalette.Create(FImageHeader);
        Self.FPaletteChunk.Assign(FPaletteChunk);
      end;

      // assign gamma chunk
      if Assigned(Self.FGammaChunk) then
        if Assigned(FGammaChunk) then
          Self.FGammaChunk.Assign(FGammaChunk)
        else
          FreeAndNil(Self.FGammaChunk)
      else if Assigned(FGammaChunk) then
      begin
        Self.FGammaChunk := TChunkPngGamma.Create(FImageHeader);
        Self.FGammaChunk.Assign(FGammaChunk);
      end;

      // assign time chunk
      if Assigned(Self.FTimeChunk) then
        if Assigned(FTimeChunk) then
          Self.FTimeChunk.Assign(FTimeChunk)
        else
          FreeAndNil(Self.FTimeChunk)
      else if Assigned(FTimeChunk) then
      begin
        Self.FTimeChunk := TChunkPngTime.Create(FImageHeader);
        Self.FTimeChunk.Assign(FTimeChunk);
      end;

      // assign significant bits
      if Assigned(Self.FSignificantBits) then
        if Assigned(FSignificantBits) then
          Self.FSignificantBits.Assign(FSignificantBits)
        else
          FreeAndNil(Self.FSignificantBits)
      else if Assigned(FSignificantBits) then
      begin
        Self.FSignificantBits := TChunkPngSignificantBits.Create(FImageHeader);
        Self.FSignificantBits.Assign(FSignificantBits);
      end;

      // assign physical dimensions
      if Assigned(Self.FPhysicalDimensions) then
        if Assigned(FPhysicalDimensions) then
          Self.FPhysicalDimensions.Assign(FPhysicalDimensions)
        else
          FreeAndNil(Self.FPhysicalDimensions)
      else if Assigned(FPhysicalDimensions) then
      begin
        Self.FPhysicalDimensions := TChunkPngPhysicalPixelDimensions.Create
          (FImageHeader);
        Self.FPhysicalDimensions.Assign(FPhysicalDimensions);
      end;

      // assign primary chromaticities
      if Assigned(Self.FChromaChunk) then
        if Assigned(FChromaChunk) then
          Self.FChromaChunk.Assign(FChromaChunk)
        else
          FreeAndNil(Self.FChromaChunk)
      else if Assigned(FChromaChunk) then
      begin
        Self.FChromaChunk := TChunkPngPrimaryChromaticities.Create
          (FImageHeader);
        Self.FChromaChunk.Assign(FChromaChunk);
      end;

      if Assigned(Self.FDataChunkList) then
        Self.FDataChunkList.Assign(FDataChunkList);
      if Assigned(Self.FAdditionalChunkList) then
        Self.FAdditionalChunkList.Assign(FAdditionalChunkList);
    end
  else
    inherited;
end;

procedure TPortableNetworkGraphic.AssignTo(Dest: TPersistent);
begin
  if Dest is TPortableNetworkGraphic then
    with TPortableNetworkGraphic(Dest) do
    begin
      FImageHeader.Assign(Self.FImageHeader);
      FPaletteChunk.Assign(Self.FPaletteChunk);
      FGammaChunk.Assign(Self.FGammaChunk);
      FTimeChunk.Assign(Self.FTimeChunk);
      FSignificantBits.Assign(Self.FSignificantBits);
      FPhysicalDimensions.Assign(Self.FPhysicalDimensions);
      FChromaChunk.Assign(Self.FChromaChunk);
      FDataChunkList.Assign(Self.FDataChunkList);
      FAdditionalChunkList.Assign(Self.FAdditionalChunkList);
    end
  else
    inherited;
end;

function TPortableNetworkGraphic.CalculateCRC(Stream: TStream): Cardinal;
var
  CrcValue: Cardinal;
  Value: Byte;
begin
  if Stream is TMemoryStream then
    Result := CalculateCRC(TMemoryStream(Stream).Memory, Stream.Size)
  else
    with Stream do
    begin
      Seek(4, soFromBeginning);

      // initialize CRC
      CrcValue := $FFFFFFFF;

      while Position < Size do
      begin
        Read(Value, 1);

        CrcValue := GCrcTable^[(CrcValue xor Value) and $FF]
          xor (CrcValue shr 8);
      end;

      Result := (CrcValue xor $FFFFFFFF);

      Seek(0, soFromBeginning);
    end;
end;

function TPortableNetworkGraphic.CalculateCRC(Buffer: PByte; Count: Cardinal)
  : Cardinal;
{$IFDEF PUREPASCAL}
var
  CrcValue: Cardinal;
  Pos: Cardinal;
begin
  // ignore size (offset by 4 bytes)
  Pos := 4;
  Inc(Buffer, 4);

  // initialize CRC
  CrcValue := $FFFFFFFF;

  while Pos < Count do
  begin
    CrcValue := GCrcTable^[(CrcValue xor Buffer^) and $FF] xor (CrcValue shr 8);
    Inc(Buffer);
    Inc(Pos);
  end;

  Result := (CrcValue xor $FFFFFFFF);
{$ELSE}
asm
  PUSH    EBX
  PUSH    EDI
  ADD     EDX, 4
  SUB     ECX, 4
  JS      @Done
  NEG     ECX
  MOV     EBX, $FFFFFFFF

  MOV     EDI, [GCrcTable]

@Start:
  MOVZX   EAX, [EDX]
  XOR     EAX, EBX
  AND     EAX, $FF
  MOV     EAX, [EDI + 4 * EAX]
  SHR     EBX, 8
  XOR     EAX, EBX
  MOV     EBX, EAX

  INC     EDX
  INC     ECX
  JS      @Start

  XOR     EBX, $FFFFFFFF
  MOV     Result, EBX

@Done:
  POP     EDI
  POP     EBX
  {$ENDIF}
end;

{$IFDEF CheckCRC}

function TPortableNetworkGraphic.CheckCRC(Stream: TStream;
  CRC: Cardinal): Boolean;
begin
  Result := CalculateCRC(Stream) = CRC;
end;
{$ENDIF}

function TPortableNetworkGraphic.GetBitDepth: Byte;
begin
  Result := FImageHeader.BitDepth;
end;

function TPortableNetworkGraphic.GetColorType: TColorType;
begin
  Result := FImageHeader.ColorType;
end;

function TPortableNetworkGraphic.GetCompressionMethod: Byte;
begin
  Result := FImageHeader.CompressionMethod;
end;

function TPortableNetworkGraphic.GetFilterMethod: TFilterMethod;
begin
  Result := FImageHeader.FilterMethod;
end;

function TPortableNetworkGraphic.GetGamma: Single;
begin
  if Assigned(FGammaChunk) then
    Result := FGammaChunk.GammaAsSingle
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetHeight: Integer;
begin
  Result := FImageHeader.Height;
end;

function TPortableNetworkGraphic.GetInterlaceMethod: TInterlaceMethod;
begin
  Result := FImageHeader.InterlaceMethod;
end;

function TPortableNetworkGraphic.GetModifiedTime: TDateTime;
begin
  if Assigned(FTimeChunk) then
    with FTimeChunk do
      Result := EncodeDate(Year, Month, Day) + EncodeTime(Hour, Minute,
        Second, 0)
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetPaletteEntry(index: Integer): TRGB24;
begin
  if Assigned(FPaletteChunk) then
    Result := FPaletteChunk.PaletteEntry[Index]
  else
    raise EPngError.CreateFmt(RCStrIndexOutOfBounds, [Index]);
end;

function TPortableNetworkGraphic.GetPaletteEntryCount: Integer;
begin
  if Assigned(FPaletteChunk) then
    Result := FPaletteChunk.Count
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitX: Cardinal;
begin
  if Assigned(FPhysicalDimensions) then
    Result := FPhysicalDimensions.PixelsPerUnitX
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetPixelsPerUnitY: Cardinal;
begin
  if Assigned(FPhysicalDimensions) then
    Result := FPhysicalDimensions.PixelsPerUnitY
  else
    Result := 1;
end;

function TPortableNetworkGraphic.GetPixelUnit: Byte;
begin
  if Assigned(FPhysicalDimensions) then
    Result := FPhysicalDimensions.PixelUnit
  else
    Result := 0;
end;

function TPortableNetworkGraphic.GetWidth: Integer;
begin
  Result := FImageHeader.Width;
end;

function TPortableNetworkGraphic.HasGammaInformation: Boolean;
begin
  Result := Assigned(FGammaChunk);
end;

function TPortableNetworkGraphic.HasModifiedTimeInformation: Boolean;
begin
  Result := Assigned(FTimeChunk);
end;

function TPortableNetworkGraphic.HasPhysicalPixelDimensionsInformation: Boolean;
begin
  Result := Assigned(FPhysicalDimensions);
end;

procedure TPortableNetworkGraphic.Clear;
begin
  // clear chunk lists
  FDataChunkList.Clear;
  FAdditionalChunkList.Clear;

  // reset image header to default
  FImageHeader.ResetToDefault;

  // free palette chunk
  if Assigned(FPaletteChunk) then
    FreeAndNil(FPaletteChunk);

  // free gamma chunk
  if Assigned(FGammaChunk) then
    FreeAndNil(FGammaChunk);

  // free gamma chunk
  if Assigned(FChromaChunk) then
    FreeAndNil(FChromaChunk);

  // free time chunk
  if Assigned(FTimeChunk) then
    FreeAndNil(FTimeChunk);

  // free time chunk
  if Assigned(FSignificantBits) then
    FreeAndNil(FSignificantBits);

  // free physical pixel dimensions chunk
  if Assigned(FPhysicalDimensions) then
    FreeAndNil(FPhysicalDimensions);
end;

{ TPortableNetworkGraphicPixel32 }

constructor TPortableNetworkGraphicPixel32.Create;
begin
  inherited;
  FStorageFormat := psfAuto;
end;

procedure TPortableNetworkGraphicPixel32.Assign(Source: TPersistent);
var
  EncoderClass: TCustomPngEncoderClass;
  DataStream: TMemoryStream;
begin
  if Source is TGuiCustomPixelMap then
    with TGuiCustomPixelMap(Source) do
    begin
      // Assign
      AssignPropertiesFromPixelMap(TGuiCustomPixelMap(Source));

      case ImageHeader.ColorType of
        ctGrayscale:
          case ImageHeader.BitDepth of
            1:
              EncoderClass := TPngBGRAGrayscale1bitEncoder;
            2:
              EncoderClass := TPngBGRAGrayscale2bitEncoder;
            4:
              EncoderClass := TPngBGRAGrayscale4bitEncoder;
            8:
              EncoderClass := TPngBGRAGrayscale8bitEncoder;
          else
            raise EPngError.Create(RCStrUnsupportedFormat);
          end;
        ctTrueColor:
          EncoderClass := TPngBGRATrueColor8bitEncoder;
        ctIndexedColor:
          case ImageHeader.BitDepth of
            1:
              EncoderClass := TPngBGRAPalette1bitEncoder;
            2:
              EncoderClass := TPngBGRAPalette2bitEncoder;
            4:
              EncoderClass := TPngBGRAPalette4bitEncoder;
            8:
              EncoderClass := TPngBGRAPalette8bitEncoder;
          else
            raise EPngError.Create(RCStrUnsupportedFormat);
          end;
        ctGrayscaleAlpha:
          EncoderClass := TPngBGRAGrayscaleAlpha8bitEncoder;
        ctTrueColorAlpha:
          EncoderClass := TPngBGRATrueColorAlpha8bitEncoder;
      else
        raise EPngError.Create(RCStrUnsupportedFormat);
      end;

      DataStream := TMemoryStream.Create;
      with DataStream do
        try
          with EncoderClass.Create(DataStream, FImageHeader, FGammaChunk,
            FPaletteChunk) do
            try
              EncodeFromScanline(TGuiCustomPixelMap(Source), PixelmapScanline);
            finally
              Free;
            end;

          // reset data stream position
          DataStream.Seek(0, soFromBeginning);

          // compress image data from data stream
          CompressImageDataFromStream(DataStream);
        finally
          FreeAndNil(DataStream);
        end;
    end
  else
    inherited;
end;

procedure TPortableNetworkGraphicPixel32.AssignTo(Dest: TPersistent);
begin
  if Dest is TGuiCustomPixelMap then
  begin
    TGuiCustomPixelMap(Dest).Width := Abs(ImageHeader.Width);
    TGuiCustomPixelMap(Dest).Height := Abs(ImageHeader.Height);
    if ImageHeader.Width * ImageHeader.Height <> 0 then
      DrawToPixelMap(TGuiCustomPixelMap(Dest));
  end
  else if Dest is TBitmap then
  begin
    TBitmap(Dest).Width := ImageHeader.Width;
    TBitmap(Dest).Height := ImageHeader.Height;
    // TBitmap(Dest).
    // DrawToPixelMap(TGuiCustomPixelMap(Dest));
  end
  else
    inherited;
end;

function ColorIndexInPalette(Pixel: TPixel32; Palette: TPalette24)
  : Integer; overload;
begin
  for Result := 0 to Length(Palette) - 1 do
    if (Pixel.R = Palette[Result].R) and (Pixel.G = Palette[Result].G) and
      (Pixel.B = Palette[Result].B) then
      Exit;
  Result := -1;
end;

procedure TPortableNetworkGraphicPixel32.AssignPropertiesFromPixelMap
  (PixelMap: TGuiCustomPixelMap);
var
  index: Integer;
  IsAlpha: Boolean;
  IsGrayScale: Boolean;
  IsPalette: Boolean;
  Color: TPixel32;
  TempPalette: TPalette24;
  TempAlpha: Byte;
begin
  with PixelMap do
  begin
    // basic properties
    ImageHeader.Width := Width;
    ImageHeader.Height := Height;
    ImageHeader.CompressionMethod := 0;
    ImageHeader.InterlaceMethod := imNone;

    // initialize
    SetLength(TempPalette, 0);
    IsGrayScale := True;
    IsPalette := True;
    IsAlpha := False;
    TempAlpha := 0;

    // check every pixel in the bitmap for the use of the alpha channel,
    // whether the image is grayscale or whether the colors can be stored
    // as a palette (and build the palette at the same time
    if FStorageFormat = psfAuto then
      for Index := 0 to Width * Height - 1 do
      begin
        Color := DataPointer[Index];

        // check whether the palette is empty
        if Length(TempPalette) = 0 then
        begin
          IsAlpha := Color.A < 255;

          // eventually store first alpha component
          if IsAlpha then
            TempAlpha := Color.A;

          SetLength(TempPalette, 1);
          TempPalette[0].R := Color.R;
          TempPalette[0].G := Color.G;
          TempPalette[0].B := Color.B;
          IsGrayScale := (Color.R = Color.G) and (Color.B = Color.G);
        end
        else
        begin
          // check alpha channel
          if (Color.A < 255) then
          begin
            if IsAlpha then
              if IsPalette and (TempAlpha <> Color.A) then
                IsPalette := False
              else
            else
              TempAlpha := Color.A;

            IsAlpha := True;
          end;
          if ColorIndexInPalette(Color, TempPalette) < 0 then
          begin
            if IsPalette then
              if (Length(TempPalette) < 256) then
              begin
                SetLength(TempPalette, Length(TempPalette) + 1);
                TempPalette[Length(TempPalette) - 1].R := Color.R;
                TempPalette[Length(TempPalette) - 1].G := Color.G;
                TempPalette[Length(TempPalette) - 1].B := Color.B;
                if IsGrayScale and
                  not((Color.R = Color.G) and (Color.B = Color.G)) then
                  IsGrayScale := False;
              end
              else
                IsPalette := False
            else if not((Color.R = Color.G) and (Color.B = Color.G)) then
              IsGrayScale := False;
          end;
        end;

        if IsAlpha and (not IsPalette) and (not IsGrayScale) then
          Break;
      end
    else if FStorageFormat = psfGrayscale then
    begin
      IsAlpha := False;
      IsPalette := False;
      IsGrayScale := True;
    end
    else if FStorageFormat = psfPalette then
    begin
      IsAlpha := False;
      IsPalette := True;
      IsGrayScale := False;
    end
    else if FStorageFormat = psfRGB then
    begin
      IsAlpha := False;
      IsPalette := False;
      IsGrayScale := False;
    end
    else if FStorageFormat = psfRGBA then
    begin
      IsAlpha := True;
      IsPalette := False;
      IsGrayScale := False;
    end;

    // set image header
    with ImageHeader do
      if IsGrayScale then
        if IsAlpha then
        begin
          ColorType := ctGrayscaleAlpha;
          BitDepth := 8;
        end
        else
        begin
          ColorType := ctIndexedColor; // ctGrayscale
          if Length(TempPalette) <= 2 then
            BitDepth := 1
          else if Length(TempPalette) <= 4 then
            BitDepth := 2
          else if Length(TempPalette) <= 16 then
            BitDepth := 4
          else
            BitDepth := 8;
        end
      else if IsPalette then
      begin
        ColorType := ctIndexedColor;
        if Length(TempPalette) <= 2 then
          BitDepth := 1
        else if Length(TempPalette) <= 4 then
          BitDepth := 2
        else if Length(TempPalette) <= 16 then
          BitDepth := 4
        else
          BitDepth := 8;
      end
      else if IsAlpha then
      begin
        ColorType := ctTrueColorAlpha;
        BitDepth := 8;
      end
      else
      begin
        ColorType := ctTrueColor;
        BitDepth := 8;
      end;

    // eventually prepare palette
    if ImageHeader.HasPalette then
    begin
      Assert(Length(TempPalette) <= 256);

      if not Assigned(FPaletteChunk) then
        FPaletteChunk := TChunkPngPalette.Create(ImageHeader);

      FPaletteChunk.Count := Length(TempPalette);
      for Index := 0 to Length(TempPalette) - 1 do
        FPaletteChunk.PaletteEntry[Index] := TempPalette[Index];
    end;

{$IFDEF StoreGamma}
    // add linear gamma chunk
    if not Assigned(FGammaChunk) then
      FGammaChunk := TChunkPngGamma.Create(ImageHeader);
    FGammaChunk.GammaAsSingle := 1;
{$ELSE}
    // delete any gama correction table
    if Assigned(FGammaChunk) then
      FreeAndNil(FGammaChunk);
{$ENDIF}
  end;
end;

procedure TPortableNetworkGraphicPixel32.DrawToPixelMap
  (PixelMap: TGuiCustomPixelMap);
var
  DecoderClass: TCustomPngDecoderClass;
  DataStream: TMemoryStream;
begin
  DataStream := TMemoryStream.Create;
  try
    // decompress image data to data stream
    DecompressImageDataToStream(DataStream);

    // reset data stream position
    DataStream.Seek(0, soFromBeginning);

    case ImageHeader.InterlaceMethod of
      imNone:
        case ImageHeader.ColorType of
          ctGrayscale:
            case ImageHeader.BitDepth of
              1:
                DecoderClass := TPngGrayscale1bitBGRADecoder;
              2:
                DecoderClass := TPngGrayscale2bitBGRADecoder;
              4:
                DecoderClass := TPngGrayscale4bitBGRADecoder;
              8:
                DecoderClass := TPngGrayscale8bitBGRADecoder;
              16:
                DecoderClass := TPngGrayscale16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor:
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngTrueColor8bitBGRADecoder;
              16:
                DecoderClass := TPngTrueColor16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctIndexedColor:
            case ImageHeader.BitDepth of
              1, 2, 4:
                DecoderClass := TPngPaletteBGRADecoder;
              8:
                DecoderClass := TPngPalette8bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha:
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngGrayscaleAlpha8bitBGRADecoder;
              16:
                DecoderClass := TPngGrayscaleAlpha16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColorAlpha:
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngTrueColorAlpha8bitBGRADecoder;
              16:
                DecoderClass := TPngTrueColorAlpha16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
        else
          raise EPngError.Create(RCStrUnsupportedFormat);
        end;
      imAdam7:
        case ImageHeader.ColorType of
          ctGrayscale:
            case ImageHeader.BitDepth of
              1:
                DecoderClass := TPngAdam7Grayscale1bitBGRADecoder;
              2:
                DecoderClass := TPngAdam7Grayscale2bitBGRADecoder;
              4:
                DecoderClass := TPngAdam7Grayscale4bitBGRADecoder;
              8:
                DecoderClass := TPngAdam7Grayscale8bitBGRADecoder;
              16:
                DecoderClass := TPngAdam7Grayscale16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColor:
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngAdam7TrueColor8bitBGRADecoder;
              16:
                DecoderClass := TPngAdam7TrueColor16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctIndexedColor:
            case ImageHeader.BitDepth of
              1:
                DecoderClass := TPngAdam7Palette1bitBGRADecoder;
              2:
                DecoderClass := TPngAdam7Palette2bitBGRADecoder;
              4:
                DecoderClass := TPngAdam7Palette4bitBGRADecoder;
              8:
                DecoderClass := TPngAdam7Palette8bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctGrayscaleAlpha:
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngAdam7GrayscaleAlpha8bitBGRADecoder;
              16:
                DecoderClass := TPngAdam7GrayscaleAlpha16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
          ctTrueColorAlpha:
            case ImageHeader.BitDepth of
              8:
                DecoderClass := TPngAdam7TrueColorAlpha8bitBGRADecoder;
              16:
                DecoderClass := TPngAdam7TrueColorAlpha16bitBGRADecoder;
            else
              raise EPngError.Create(RCStrUnsupportedFormat);
            end;
        else
          raise EPngError.Create(RCStrUnsupportedFormat);
        end;
    else
      raise EPngError.Create(RCStrUnsupportedFormat);
    end;

    with DecoderClass.Create(DataStream, FImageHeader, FGammaChunk,
      FPaletteChunk) do
      try
        DecodeToScanline(PixelMap, PixelmapScanline);
      finally
        Free;
      end;
  finally
    FreeAndNil(DataStream);
  end;
end;

function TPortableNetworkGraphicPixel32.PixelmapScanline(Bitmap: TObject;
  Y: Integer): Pointer;
begin
  if Bitmap is TGuiCustomPixelMap then
    Result := TGuiCustomPixelMap(Bitmap).ScanLine[Y]
  else
    Result := nil;
end;

procedure TPortableNetworkGraphicPixel32.ReadData(Stream: TStream);
begin
  LoadFromStream(Stream);
end;

procedure TPortableNetworkGraphicPixel32.SetStorageFormat
  (const Value: TPngStorageFormat);
begin
  FStorageFormat := Value;
end;

procedure TPortableNetworkGraphicPixel32.WriteData(Stream: TStream);
begin
  SaveToStream(Stream);
end;

procedure TPortableNetworkGraphicPixel32.DefineProperties(Filer: TFiler);
var
  HasData: Boolean;
begin
  HasData := Width * Height <> 0;
  Filer.DefineBinaryProperty('Data', ReadData, WriteData, HasData);
end;

procedure BuildCrcTable(Polynomial: Cardinal);
var
  c: Cardinal;
  n, k: Integer;
begin
  // allocate CRC table memory
  GetMem(GCrcTable, 256 * SizeOf(Cardinal));

  // fill CRC table
  for n := 0 to 255 do
  begin
    c := n;
    for k := 0 to 7 do
    begin
      if (c and 1) <> 0 then
        c := Polynomial xor (c shr 1)
      else
        c := c shr 1;
    end;
    GCrcTable^[n] := c;
  end;
end;

initialization

BuildCrcTable($EDB88320);
RegisterGraphicFileFormat(TPortableNetworkGraphicPixel32);

finalization

if Assigned(GCrcTable) then
  Dispose(GCrcTable);

end.
