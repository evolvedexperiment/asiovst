unit DAV_GuiPngChunks;

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
  Classes, Graphics, SysUtils, zlib, DAV_Types, DAV_ChunkClasses,
  DAV_GuiCommon, DAV_GuiPngTypes, DAV_GuiPngClasses;

type
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
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure ResetToDefault; virtual;

    property Width: Integer read FWidth write FWidth;
    property Height: Integer read FHeight write FHeight;
    property BitDepth: Byte read FBitDepth write FBitDepth;
    property ColourType: TColourType read FColourType write FColourType;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
    property FilterMethod: Byte read FFilterMethod write FFilterMethod;
    property InterlaceMethod: Byte read FInterlaceMethod write FInterlaceMethod;
  end;

  TCustomChunkPngWithHeader = class(TCustomChunkPng)
  protected
    FHeader : TChunkPngImageHeader;
  public
    constructor Create(Header: TChunkPngImageHeader); reintroduce; virtual;
    procedure HeaderChanged; virtual;
  end;
  TCustomChunkPngWithHeaderClass = class of TCustomChunkPngWithHeader;

  TChunkPngImageData = class(TCustomChunkPngWithHeader)
  private
    FData : TMemoryStream;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Data: TMemoryStream read FData;
  end;

  TChunkPngPalette = class(TCustomChunkPngWithHeader)
  private
    FPaletteEntries : array of TRGB24;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngGamma = class(TCustomChunkPngWithHeader)
  private
    FGamma : Cardinal;
    function GetGammaAsSingle: Single;
    procedure SetGammaAsSingle(const Value: Single);
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Gamma: Cardinal read FGamma write FGamma;
    property GammaAsSingle: Single read GetGammaAsSingle write SetGammaAsSingle;
  end;

  TChunkPngStandardColourSpaceRGB = class(TCustomChunkPngWithHeader)
  private
    FRenderingIntent : Byte;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngPrimaryChromaticities = class(TCustomChunkPngWithHeader)
  private
    FWhiteX : Integer;
    FWhiteY : Integer;
    FRedX   : Integer;
    FRedY   : Integer;
    FGreenX : Integer;
    FGreenY : Integer;
    FBlueX  : Integer;
    FBlueY  : Integer;
    function GetBlueX: Single;
    function GetBlueY: Single;
    function GetGreenX: Single;
    function GetGreenY: Single;
    function GetRedX: Single;
    function GetRedY: Single;
    function GetWhiteX: Single;
    function GetWhiteY: Single;
    procedure SetBlueX(const Value: Single);
    procedure SetBlueY(const Value: Single);
    procedure SetGreenX(const Value: Single);
    procedure SetGreenY(const Value: Single);
    procedure SetRedX(const Value: Single);
    procedure SetRedY(const Value: Single);
    procedure SetWhiteX(const Value: Single);
    procedure SetWhiteY(const Value: Single);
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property WhiteX: Integer read FWhiteX write FWhiteX;
    property WhiteY: Integer read FWhiteY write FWhiteY;
    property RedX: Integer read FRedX write FRedX;
    property RedY: Integer read FRedY write FRedY;
    property GreenX: Integer read FGreenX write FGreenX;
    property GreenY: Integer read FGreenY write FGreenY;
    property BlueX: Integer read FBlueX write FBlueX;
    property BlueY: Integer read FBlueY write FBlueY;

    property WhiteXAsSingle: Single read GetWhiteX write SetWhiteX;
    property WhiteYAsSingle: Single read GetWhiteY write SetWhiteY;
    property RedXAsSingle: Single read GetRedX write SetRedX;
    property RedYAsSingle: Single read GetRedY write SetRedY;
    property GreenXAsSingle: Single read GetGreenX write SetGreenX;
    property GreenYAsSingle: Single read GetGreenY write SetGreenY;
    property BlueXAsSingle: Single read GetBlueX write SetBlueX;
    property BlueYAsSingle: Single read GetBlueY write SetBlueY;
  end;

  TChunkPngTime = class(TCustomChunkPngWithHeader)
  private
    FYear   : Word;
    FMonth  : Byte;
    FDay    : Byte;
    FHour   : Byte;
    FMinute : Byte;
    FSecond : Byte;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Year: Word read FYear write FYear;
    property Month: Byte read FMonth write FMonth;
    property Day: Byte read FDay write FDay;
    property Hour: Byte read FHour write FHour;
    property Minute: Byte read FMinute write FMinute;
    property Second: Byte read FSecond write FSecond;
  end;

  TChunkPngEmbeddedIccProfile = class(TCustomChunkPngWithHeader)
  private
    FProfileName       : AnsiString;
    FCompressionMethod : Byte;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ProfileName: AnsiString read FProfileName write FProfileName;
    property CompressionMethod: Byte read FCompressionMethod write FCompressionMethod;
  end;

  TChunkPngSignificantBits = class(TCustomChunkPngWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TCustomPngBackgroundColour = class(TPersistent)
  protected
    class function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngBackgroundColourFormat04 = class(TCustomPngBackgroundColour)
  private
    FGreySampleValue : Word;
  protected
    class function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngBackgroundColourFormat26 = class(TCustomPngBackgroundColour)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    class function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TPngBackgroundColourFormat3 = class(TCustomPngBackgroundColour)
  protected
    FIndex : Byte;
    class function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PaletteIndex: Byte read FIndex write FIndex;
  end;

  TChunkPngBackgroundColor = class(TCustomChunkPngWithHeader)
  protected
    FBackground : TCustomPngBackgroundColour;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    destructor Destroy; override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;
  end;

  TChunkPngImageHistogram = class(TCustomChunkPngWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngSuggestedPalette = class(TCustomChunkPngWithHeader)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TCustomPngTransparency = class(TPersistent)
  protected
    function GetChunkSize: Integer; virtual; abstract;
  public
    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngTransparencyFormat0 = class(TCustomPngTransparency)
  private
    FGreySampleValue : Word;
  protected
    function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property ChunkSize: Integer read GetChunkSize;
  end;

  TPngTransparencyFormat2 = class(TCustomPngTransparency)
  private
    FRedSampleValue : Word;
    FBlueSampleValue : Word;
    FGreenSampleValue : Word;
  protected
    function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TPngTransparencyFormat3 = class(TCustomPngTransparency)
  private
    function GetCount: Integer;
    function GetTransparency(Index: Integer): Byte;
  protected
    FTransparency : array of Byte;
    function GetChunkSize: Integer; override;
  public
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property Count: Integer read GetCount;
    property Transparency[Index: Integer]: Byte read GetTransparency;
  end;

  TChunkPngTransparency = class(TCustomChunkPngWithHeader)
  protected
    FTransparency : TCustomPngTransparency;
  public
    constructor Create(Header: TChunkPngImageHeader); override;
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    procedure HeaderChanged; override;
  end;

  TChunkPngPhysicalPixelDimensions = class(TCustomChunkPngWithHeader)
  private
    FPixelsPerUnitX : Cardinal;
    FPixelsPerUnitY : Cardinal;
    FUnit           : Byte;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property PixelsPerUnitX: Cardinal read FPixelsPerUnitX write FPixelsPerUnitX;
    property PixelsPerUnitY: Cardinal read FPixelsPerUnitY write FPixelsPerUnitY;
    property PixelUnit: Byte read FUnit write FUnit;
  end;

  TChunkPngPhysicalScale = class(TCustomChunkPngWithHeader)
  private
    FUnitSpecifier  : Byte;
    FUnitsPerPixelX : Single;
    FUnitsPerPixelY : Single;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    property UnitSpecifier: Byte read FUnitSpecifier;
    property UnitsPerPixelX: Single read FUnitsPerPixelX;
    property UnitsPerPixelY: Single read FUnitsPerPixelY;
  end;

  TChunkPngImageOffset = class(TCustomChunkPngWithHeader)
  private
    FImagePositionX : Integer;
    FImagePositionY : Integer;
    FUnitSpecifier  : Byte;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngPixelCalibrator = class(TCustomChunkPngWithHeader)
  private
    FCalibratorName : AnsiString;
    FOriginalZeroes : array [0..1] of Integer;
    FEquationType   : Byte;
    FNumberOfParams : Byte;
    FUnitName       : AnsiString;
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TCustomChunkPngTextChunk = class(TCustomChunkPngWithHeader)
  protected
    FKeyword : AnsiString;
    FText    : AnsiString;
  public
    property Keyword: AnsiString read FKeyword write FKeyword;
    property Text: AnsiString read FText write FText;
  end;

  TChunkPngTextChunk = class(TCustomChunkPngTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  TChunkPngCompressedTextChunk = class(TCustomChunkPngTextChunk)
  private
    FCompressionMethod : Byte;
  public
    class function GetClassChunkName: TChunkName; override;
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
    class function GetClassChunkName: TChunkName; override;
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

procedure RegisterPngChunk(ChunkClass: TCustomChunkPngWithHeaderClass);
procedure RegisterPngChunks(ChunkClasses: array of TCustomChunkPngWithHeaderClass);
function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomChunkPngWithHeaderClass;

implementation

uses
  DAV_GuiPngResourceStrings;

var
  GPngChunkClasses: array of TCustomChunkPngWithHeaderClass;


function IsPngChunkRegistered(ChunkClass: TCustomChunkPngWithHeaderClass): Boolean;
var
  ChunkClassIndex : Integer;
begin
 Result := False;
 for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
  if GPngChunkClasses[ChunkClassIndex] = ChunkClass then
   begin
    Result := True;
    Exit;
   end;
end;

procedure RegisterPngChunk(ChunkClass: TCustomChunkPngWithHeaderClass);
begin
 Assert(IsPngChunkRegistered(ChunkClass) = False);
 SetLength(GPngChunkClasses, Length(GPngChunkClasses) + 1);
 GPngChunkClasses[Length(GPngChunkClasses) - 1] := ChunkClass;
end;

procedure RegisterPngChunks(ChunkClasses: array of TCustomChunkPngWithHeaderClass);
var
  ChunkClassIndex : Integer;
begin
 for ChunkClassIndex := 0 to Length(ChunkClasses) - 1
  do RegisterPngChunk(ChunkClasses[ChunkClassIndex]);
end;

function FindPngChunkByChunkName(ChunkName: TChunkName): TCustomChunkPngWithHeaderClass;
var
  ChunkClassIndex : Integer;
begin
 Result := nil;
 for ChunkClassIndex := 0 to Length(GPngChunkClasses) - 1 do
  if GPngChunkClasses[ChunkClassIndex].GetClassChunkName = ChunkName then
   begin
    Result := GPngChunkClasses[ChunkClassIndex];
    Exit;
   end;
end;


{ TCustomChunkPng }

constructor TCustomChunkPng.Create;
begin
 inherited;
 ChunkFlags := [cfSizeFirst, cfReversedByteOrder];
end;


{ TChunkPngImageHeader }

constructor TChunkPngImageHeader.Create;
begin
 inherited;

 ResetToDefault;
end;

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
   FWidth := ReadSwappedCardinal(Stream);

   // read height
   FHeight := ReadSwappedCardinal(Stream);

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

procedure TChunkPngImageHeader.ResetToDefault;
begin
 FWidth             := 0;
 FHeight            := 0;
 FBitDepth          := 8;
 FColourType        := ctTrueColour;
 FCompressionMethod := 0;
 FFilterMethod      := 0;
 FInterlaceMethod   := 0;
end;

procedure TChunkPngImageHeader.SaveToStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   // write width
   WriteSwappedCardinal(Stream, FWidth);

   // write height
   WriteSwappedCardinal(Stream, FHeight);

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


{ TCustomChunkPngWithHeader }

constructor TCustomChunkPngWithHeader.Create(Header: TChunkPngImageHeader);
begin
 FHeader := Header;
 inherited Create;
end;

procedure TCustomChunkPngWithHeader.HeaderChanged;
begin
 // purely virtual, do nothing by default
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
   if ((Size - Position) mod SizeOf(TRGB24)) <> 0
    then raise EPngError.Create(RCStrIncompletePalette);

   SetLength(FPaletteEntries, (Size - Position) div SizeOf(TRGB24));

   Read(FPaletteEntries[0], Length(FPaletteEntries) * SizeOf(TRGB24));
  end;
end;

procedure TChunkPngPalette.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 FChunkSize := Length(FPaletteEntries) * SizeOf(TRGB24);

 inherited;

 Stream.Write(FPaletteEntries[0], FChunkSize);
end;


{ TChunkPngTransparency }

constructor TChunkPngTransparency.Create(Header: TChunkPngImageHeader);
begin
 inherited;
 case Header.ColourType of
  ctGreyscale     : FTransparency := TPngTransparencyFormat0.Create;
  ctTrueColour    : FTransparency := TPngTransparencyFormat2.Create;
  ctIndexedColour : FTransparency := TPngTransparencyFormat3.Create;
 end;
end;

class function TChunkPngTransparency.GetClassChunkName: TChunkName;
begin
 Result := 'tRNS';
end;

procedure TChunkPngTransparency.HeaderChanged;
var
  OldTransparency : TCustomPngTransparency;
begin
 inherited;

 // store old transparency object
 OldTransparency := FTransparency;

 // change transparency object class
 case FHeader.ColourType of
  ctGreyscale     : if not (FTransparency is TPngTransparencyFormat0) then
                     begin
                      FTransparency := TPngTransparencyFormat0.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  ctTrueColour    : if not (FTransparency is TPngTransparencyFormat2) then
                     begin
                      FTransparency := TPngTransparencyFormat2.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  ctIndexedColour : if not (FTransparency is TPngTransparencyFormat3) then
                     begin
                      FTransparency := TPngTransparencyFormat3.Create;
                      if Assigned(OldTransparency) then
                       begin
                        FTransparency.Assign(OldTransparency);
                        FreeAndNil(OldTransparency);
                       end;
                     end;
  else if Assigned(FTransparency) then FreeAndNil(FTransparency);

 end;
end;

procedure TChunkPngTransparency.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FTransparency)
  then FTransparency.LoadFromStream(Stream);
end;

procedure TChunkPngTransparency.SaveToStream(Stream: TStream);
begin
 if Assigned(FTransparency)
  then FChunkSize := FTransparency.ChunkSize
  else FChunkSize := 0;

 inherited;

 // check consistency
 case FHeader.ColourType of
  ctGreyscale     : if not (FTransparency is TPngTransparencyFormat0)
                     then raise EPngError.Create(RCStrWrongTransparencyFormat);
  ctTrueColour    : if not (FTransparency is TPngTransparencyFormat2)
                     then raise EPngError.Create(RCStrWrongTransparencyFormat);
  ctIndexedColour : if not (FTransparency is TPngTransparencyFormat3)
                     then raise EPngError.Create(RCStrWrongTransparencyFormat);
 end;

 if Assigned(FTransparency)
  then FTransparency.SaveToStream(Stream);
end;


{ TPngTransparencyFormat0 }

function TPngTransparencyFormat0.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngTransparencyFormat0.LoadFromStream(Stream: TStream);
begin
 inherited;

 FGreySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat0.SaveToStream(Stream: TStream);
begin
 inherited;

 WriteSwappedWord(Stream, FGreySampleValue);
end;


{ TPngTransparencyFormat2 }

function TPngTransparencyFormat2.GetChunkSize: Integer;
begin
 Result := 6;
end;

procedure TPngTransparencyFormat2.LoadFromStream(Stream: TStream);
begin
 inherited;

 FRedSampleValue  := ReadSwappedWord(Stream);
 FBlueSampleValue  := ReadSwappedWord(Stream);
 FGreenSampleValue  := ReadSwappedWord(Stream);
end;

procedure TPngTransparencyFormat2.SaveToStream(Stream: TStream);
begin
 inherited;

 WriteSwappedWord(Stream, FRedSampleValue);
 WriteSwappedWord(Stream, FBlueSampleValue);
 WriteSwappedWord(Stream, FGreenSampleValue);
end;


{ TPngTransparencyFormat3 }

function TPngTransparencyFormat3.GetChunkSize: Integer;
begin
 Result := Count;
end;

function TPngTransparencyFormat3.GetCount: Integer;
begin
 Result := Length(FTransparency);
end;

function TPngTransparencyFormat3.GetTransparency(Index: Integer): Byte;
begin
 if (Index >= 0) and (Index < Count)
  then Result := FTransparency[Index]
  else raise EPngError.Create(RCStrIndexOutOfBounds);
end;

procedure TPngTransparencyFormat3.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   SetLength(FTransparency, Size - Position);
   Read(FTransparency[0], Length(FTransparency));
  end;
end;

procedure TPngTransparencyFormat3.SaveToStream(Stream: TStream);
begin
 inherited;

 Stream.Write(FTransparency[0], Length(FTransparency));
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
   FPixelsPerUnitX := ReadSwappedCardinal(Stream);

   // read pixels per unit, Y axis
   FPixelsPerUnitY := ReadSwappedCardinal(Stream);

   // read unit
   Read(FUnit, 1);
  end;
end;

procedure TChunkPngPhysicalPixelDimensions.SaveToStream(Stream: TStream);
begin
 FChunkSize := 13;

 inherited;

 with Stream do
  begin
   if Size < 13
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // write pixels per unit, X axis
   WriteSwappedCardinal(Stream, FPixelsPerUnitX);

   // write pixels per unit, Y axis
   WriteSwappedCardinal(Stream, FPixelsPerUnitY);

   // write unit
   Write(FUnit, 1);
  end;
end;


{ TChunkPngPhysicalScale }

class function TChunkPngPhysicalScale.GetClassChunkName: TChunkName;
begin
 Result := 'sCAL';
end;

procedure TChunkPngPhysicalScale.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 4
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read unit specifier
   Read(FUnitSpecifier, 1);

   // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
  end;
end;

procedure TChunkPngPhysicalScale.SaveToStream(Stream: TStream);
begin
 inherited;

 raise EPngError.Create(RCStrNotYetImplemented);
 // yet todo, see http://www.libpng.org/pub/png/book/chapter11.html#png.ch11.div.9
end;


{ TChunkPngImageOffset }

class function TChunkPngImageOffset.GetClassChunkName: TChunkName;
begin
 Result := 'oFFs';
end;

procedure TChunkPngImageOffset.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 9
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read image positions
   FImagePositionX := ReadSwappedCardinal(Stream);
   FImagePositionY := ReadSwappedCardinal(Stream);

   // read unit specifier
   Read(FUnitSpecifier, 1);
  end;
end;

procedure TChunkPngImageOffset.SaveToStream(Stream: TStream);
begin
 FChunkSize := 9;

 inherited;

 // read image positions
 WriteSwappedCardinal(Stream, FImagePositionX);
 WriteSwappedCardinal(Stream, FImagePositionY);

 // read unit specifier
 Write(FUnitSpecifier, 1);
end;


{ TChunkPngPixelCalibrator }

class function TChunkPngPixelCalibrator.GetClassChunkName: TChunkName;
begin
 Result := 'pCAL';
end;

procedure TChunkPngPixelCalibrator.LoadFromStream(Stream: TStream);
var
  Index      : Integer;
  ParamIndex : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FCalibratorName, 80);
   while (Position < Size) do
    begin
     Read(FCalibratorName[Index], SizeOf(Byte));
     if FCalibratorName[Index] = #0 then
      begin
       SetLength(FCalibratorName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read original zeros
   FOriginalZeroes[0] := ReadSwappedCardinal(Stream);
   FOriginalZeroes[1] := ReadSwappedCardinal(Stream);

   // read equation type
   Stream.Read(FEquationType, 1);

   // read number of parameters
   Stream.Read(FNumberOfParams, 1);

   // read keyword
   Index := 1;
   SetLength(FUnitName, 80);
   while (Position < Size) do
    begin
     Read(FUnitName[Index], SizeOf(Byte));
     if FUnitName[Index] = #0 then
      begin
       SetLength(FUnitName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   for ParamIndex := 0 to FNumberOfParams - 2 do
    begin
     // yet todo
    end;
  end;
end;

procedure TChunkPngPixelCalibrator.SaveToStream(Stream: TStream);
begin
  inherited;

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
  DataIn      : Pointer;
  DataOut     : Pointer;
  Index       : Integer;
  DataInSize  : Integer;
  DataOutSize : Integer;
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
     DataInSize := Size - Position;
     GetMem(DataIn, DataInSize);
     try
      Read(DataIn^, DataInSize);
      ZDecompress(DataIn, DataInSize, DataOut, DataOutSize);
      try
       SetLength(FText, DataOutSize);
       Move(DataOut^, FText[1], DataOutSize);
      finally
       Dispose(DataOut);
      end;
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
   FYear := ReadSwappedWord(Stream);

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
begin
 FChunkSize := 7;

 inherited;

 with Stream do
  begin
   // write year
   WriteSwappedWord(Stream, FYear);

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


{ TChunkPngEmbeddedIccProfile }

class function TChunkPngEmbeddedIccProfile.GetClassChunkName: TChunkName;
begin
 Result := 'iCCP';
end;

procedure TChunkPngEmbeddedIccProfile.LoadFromStream(Stream: TStream);
var
  Index : Integer;
begin
 inherited;

 with Stream do
  begin
   // read keyword
   Index := 1;
   SetLength(FProfileName, 80);
   while (Position < Size) do
    begin
     Read(FProfileName[Index], SizeOf(Byte));
     if FProfileName[Index] = #0 then
      begin
       SetLength(FProfileName, Index - 1);
       Break;
      end;
     Inc(Index);
    end;

   // read compression method
   Read(FCompressionMethod, 1);

   // not yet completed
  end;
end;

procedure TChunkPngEmbeddedIccProfile.SaveToStream(Stream: TStream);
var
  Temp  : Byte;
begin
 FChunkSize := Length(FProfileName) + 2;

 inherited;

 with Stream do
  begin
   // write keyword
   Write(FProfileName[1], Length(FProfileName));

   // write separator
   Temp := 0;
   Write(Temp, 1);

   // write compression method
   Write(FCompressionMethod, 1);
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
   FGamma := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngGamma.SaveToStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   // write gamma
   WriteSwappedCardinal(Stream, FGamma);
  end;
end;


{ TChunkPngStandardColourSpaceRGB }

class function TChunkPngStandardColourSpaceRGB.GetClassChunkName: TChunkName;
begin
 Result := 'sRGB';
end;

procedure TChunkPngStandardColourSpaceRGB.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 1
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read rendering intent
   Read(FRenderingIntent, SizeOf(Byte));
  end;
end;

procedure TChunkPngStandardColourSpaceRGB.SaveToStream(Stream: TStream);
begin
 FChunkSize := 1;

 inherited;

 // write rendering intent
 Stream.Write(FRenderingIntent, SizeOf(Byte));
end;


{ TChunkPngPrimaryChromaticities }

class function TChunkPngPrimaryChromaticities.GetClassChunkName: TChunkName;
begin
 Result := 'cHRM';
end;

function TChunkPngPrimaryChromaticities.GetBlueX: Single;
begin
 Result := FBlueX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetBlueY: Single;
begin
 Result := FBlueY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenX: Single;
begin
 Result := FGreenX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetGreenY: Single;
begin
 Result := FGreenY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedX: Single;
begin
 Result := FRedX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetRedY: Single;
begin
 Result := FRedY * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteX: Single;
begin
 Result := FWhiteX * 1E-6;
end;

function TChunkPngPrimaryChromaticities.GetWhiteY: Single;
begin
 Result := FWhiteY * 1E-6;
end;

procedure TChunkPngPrimaryChromaticities.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   if Size < 32
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // read white point x
   FWhiteX := ReadSwappedCardinal(Stream);

   // read white point y
   FWhiteY := ReadSwappedCardinal(Stream);

   // read red x
   FRedX := ReadSwappedCardinal(Stream);

   // read red y
   FRedY := ReadSwappedCardinal(Stream);

   // read green x
   FGreenX := ReadSwappedCardinal(Stream);

   // read green y
   FGreenY := ReadSwappedCardinal(Stream);

   // read blue x
   FBlueX := ReadSwappedCardinal(Stream);

   // read blue y
   FBlueY := ReadSwappedCardinal(Stream);
  end;
end;

procedure TChunkPngPrimaryChromaticities.SaveToStream(Stream: TStream);
begin
 FChunkSize := 32;

 inherited;


 with Stream do
  begin
   if Size < 32
    then raise EPngError.Create(RCStrChunkSizeTooSmall);

   // write white point x
   WriteSwappedCardinal(Stream, FWhiteX);

   // write white point y
   WriteSwappedCardinal(Stream, FWhiteY);

   // write red x
   WriteSwappedCardinal(Stream, FRedX);

   // write red y
   WriteSwappedCardinal(Stream, FRedY);

   // write green x
   WriteSwappedCardinal(Stream, FGreenX);

   // write green y
   WriteSwappedCardinal(Stream, FGreenY);

   // write blue x
   WriteSwappedCardinal(Stream, FBlueX);

   // write blue y
   WriteSwappedCardinal(Stream, FBlueY);
  end;
end;

procedure TChunkPngPrimaryChromaticities.SetBlueX(const Value: Single);
begin
 FBlueX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetBlueY(const Value: Single);
begin
 FBlueY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenX(const Value: Single);
begin
 FGreenX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetGreenY(const Value: Single);
begin
 FGreenY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedX(const Value: Single);
begin
 FRedX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetRedY(const Value: Single);
begin
 FRedY := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteX(const Value: Single);
begin
 FWhiteX := Round(Value * 1E6);
end;

procedure TChunkPngPrimaryChromaticities.SetWhiteY(const Value: Single);
begin
 FWhiteY := Round(Value * 1E6);
end;


{ TChunkPngSignificantBits }

class function TChunkPngSignificantBits.GetClassChunkName: TChunkName;
begin
 Result := 'sBIT';
end;

procedure TChunkPngSignificantBits.LoadFromStream(Stream: TStream);
begin
 inherited;

 // yet todo
end;

procedure TChunkPngSignificantBits.SaveToStream(Stream: TStream);
begin
 inherited;

 raise Exception.Create(RCStrNotYetImplemented);
 // yet todo
end;


{ TPngBackgroundColourFormat04 }

class function TPngBackgroundColourFormat04.GetChunkSize: Integer;
begin
 Result := 2;
end;

procedure TPngBackgroundColourFormat04.LoadFromStream(Stream: TStream);
begin
 FGreySampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColourFormat04.SaveToStream(Stream: TStream);
begin
 WriteSwappedWord(Stream, FGreySampleValue);
end;


{ TPngBackgroundColourFormat26 }

class function TPngBackgroundColourFormat26.GetChunkSize: Integer;
begin
 Result := 6;
end;

procedure TPngBackgroundColourFormat26.LoadFromStream(Stream: TStream);
begin
 FRedSampleValue := ReadSwappedWord(Stream);
 FGreenSampleValue := ReadSwappedWord(Stream);
 FBlueSampleValue := ReadSwappedWord(Stream);
end;

procedure TPngBackgroundColourFormat26.SaveToStream(Stream: TStream);
begin
 WriteSwappedWord(Stream, FRedSampleValue);
 WriteSwappedWord(Stream, FGreenSampleValue);
 WriteSwappedWord(Stream, FBlueSampleValue);
end;


{ TPngBackgroundColourFormat3 }

class function TPngBackgroundColourFormat3.GetChunkSize: Integer;
begin
 Result := 1;
end;

procedure TPngBackgroundColourFormat3.LoadFromStream(Stream: TStream);
begin
 Stream.Read(FIndex, 1);
end;

procedure TPngBackgroundColourFormat3.SaveToStream(Stream: TStream);
begin
 Stream.Write(FIndex, 1);
end;


{ TChunkPngBackgroundColor }

constructor TChunkPngBackgroundColor.Create(Header: TChunkPngImageHeader);
begin
 inherited;

 case Header.ColourType of
  ctGreyscale, ctGreyscaleAlpha : FBackground := TPngBackgroundColourFormat04.Create;
  ctTrueColour, ctTrueColourAlpha: FBackground := TPngBackgroundColourFormat26.Create;
  ctIndexedColour: FBackground := TPngBackgroundColourFormat3.Create;
 end;
end;

destructor TChunkPngBackgroundColor.Destroy;
begin
 if Assigned(FBackground)
  then FreeAndNil(FBackground);
 inherited;
end;

class function TChunkPngBackgroundColor.GetClassChunkName: TChunkName;
begin
 Result := 'bKGD';
end;

procedure TChunkPngBackgroundColor.HeaderChanged;
var
  OldBackground : TCustomPngBackgroundColour;
begin
 inherited;

 // store old Background object
 OldBackground := FBackground;

 // change Background object class
 case FHeader.ColourType of
  ctGreyscale, ctGreyscaleAlpha :
   if not (FBackground is TPngBackgroundColourFormat04) then
    begin
     FBackground := TPngBackgroundColourFormat04.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  ctTrueColour, ctTrueColourAlpha :
   if not (FBackground is TPngBackgroundColourFormat26) then
    begin
     FBackground := TPngBackgroundColourFormat26.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  ctIndexedColour :
   if not (FBackground is TPngBackgroundColourFormat3) then
    begin
     FBackground := TPngBackgroundColourFormat3.Create;
     if Assigned(OldBackground) then
      begin
       FBackground.Assign(OldBackground);
       FreeAndNil(OldBackground);
      end;
    end;
  else if Assigned(FBackground) then FreeAndNil(FBackground);

 end;
end;

procedure TChunkPngBackgroundColor.LoadFromStream(Stream: TStream);
begin
 inherited;

 if Assigned(FBackground)
  then FBackground.LoadFromStream(Stream);
end;

procedure TChunkPngBackgroundColor.SaveToStream(Stream: TStream);
begin
 // determine chunk size
 if Assigned(FBackground)
  then FChunkSize := FBackground.GetChunkSize
  else FChunkSize := 0;

 inherited;

 if Assigned(FBackground)
  then FBackground.SaveToStream(Stream);
end;


{ TChunkPngImageHistogram }

class function TChunkPngImageHistogram.GetClassChunkName: TChunkName;
begin
 Result := 'hIST';
end;

procedure TChunkPngImageHistogram.LoadFromStream(Stream: TStream);
begin
 inherited;

 // yet todo
end;

procedure TChunkPngImageHistogram.SaveToStream(Stream: TStream);
begin
 inherited;


 raise Exception.Create(RCStrNotYetImplemented);
 // yet todo
end;


{ TChunkPngSuggestedPalette }

class function TChunkPngSuggestedPalette.GetClassChunkName: TChunkName;
begin

end;

procedure TChunkPngSuggestedPalette.LoadFromStream(Stream: TStream);
begin
  inherited;

end;

procedure TChunkPngSuggestedPalette.SaveToStream(Stream: TStream);
begin
  inherited;

end;


{ TUnknownPngChunk }

constructor TUnknownPngChunk.Create;
begin
 inherited;
 ChunkFlags := [cfSizeFirst, cfReversedByteOrder];
end;


initialization
  RegisterPngChunks([TChunkPngImageData, TChunkPngPalette, TChunkPngGamma,
    TChunkPngStandardColourSpaceRGB, TChunkPngPrimaryChromaticities,
    TChunkPngTime, TChunkPngTransparency, TChunkPngEmbeddedIccProfile,
    TChunkPngPhysicalPixelDimensions, TChunkPngTextChunk,
    TChunkPngCompressedTextChunk, TChunkPngInternationalTextChunk,
    TChunkPngImageHistogram, TChunkPngBackgroundColor,
    TChunkPngSignificantBits, TChunkPngImageOffset, TChunkPngPixelCalibrator]);

end.
