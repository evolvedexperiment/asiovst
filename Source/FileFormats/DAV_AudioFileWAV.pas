unit DAV_AudioFileWAV;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, SysUtils, DAV_Common, DAV_AudioFile, DAV_WaveFileTypes,
  DAV_ChunkClasses, DAV_ChunkWaveFile, DAV_ChannelDataCoder;

type
  EWavError = class(Exception);

  TWaveChunkType = (ctFormat, ctFact, ctData);
  TWaveChunkTypes = set of TWaveChunkType;

  TWaveChunkClass = class of TWaveChunk;

  TWaveChunk = class(TComponent)
  private
    FChunk           : TCustomChunk;
    FCollectionItem  : TCollectionItem;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    class function CanApplyTo(aComponent: TPersistent): Boolean; virtual;
    class function GetDisplayName: string; virtual;
    property CollectionItem: TCollectionItem read FCollectionItem;
    property Chunk: TCustomChunk read FChunk;
  end;

  TWaveChunkCollectionItem = class(TCollectionItem)
  private
    FSubPropertiesSize   : Integer;
    FSubProperties       : Pointer;
    FWaveChunkParameters : string;
    FWaveChunk           : TWaveChunk;
    FEnabled             : Boolean;
    FWaveChunkClass      : TWaveChunkClass;
    FWaveChunkClassName  : string;
    function GetWaveChunkClass: TWaveChunkClass;
    procedure ReadParams(S: TStream);
    procedure SetWaveChunkClassName(const Value: string);
    procedure SetWaveChunkClass(const AWaveChunkClass : TWaveChunkClass);
    procedure WriteParams(S: TStream);
  protected
    procedure AssignTo(Dest: TPersistent); override;
    procedure DefineProperties(Filer: TFiler); override;
    procedure Loaded; virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    function GetDisplayName: string; override;
    property WaveChunk: TWaveChunk read FWaveChunk write FWaveChunk;
  published
    property Enabled: Boolean read FEnabled write FEnabled default True;
    property WaveChunkClass: TWaveChunkClass read GetWaveChunkClass write SetWaveChunkClass;
    property WaveChunkClassName: string read FWaveChunkClassName write SetWaveChunkClassName;
    property WaveChunkParameters: string read FWaveChunkParameters write FWaveChunkParameters;
  end;

  TWaveChunkCollection = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TWaveChunkCollectionItem;
    procedure SetItem(Index: Integer; const Value: TWaveChunkCollectionItem);
  protected
    procedure Loaded; virtual;
  public
    constructor Create(AOwner: TComponent; ItemClass: TCollectionItemClass); reintroduce;
    function Add: TWaveChunkCollectionItem; reintroduce;
    procedure Assign(Source: TPersistent); override;
    property Items[Index: Integer]: TWaveChunkCollectionItem read GetItem write SetItem; default;
  end;

  TCustomAudioFileWAV = class(TCustomAudioFile)
  private
    FChunkSize         : Cardinal;
    FTotalSampleFrames : Cardinal;
    FFormatChunk       : TFormatChunk;
    FFactChunk         : TFactChunk;
    FBextChunk         : TBextChunk;
    FCartChunk         : TCartChunk;
//    FFileTags          : TObjectList;
    FBytesPerSample    : Integer;
    FAudioDataPosition : Cardinal;
    FFormatChunkFound  : Boolean;
    function GetTitle: string;
    function GetArtist: string;
    function GetCategory: string;
    function GetClassification: string;
    function GetClientID: string;
    function GetCutID: string;
    function GetdbLevelReference: Integer;
    function GetEndDate: string;
    function GetEndTime: string;
    function GetOutCue: string;
    function GetProducerAppID: string;
    function GetProducerAppVersion: string;
    function GetStartDate: string;
    function GetStartTime: string;
    function GetUserDef: string;
    function GetCartVersion: Integer;
    function GetBextVersion: Integer;
    function GetBextDescription: string;
    function GetOriginationDate: string;
    function GetOriginationTime: string;
    function GetOriginator: string;
    function GetOriginatorRef: string;
    function GetTimeRefHigh: Integer;
    function GetTimeRefLow: Integer;
    procedure ReadAudioDataFromStream(const Stream: TStream);
    procedure WriteAudioDataToStream(const Stream: TStream);
    procedure ReadItaHeaderChunk(const Stream: TStream);
    procedure SetTitle(const Value: string);
    procedure SetArtist(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetClassification(const Value: string);
    procedure SetClientID(const Value: string);
    procedure SetCutID(const Value: string);
    procedure SetdbLevelReference(const Value: Integer);
    procedure SetEndDate(const Value: string);
    procedure SetEndTime(const Value: string);
    procedure SetOutCue(const Value: string);
    procedure SetProducerAppID(const Value: string);
    procedure SetProducerAppVersion(const Value: string);
    procedure SetStartDate(const Value: string);
    procedure SetStartTime(const Value: string);
    procedure SetUserDef(const Value: string);
    procedure SetBextVersion(const Value: Integer);
    procedure SetCartVersion(const Value: Integer);
    procedure SetBextDescription(const Value: string);
    procedure SetOriginationDate(const Value: string);
    procedure SetOriginationTime(const Value: string);
    procedure SetOriginator(const Value: string);
    procedure SetOriginatorRef(const Value: string);
    procedure SetTimeRefHigh(const Value: Integer);
    procedure SetTimeRefLow(const Value: Integer);
    function GetEmptyData: Boolean;
  protected
    function GetBitsPerSample: Byte; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Cardinal; override;
    function GetSampleRate: Double; override;
    function GetSampleFrames: Cardinal; override;
    function GetDataSize: Cardinal;

    procedure SetBitsPerSample(const Value: Byte); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Cardinal); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetSampleFrames(const Value: Cardinal); override;

    procedure CheckCartChunkEmpty; virtual;
    procedure CheckBextChunkEmpty; virtual;
    procedure CheckCreateBextChunk; virtual;
    procedure CheckCreateCartChunk; virtual;
    procedure CheckHeader(const Stream: TStream); override;
    procedure ParseStream(const Stream: TStream); override;

    function CreateDataCoder: TCustomChannelDataCoder;
    procedure ReadFactChunk(const Stream: TStream);
    procedure ReadFormatChunk(const Stream: TStream);
    procedure ReadDataChunk(const Stream: TStream);
    procedure ReadBextChunk(const Stream: TStream);
    procedure ReadCueChunk(const Stream: TStream);
    procedure ReadJunkChunk(const Stream: TStream);
    procedure ReadPeakChunk(const Stream: TStream);
    procedure ReadListChunk(const Stream: TStream);
    procedure ReadDispChunk(const Stream: TStream);
    procedure ReadCartChunk(const Stream: TStream);
    procedure ReadMextChunk(const Stream: TStream);
    procedure ReadAFSPChunk(const Stream: TStream);
    procedure ReadSDA8Chunk(const Stream: TStream);
    procedure ReadLevelChunk(const Stream: TStream);
    procedure ReadAuxChunk(const Stream: TStream);
    procedure ReadSilentChunk(const Stream: TStream);
    procedure ReadPlaylistChunk(const Stream: TStream);
    procedure ReadLableChunk(const Stream: TStream);
    procedure ReadSampleChunk(const Stream: TStream);
    procedure ReadPadChunk(const Stream: TStream);
    procedure ReadUnknownChunk(const Stream: TStream);

    procedure WriteDataChunk(const Stream: TStream);
    procedure WriteFormatChunk(const Stream: TStream);
    procedure WriteTotalSampleFrames(const Stream: TStream);

    property EmptyData: Boolean read GetEmptyData;
  public
    constructor Create; override;
    destructor Destroy; override;

    // load/save stream
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // decode/encode
    procedure Decode(SamplePosition: Cardinal; SampleFrames: Cardinal); override;
    procedure Encode(SamplePosition: Cardinal; SampleFrames: Cardinal); override;

    // file format identifier
    class function DefaultExtension: string; override;
    class function Description: string; override;
    class function FileFormatFilter: string; override;
    class function CanLoad(const Stream: TStream): Boolean; override;

    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property BytesPerSample: Integer read FBytesPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
    property DataSize: Cardinal read GetDataSize; 

    // from CART chunk
    property CartVersion: Integer read GetCartVersion write SetCartVersion;
    property Title: string read GetTitle write SetTitle;
    property Artist: string read GetArtist write SetArtist;
    property CutID: string read GetCutID write SetCutID;
    property ClientID: string read GetClientID write SetClientID;
    property Category: string read GetCategory write SetCategory;
    property Classification: string read GetClassification write SetClassification;
    property OutCue: string read GetOutCue write SetOutCue;
    property StartDate: string read GetStartDate write SetStartDate;
    property StartTime: string read GetStartTime write SetStartTime;
    property EndDate: string read GetEndDate write SetEndDate;
    property EndTime: string read GetEndTime write SetEndTime;
    property ProducerAppID: string read GetProducerAppID write SetProducerAppID;
    property ProducerAppVersion: string read GetProducerAppVersion write SetProducerAppVersion;
    property UserDef: string read GetUserDef write SetUserDef;
    property dbLevelReference: Integer read GetdbLevelReference write SetdbLevelReference;

    // from BEXT chunk
    property BextVersion: Integer read GetBextVersion write SetBextVersion;
    property BextDescription: string read GetBextDescription write SetBextDescription;
    property Originator: string read GetOriginator write SetOriginator;
    property OriginatorRef: string read GetOriginatorRef write SetOriginatorRef;
    property OriginationDate: string read GetOriginationDate write SetOriginationDate;
    property OriginationTime: string read GetOriginationTime write SetOriginationTime;
    property TimeRefLow: Integer read GetTimeRefLow write SetTimeRefLow;
    property TimeRefHigh: Integer read GetTimeRefHigh write SetTimeRefHigh;
  end;

  TAudioFileWAV  = class(TCustomAudioFileWAV)
  published
    property SampleRate;
    property ChannelCount;
    property SampleFrames;
    property TotalTime;
    property BitsPerSample;
    property BytesPerSample;
    property Encoding;

    property Title;
  end;

var
  WaveChunkClasses: array of TWaveChunkClass;

implementation

resourcestring
  RCRIFFChunkNotFound  = 'This is not a RIFF file!';
  RCRIFFSizeMismatch   = 'Filesize mismatch';
  RCWAVEChunkNotFound  = 'This is not a WAVE file!';
  RCFMTChunkDublicate  = 'More than one format chunk found!';
  RCFACTChunkDublicate = 'More than one fact chunk found!';
  RCDATAChunkDublicate = 'Only one data chunk supported!';

function WaveChunkClassByName(Value: string): TWaveChunkClass;
var
  X: Integer;
begin
 Result := nil;
 for X := Length(WaveChunkClasses) - 1 downto 0 do
  begin
   if CompareText(WaveChunkClasses[X].ClassName, Value) = 0 then
    begin
     Result := WaveChunkClasses[X];
     Break;
    end;
  end;
end;

procedure RegisterWaveChunk(aClass: TWaveChunkClass);
begin
 Classes.RegisterClass(aClass);
 Setlength(WaveChunkClasses, Length(WaveChunkClasses) + 1);
 WaveChunkClasses[Length(WaveChunkClasses) - 1] := aClass;
end;

{ TWaveChunk }

destructor TWaveChunk.Destroy;
begin
 if assigned(FChunk)
  then FreeAndNil(FChunk);
 inherited;
end;

procedure TWaveChunk.AssignTo(Dest: TPersistent);
begin
 if (Dest is TWaveChunk) then
  begin
   TWaveChunk(Dest).FChunk.Assign(FChunk);
   TWaveChunk(Dest).FCollectionItem.Assign(FCollectionItem);
  end
 else inherited;
end;

class function TWaveChunk.CanApplyTo(aComponent: TPersistent): Boolean;
begin
 Result := True;
end;

class function TWaveChunk.GetDisplayName: string;
begin
 Result := 'Unknown Element';
end;

{ TWaveChunkCollectionItem }

constructor TWaveChunkCollectionItem.Create(Collection: TCollection);
begin
 inherited;
 FEnabled := True;
 FSubProperties := nil;
end;

destructor TWaveChunkCollectionItem.Destroy;
begin
 if FSubProperties <> nil then FreeMem(FSubProperties);
 if FWaveChunk <> nil then FWaveChunk.Free;
 inherited;
end;

procedure TWaveChunkCollectionItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TWaveChunkCollectionItem then
  with TWaveChunkCollectionItem(Dest) do
   begin
    Enabled := Self.Enabled;
    WaveChunkClassName := Self.WaveChunkClassName;
    FWaveChunk.Assign(Self.FWaveChunk);
   end
 else inherited;
end;

procedure TWaveChunkCollectionItem.DefineProperties(Filer: TFiler);
begin
 inherited;
 Filer.DefineBinaryProperty('WaveChunkParameters', ReadParams, WriteParams, (FWaveChunk <> nil));
end;

function TWaveChunkCollectionItem.GetDisplayName: string;
begin
 if FWaveChunk = nil
  then Result := 'Wave Chunk'
  else Result := FWaveChunk.GetDisplayName;
end;

function TWaveChunkCollectionItem.GetWaveChunkClass: TWaveChunkClass;
begin
 result := FWaveChunkClass;
end;

procedure TWaveChunkCollectionItem.Loaded;
var
  MS: TMemoryStream;
  Reader: TReader;
begin
 inherited;
 if FSubProperties <> nil then
  begin
   MS := TMemoryStream.Create;
   try
    MS.SetSize(FSubPropertiesSize);
    move(FSubProperties^, MS.Memory^, MS.Size);
    Reader := TReader.Create(MS, 4096);
    try                           
     Reader.IgnoreChildren := False;
     WaveChunkClassName := Reader.ReadString;
     Reader.ReadRootComponent(FWaveChunk);
    finally
     Reader.Free;
    end;
   finally
    MS.Free;
   end;
  end;
end;

procedure TWaveChunkCollectionItem.ReadParams(S: TStream);
begin
 if S.Size > 0 then
  begin
   FSubPropertiesSize := S.Size;
   Getmem(FSubProperties, S.Size);
   S.Read(FSubProperties^, S.Size);
  end
 else FSubPropertiesSize := 0;
end;

procedure TWaveChunkCollectionItem.SetWaveChunkClass(const AWaveChunkClass: TWaveChunkClass);
var
  i         : Integer;
  oldFilter : TWaveChunk;
label
  FoundFilter;
begin
 for i := 0 to Length(WaveChunkClasses) - 1 do
  if WaveChunkClasses[i] = AWaveChunkClass then goto FoundFilter;
 raise EWavError.Create(AWaveChunkClass.ClassName + ' has not been registered');

 FoundFilter:

 oldFilter := FWaveChunk;

 FWaveChunkClass := AWaveChunkClass;
 if FWaveChunkClass <> nil then
  begin
   FWaveChunk := AWaveChunkClass.Create(nil); // TWaveChunkCollection(GetOwner)
   FWaveChunk.FCollectionItem := Self;
   if oldFilter <> nil
    then FWaveChunk.Assign(oldFilter);
  end;

 if oldFilter <> nil
  then FreeAndNil(oldFilter);
end;

procedure TWaveChunkCollectionItem.SetWaveChunkClassName(const Value: string);
var
  TheClass : TWaveChunkClass;
begin
 TheClass := nil;
 if Value <> '' then
  begin
   TheClass := WaveChunkClassByName(Value);
   if TheClass = nil then
    raise EWavError.Create(Value + ' has not been registered');
  end;

 FWaveChunkClass := TheClass;
 if FWaveChunk <> nil then
  begin
   FWaveChunk.Free;
   FWaveChunk := nil;
  end;
 FWaveChunkClassName := Value;
 if TheClass <> nil then
  begin
   FWaveChunk := TheClass.Create(nil); // TWaveChunkList(GetOwner).
   FWaveChunk.FCollectionItem := Self;
  end;
end;

procedure TWaveChunkCollectionItem.WriteParams(S: TStream);
var
  Writer: TWriter;
begin
 Writer := TWriter.Create(S, 4096);
 with Writer do
  try
   IgnoreChildren := False;
   WriteString(FWaveChunkClassName);
   WriteRootComponent(FWaveChunk);
  finally
   Free;
  end;
end;

{ TWaveChunkCollection }

constructor TWaveChunkCollection.Create(AOwner: TComponent; ItemClass: TCollectionItemClass);
begin
 inherited Create(AOwner, ItemClass);
end;

function TWaveChunkCollection.Add: TWaveChunkCollectionItem;
begin
 result := TWaveChunkCollectionItem(inherited Add);
end;

procedure TWaveChunkCollection.Assign(Source: TPersistent);
var
  i : Integer;
begin
 if Source is TWaveChunkCollection then
  begin
   Clear;
   for i := 0 to TWaveChunkCollection(Source).Count - 1 do
    with Add do Assign(TWaveChunkCollection(Source).Items[i]);
  end else inherited;
end;

function TWaveChunkCollection.GetItem(Index: Integer): TWaveChunkCollectionItem;
begin
 result := TWaveChunkCollectionItem(inherited GetItem(index));
end;

procedure TWaveChunkCollection.Loaded;
var
  X: Integer;
begin
  for X := 0 to Count - 1
   do Items[X].Loaded;
end;

procedure TWaveChunkCollection.SetItem(Index: Integer;
  const Value: TWaveChunkCollectionItem);
begin
 inherited SetItem(Index, Value);
end;

{ TCustomAudioFileWAV }

constructor TCustomAudioFileWAV.Create;
begin
 inherited;
 FAudioDataPosition := 0;
 FBytesPerSample := 3; // 24 bit
 FFormatChunk := TFormatChunk.Create;
end;

destructor TCustomAudioFileWAV.Destroy;
begin
 FreeAndNil(FFormatChunk);
 if assigned(FFactChunk)
  then FreeAndNil(FFactChunk);
 if assigned(FBextChunk)
  then FreeAndNil(FBextChunk);
 if assigned(FCartChunk)
  then FreeAndNil(FCartChunk);

 inherited;
end;

class function TCustomAudioFileWAV.DefaultExtension: string;
begin
 result := '.wav';
end;

class function TCustomAudioFileWAV.Description: string;
begin
 result := 'Microsoft RIFF WAVE';
end;

class function TCustomAudioFileWAV.FileFormatFilter: string;
begin
 result := Description + ' (*.' + DefaultExtension + ')|*.wav*'
end;

class function TCustomAudioFileWAV.CanLoad(const Stream: TStream): Boolean;
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  OldPosition : Cardinal;
begin
 result := False;

 // store old position
 OldPosition := Stream.Position;

 with Stream do
  try
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF' then exit;

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   if (ChunkSize > Size - Position) and not (ChunkSize = $FFFFFFFF) then exit;

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE' then exit;

   Result := True;
  finally
   // restore old position
   Position := OldPosition;
  end;
end;

function TCustomAudioFileWAV.GetBextDescription: string;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.Description
  else result := '';
end;

function TCustomAudioFileWAV.GetCategory: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.Category
  else result := '';
end;

function TCustomAudioFileWAV.GetChannels: Cardinal;
begin
 result := FFormatChunk.Channels;
end;

function TCustomAudioFileWAV.GetClassification: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.Classification
  else result := '';
end;

function TCustomAudioFileWAV.GetClientID: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.ClientID
  else result := '';
end;

function TCustomAudioFileWAV.GetCutID: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.CutID
  else result := '';
end;

function TCustomAudioFileWAV.GetdbLevelReference: Integer;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.dbLevelReference
  else result := 0;
end;

function TCustomAudioFileWAV.GetSampleFrames: Cardinal;
begin
 result := FTotalSampleFrames;
end;

function TCustomAudioFileWAV.GetSampleRate: Double;
begin
 result := FFormatChunk.SampleRate;
end;

function TCustomAudioFileWAV.GetStartDate: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.StartDate
  else result := '';
end;

function TCustomAudioFileWAV.GetStartTime: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.StartTime
  else result := '';
end;

function TCustomAudioFileWAV.GetTimeRefHigh: Integer;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.TimeRefHigh
  else result := 0;
end;

function TCustomAudioFileWAV.GetTimeRefLow: Integer;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.TimeRefLow
  else result := 0;
end;

function TCustomAudioFileWAV.GetTitle: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.Title
  else result := '';
end;

function TCustomAudioFileWAV.GetArtist: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.Artist
  else result := '';
end;

function TCustomAudioFileWAV.GetUserDef: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.UserDef
  else result := '';
end;

function TCustomAudioFileWAV.GetBextVersion: Integer;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.Version
  else result := 0;
end;

function TCustomAudioFileWAV.GetCartVersion: Integer;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.Version
  else result := 0;
end;

function TCustomAudioFileWAV.GetBitsPerSample: Byte;
begin
 result := FFormatChunk.BitsPerSample;
end;

function TCustomAudioFileWAV.GetDataSize: Cardinal;
begin
 result := FFormatChunk.BlockAlign * SampleFrames;
end;

function TCustomAudioFileWAV.GetEmptyData: Boolean;
begin
 Result := FAudioDataPosition = 0;
end;

function TCustomAudioFileWAV.GetEncoding: TAudioEncoding;
begin
 case FFormatChunk.FormatTag of
             etPCM : result := aeInteger;
        etPCMFLOAT : result := aeFloat;
         etMSADPCM : result := aeMSADPCM;
        etDVIADPCM : result := aeDVIADPCM;
//  etACM, etACMMPEG : result := aeACM;
              else   result := aeOther;
 end;
end;

function TCustomAudioFileWAV.GetEndDate: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.EndDate
  else result := '';
end;

function TCustomAudioFileWAV.GetEndTime: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.EndTime
  else result := '';
end;

function TCustomAudioFileWAV.GetOriginationDate: string;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.OriginationDate
  else result := '';
end;

function TCustomAudioFileWAV.GetOriginationTime: string;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.OriginationTime
  else result := '';
end;

function TCustomAudioFileWAV.GetOriginator: string;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.Originator
  else result := '';
end;

function TCustomAudioFileWAV.GetOriginatorRef: string;
begin
 if assigned(FBextChunk)
  then result := FBextChunk.OriginatorRef
  else result := '';
end;

function TCustomAudioFileWAV.GetOutCue: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.OutCue
  else result := '';
end;

function TCustomAudioFileWAV.GetProducerAppID: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.ProducerAppID
  else result := '';
end;

function TCustomAudioFileWAV.GetProducerAppVersion: string;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.ProducerAppVersion
  else result := '';
end;

procedure TCustomAudioFileWAV.SetArtist(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Artist := Value;
  end
 else
  begin
   FCartChunk.Artist := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetBitsPerSample(const Value: Byte);
begin
 // assert stream is empty
 if assigned(FStream) and not EmptyData
  then raise Exception.Create('Can''t change the format!');

 with FFormatChunk do
  if BitsPerSample <> Value then
   begin
    BitsPerSample   := Value;
    FBytesPerSample := (BitsPerSample + 7) div 8;
    BlockAlign      := Channels * FBytesPerSample;
    BytesPerSecond  := BlockAlign * SampleRate;
//    BitsPerSampleChanged;
   end;

 // if empty stream is assigned update format chunk
 if assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetCategory(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Category := Value;
  end
 else
  begin
   FCartChunk.Category := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetChannels(const Value: Cardinal);
begin
 // assert stream is empty
 if assigned(FStream) and not EmptyData
  then raise Exception.Create('Can''t change the format!');

 inherited;

 with FFormatChunk do
  if Channels <> Value then
   begin
    Channels       := Value;
    BlockAlign     := FBytesPerSample * Value;
    BytesPerSecond := BlockAlign * SampleRate;
   end;

 // if empty stream is assigned update format chunk
 if assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetClassification(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Classification := Value;
  end
 else
  begin
   FCartChunk.Classification := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetClientID(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.ClientID := Value;
  end
 else
  begin
   FCartChunk.ClientID := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetCutID(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.CutID := Value;
  end
 else
  begin
   FCartChunk.CutID := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetdbLevelReference(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FCartChunk.dbLevelReference := Value;
  end
 else
  begin
   FCartChunk.dbLevelReference := 0;
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetBextDescription(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.Description := Value;
  end
 else
  begin
   FBextChunk.Description := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetEncoding(const Value: TAudioEncoding);
begin
 // assert stream is empty
 if assigned(FStream) and not EmptyData
  then raise Exception.Create('Can''t change the format!');

 case Value of
   aeInteger : FFormatChunk.FormatTag := etPCM;
     aeFloat : begin
                FFormatChunk.FormatTag := etPCMFLOAT;
                BitsPerSample := 32;
               end;
   aeMSADPCM : FFormatChunk.FormatTag := etMSADPCM;
  aeDVIADPCM : FFormatChunk.FormatTag := etDVIADPCM;
      aeALaw : begin
                FFormatChunk.FormatTag := etALaw;
                BitsPerSample := 8;
               end;
     aeMuLaw : begin
                FFormatChunk.FormatTag := etMuLaw;
                BitsPerSample := 8;
               end;
  else raise Exception.Create('Not yet implemented');
 end;

 // if empty stream is assigned update format chunk
 if assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetEndDate(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.EndTime := Value;
  end
 else
  begin
   FCartChunk.EndTime := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetEndTime(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.EndTime := Value;
  end
 else
  begin
   FCartChunk.EndTime := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginationDate(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.OriginationDate := Value;
  end
 else
  begin
   FBextChunk.OriginationDate := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginationTime(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.OriginationTime := Value;
  end
 else
  begin
   FBextChunk.OriginationTime := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginator(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.Originator := Value;
  end
 else
  begin
   FBextChunk.Originator := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOriginatorRef(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FBextChunk.OriginatorRef := Value;
  end
 else
  begin
   FBextChunk.OriginatorRef := '';
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetOutCue(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.OutCue := Value;
  end
 else
  begin
   FCartChunk.OutCue := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetProducerAppID(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.ProducerAppID := Value;
  end
 else
  begin
   FCartChunk.ProducerAppID := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetProducerAppVersion(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.ProducerAppVersion := Value;
  end
 else
  begin
   FCartChunk.ProducerAppVersion := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetSampleFrames(const Value: Cardinal);
begin
 if FTotalSampleFrames <> Value then
  begin
   inherited;
   FTotalSampleFrames := Value;
   if assigned(FFactChunk)
    then FFactChunk.SampleCount := FTotalSampleFrames;
   if assigned(FStream)
    then WriteTotalSampleFrames(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetSampleRate(const Value: Double);
begin
 // assert stream is empty
 if assigned(FStream) and not EmptyData
  then raise Exception.Create('Can''t change the format!');

 inherited;
 with FFormatChunk do
  if SampleRate <> Value then
   begin
    SampleRate := Round(Value);
    BytesPerSecond := BlockAlign * SampleRate;
   end;

 // if empty stream is assigned update format chunk
 if assigned(FStream) and EmptyData then
  begin
   FStream.Position := 12;
   WriteFormatChunk(FStream);
  end;
end;

procedure TCustomAudioFileWAV.SetStartDate(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.StartDate := Value;
  end
 else
  begin
   FCartChunk.StartDate := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetStartTime(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.StartTime := Value;
  end
 else
  begin
   FCartChunk.StartTime := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetTimeRefHigh(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FBextChunk.TimeRefHigh := Value;
  end
 else
  begin
   FBextChunk.TimeRefHigh := 0;
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetTimeRefLow(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FBextChunk.TimeRefLow := Value;
  end
 else
  begin
   FBextChunk.TimeRefLow := 0;
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetTitle(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.Title := Value;
  end
 else
  begin
   FCartChunk.Title := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetUserDef(const Value: string);
begin
 if Value <> '' then
  begin
   CheckCreateCartChunk;
   FCartChunk.UserDef := Value;
  end
 else
  begin
   FCartChunk.UserDef := '';
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetBextVersion(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateBextChunk;
   FBextChunk.Version := Value;
  end
 else
  begin
   FBextChunk.Version := 0;
   CheckBextChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.SetCartVersion(const Value: Integer);
begin
 if Value <> 0 then
  begin
   CheckCreateCartChunk;
   FCartChunk.Version := Value;
  end
 else
  begin
   FCartChunk.Version := 0;
   CheckCartChunkEmpty;
  end;
end;

procedure TCustomAudioFileWAV.CheckCartChunkEmpty;
begin
 // todo: not yet implemented!
end;

procedure TCustomAudioFileWAV.CheckBextChunkEmpty;
begin
 with FBextChunk do
  if (Description = '') and (Originator = '') and (OriginatorRef = '') and
    (OriginationDate = '') and (OriginationTime = '') and (TimeRefLow = 0) and
    (TimeRefHigh = 0) and (Version = 0) then
   begin
    FreeAndNil(FBextChunk);
   end;
end;

procedure TCustomAudioFileWAV.CheckCreateCartChunk;
begin
 // eventually create cart chunk
 if not assigned(FCartChunk)
  then FCartChunk := TCartChunk.Create;
end;

procedure TCustomAudioFileWAV.CheckCreateBextChunk;
begin
 // eventually create bext chunk
 if not assigned(FBextChunk)
  then FBextChunk := TBextChunk.Create;
end;

procedure TCustomAudioFileWAV.CheckHeader(const Stream: TStream);
var
  ChunkName : TChunkName;
begin
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF'
    then raise EWavError.Create(rcRIFFChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(FChunkSize, 4);
   if (FChunkSize > Size - Position) and not (FChunkSize = $FFFFFFFF)
    then raise EWavError.Create(rcRIFFSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE'
    then raise EWavError.Create(rcWAVEChunkNotFound);
  end;
end;

procedure TCustomAudioFileWAV.ParseStream(const Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkEnd     : Cardinal;
begin
 with Stream do
  begin
   // clear all chunks
   if assigned(FFactChunk) then FreeAndNil(FFactChunk);
   if assigned(FCartChunk) then FreeAndNil(FCartChunk);
   if assigned(FBextChunk) then FreeAndNil(FBextChunk);

   FFormatChunkFound := False;

   // start parsing here
   ChunkEnd := Position + FChunkSize - 4;
   while Position < ChunkEnd do
    begin
     // read chunk name
     Read(ChunkName, 4);

     // read chunk position
     Position := Position - 4;

     if ChunkName = 'fmt ' then ReadFormatChunk(Stream) else
     if ChunkName = 'fact' then ReadFactChunk(Stream) else
     if ChunkName = 'junk' then ReadJunkChunk(Stream) else
     if ChunkName = 'cue ' then ReadCueChunk(Stream) else
     if ChunkName = 'PEAK' then ReadPeakChunk(Stream) else
     if ChunkName = 'LIST' then ReadListChunk(Stream) else
     if ChunkName = 'DISP' then ReadDispChunk(Stream) else
     if ChunkName = 'cart' then ReadCartChunk(Stream) else
     if ChunkName = 'bext' then ReadBextChunk(Stream) else
     if ChunkName = 'mext' then ReadMextChunk(Stream) else
     if ChunkName = 'levl' then ReadLevelChunk(Stream) else
     if ChunkName = 'aux ' then ReadAuxChunk(Stream) else
     if ChunkName = 'afsp' then ReadAFSPChunk(Stream) else
     if ChunkName = 'slnt' then ReadSilentChunk(Stream) else
     if ChunkName = 'plst' then ReadPlaylistChunk(Stream) else
     if ChunkName = 'itah' then ReadItaHeaderChunk(Stream) else
     if ChunkName = 'labl' then ReadLableChunk(Stream) else
     if ChunkName = 'SyLp' then ReadUnknownChunk(Stream) else
     if ChunkName = 'SDA8' then ReadSDA8Chunk(Stream) else
     if ChunkName = 'Smpl' then ReadSampleChunk(Stream) else
     if ChunkName = 'pad ' then ReadPadChunk(Stream) else
     if ChunkName = 'data' then ReadDataChunk(Stream)
      else ReadUnknownChunk(Stream);
    end;
  end;
end;

procedure TCustomAudioFileWAV.ReadFormatChunk(const Stream: TStream);
begin
 with Stream do
  begin
   // check whether format chunk has already been created
   if FFormatChunkFound
    then raise Exception.Create(RCFACTChunkDublicate);

   // load format chunk
   FFormatChunk.LoadFromStream(Stream);
   FFormatChunkFound := True;
  end;
end;

procedure TCustomAudioFileWAV.ReadFactChunk(const Stream: TStream);
begin
 with Stream do
  begin
   // check whether fact chunk has already been created
   if assigned(FFactChunk)
    then raise Exception.Create(RCFACTChunkDublicate);

   FFactChunk := TFactChunk.Create;
   with FFactChunk do
    begin
     // now load fact chunk
     LoadFromStream(Stream);

     // now only use the sample count information
     FTotalSampleFrames := SampleCount;
    end;
  end;
end;

procedure TCustomAudioFileWAV.ReadDataChunk(const Stream: TStream);
var
  DataSize      : Cardinal;
  ChunksReaded  : TWaveChunkTypes;
begin
 with Stream do
  if ctData in ChunksReaded
   then raise EWavError.Create(rcDATAChunkDublicate)
   else
    begin
     FAudioDataPosition := Position;
     Position := Position + 4;
     Read(DataSize, 4);

     // eventually set total number of samples
     if not assigned(FFactChunk)
      then FTotalSampleFrames := DataSize div FFormatChunk.BlockAlign
      else
     if FFormatChunk.FormatTag <> etPcm
      then FTotalSampleFrames := FFactChunk.SampleCount;

     Position := Position + DataSize;

     // make all chunks word aligned!
     // Quote: "The sample data must end on an even byte boundary"
     Position := Position + ((Position - FAudioDataPosition) and $1);
    end
end;

procedure TCustomAudioFileWAV.ReadItaHeaderChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadAuxChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadDispChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadBextChunk(const Stream: TStream);
begin
 with Stream do
  begin
   CheckCreateBextChunk;
   FBextChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.ReadCartChunk(const Stream: TStream);
begin
 with Stream do
  begin
   CheckCreateCartChunk;
   FCartChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.ReadCueChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadJunkChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadLableChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadLevelChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadListChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadMextChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadPadChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadPeakChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadPlaylistChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadAFSPChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadSampleChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadSDA8Chunk(const Stream: TStream);
begin
 with Stream, TWavSDA8Chunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadSilentChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadUnknownChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.WriteFormatChunk(const Stream: TStream);
begin
 FFormatChunk.SaveToStream(Stream);
end;

procedure TCustomAudioFileWAV.WriteDataChunk(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
begin
 with Stream do
  begin
   // write 'data' chunk name
   ChunkName := 'data';
   Write(ChunkName, 4);

   // write chunk size
   ChunkSize := DataSize;
   Write(ChunkSize, 4);
  end;
end;


// Load/Save

procedure TCustomAudioFileWAV.LoadFromStream(Stream: TStream);
begin
 inherited;
 CheckHeader(Stream);
 ParseStream(Stream);
 ReadAudioDataFromStream(Stream);
end;

procedure TCustomAudioFileWAV.SaveToStream(Stream: TStream);
var
  ChunkName  : TChunkName;
  ChunkStart : Cardinal;
  ChunkSize  : Cardinal;
begin
 inherited;
 with Stream do
  begin
   // Store chunk start position, just in case the stream position is not 0;
   ChunkStart := Position;

   // first write 'RIFF' (resource interchange file format)
   ChunkName := 'RIFF';
   Write(ChunkName, 4);

   // write dummy filesize yet, since final size is still unknown
   ChunkSize := $FFFFFFFF;
   Write(ChunkSize, 4);

   // now specify the RIFF file to be a WAVE file
   ChunkName := 'WAVE';
   Write(ChunkName, 4);

   // write format chunk
   WriteFormatChunk(Stream);

   // check whether a fact chunk is necessary
   if (FFormatChunk.FormatTag <> etPCM) then
    begin
     // if no fact chunk has been found, create it
     if not assigned(FFactChunk)
      then FFactChunk := TFactChunk.Create;

     // store total number of samples to fact
     FFactChunk.SampleCount := FTotalSampleFrames;
     FFactChunk.SaveToStream(Stream);
    end;

   // ToDo: write data here!
   WriteAudioDataToStream(Stream);

   // write cart chunk if available
   if assigned(FCartChunk)
    then FCartChunk.SaveToStream(Stream);

   // write bext chunk if available
   if assigned(FBextChunk)
    then FBextChunk.SaveToStream(Stream);

   // finally write filesize
   ChunkSize := Position - (ChunkStart + 8);
   Position  := ChunkStart + 4;
   Write(ChunkSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

procedure TCustomAudioFileWAV.WriteTotalSampleFrames(const Stream: TStream);
var
  ChunkSize   : Cardinal;
  OldPosition : Cardinal;
begin
 with Stream do
  begin
   OldPosition := Position;
   if not EmptyData then
    begin
     Position := FAudioDataPosition + 4;
     ChunkSize := DataSize;
     Write(ChunkSize, 4);
    end;

   // finally write filesize
   ChunkSize := Size - 8;
   Seek(4, soFromBeginning);
   Write(ChunkSize, 4);
   Position := OldPosition;
  end;
end;

function TCustomAudioFileWAV.CreateDataCoder: TCustomChannelDataCoder;
begin
 case FFormatChunk.FormatTag of
  etPcm:
   begin
    Result := TChannel32DataCoderFixedPoint.Create;
    with TChannel32DataCoderFixedPoint(Result), FFormatChunk
     do SetBitsAndSampleSize(ValidBitsPerSample, BlockAlign div Channels);
   end;
  etPcmFloat:
    case FFormatChunk.BlockAlign div FFormatChunk.Channels of
      2 : Result := TChannel32DataCoderFloat16.Create;
      4 : Result := TChannel32DataCoderFloat32.Create;
      8 : Result := TChannel32DataCoderFloat64.Create;
     else Result := nil
    end;
  etALaw: Result := TChannel32DataCoderALaw.Create;
  etMuLaw: Result := TChannel32DataCoderMuLaw.Create;
  else Result := nil;
 end;

 // set blocksize
 if assigned(Result) then
  with Result do
   begin
    BlockSize := Self.FBlockSize;
    ChannelCount := FFormatChunk.Channels;
   end;
end;

procedure TCustomAudioFileWAV.Decode(SamplePosition, SampleFrames: Cardinal);
var
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 inherited;

 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with FStream do
  begin
   DataDecoder := CreateDataCoder;
   if not assigned(DataDecoder) then exit;

   assert(FAudioDataPosition > 0);
   Position := FAudioDataPosition + 8 + DataDecoder.SampleToByte(SamplePosition);

   if assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   try
    Samples := SamplePosition;
    while Samples - SamplePosition + DataDecoder.SampleFrames < SampleFrames do
     begin
      DataDecoder.LoadFromStream(FStream);
      if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);

      Samples := Samples + DataDecoder.SampleFrames;
     end;

     DataDecoder.SampleFrames := SampleFrames - Samples + SamplePosition;
     DataDecoder.LoadFromStream(FStream);
     if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
   finally
    FreeAndNil(DataDecoder);
   end;
  end;
end;

procedure TCustomAudioFileWAV.Encode(SamplePosition, SampleFrames: Cardinal);
var
  DataEncoder  : TCustomChannelDataCoder;
  Samples, Pos : Cardinal;
begin
 inherited;

 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with FStream do
  begin
   DataEncoder := CreateDataCoder;
   if not assigned(DataEncoder) then exit;

   if EmptyData then
    begin
     FStream.Seek(0, soFromEnd);
     FAudioDataPosition := FStream.Position;
     WriteDataChunk(FStream);
    end;

   Position := FAudioDataPosition + 8 + DataEncoder.SampleToByte(SamplePosition);

   if assigned(FOnBeginWrite)
    then FOnBeginWrite(Self);

   try
    Samples := 0;
    Pos := SamplePosition;
    while Samples + DataEncoder.SampleFrames < SampleFrames do
     begin
      if assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
      DataEncoder.SaveToStream(FStream);

      Samples := Samples + DataEncoder.SampleFrames;
      Pos := Pos + DataEncoder.SampleFrames;
     end;

     DataEncoder.SampleFrames := SampleFrames - Samples;
     if assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Pos);
     DataEncoder.SaveToStream(FStream);
   finally
    FreeAndNil(DataEncoder);
   end;
  end;
end;

procedure TCustomAudioFileWAV.ReadAudioDataFromStream(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with Stream do
  begin
   assert(FAudioDataPosition > 0);
   Position := FAudioDataPosition;

   Read(ChunkName, 4);
   assert(ChunkName = 'data');

   Read(ChunkSize, 4);

   DataDecoder := CreateDataCoder;
   if not assigned(DataDecoder) then exit;

   if assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   with DataDecoder do
    try
     Samples := 0;
     while Samples + SampleFrames < Self.SampleFrames do
      begin
       LoadFromStream(Stream);
       if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);

       Samples := Samples + SampleFrames;
      end;

      SampleFrames := Self.SampleFrames - Samples;
      LoadFromStream(Stream);
      if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
    finally
     FreeAndNil(DataDecoder);
    end;
   assert((Stream.Position - FAudioDataPosition - 8) <= ChunkSize);
  end;
end;

procedure TCustomAudioFileWAV.WriteAudioDataToStream(const Stream: TStream);
var
  ChunkSize   : Cardinal;
  ChunkEnd    : Cardinal;
  DataEncoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 // check for no samples to load
 if SampleFrames = 0 then Exit;

 with Stream do
  begin
   FAudioDataPosition := Position;

   // write data chunk
   WriteDataChunk(Stream);

   // calculate chunk end (to ensure the above value is correct)
   ChunkEnd := Stream.Position + DataSize;

   DataEncoder := CreateDataCoder;
   if not assigned(DataEncoder) then exit;

   if assigned(FOnBeginWrite)
    then FOnBeginWrite(Self);

   with DataEncoder do
    try
     Samples := 0;
     while Samples + SampleFrames < SampleFrames do
      begin
       if assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
       SaveToStream(Stream);

       Samples := Samples + SampleFrames;
      end;

      SampleFrames := Self.SampleFrames - Samples;
      if assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
      SaveToStream(Stream);
    finally
     FreeAndNil(DataEncoder);
    end;

   assert(Position = ChunkEnd);
   Position := ChunkEnd;
  end;
end;

initialization
  RegisterFileFormat(TAudioFileWAV);
//  RegisterWaveChunk(TQualityChunk);

end.
