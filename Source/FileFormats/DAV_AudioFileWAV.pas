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
    FTotalNrOfSamples  : Cardinal;
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
    function GetVersion: Integer;
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
    procedure SetVersion(const Value: Integer);
    procedure CheckCreateCartChunk;
  protected
    function GetBitsPerSample: Byte; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Cardinal; override;
    function GetSampleRate: Double; override;
    function GetSampleFrames: Cardinal; override;

    procedure SetBitsPerSample(const Value: Byte); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Cardinal); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetSampleFrames(const Value: Cardinal); override;

    procedure CheckCartChunkEmpty; virtual;
    procedure CheckHeader(const Stream: TStream); virtual;
    procedure ParseChunkInformation(const Stream: TStream);

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
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // file format identifier
    class function DefaultExtension: string; override;
    class function Description: string; override;
    class function FileFormatFilter: string; override;
    class function CanLoad(const Stream: TStream): Boolean; override;

    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property BytesPerSample: Integer read FBytesPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;

    property Version: Integer read GetVersion write SetVersion;
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

constructor TCustomAudioFileWAV.Create(AOwner: TComponent);
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
 inherited;
end;

class function TCustomAudioFileWAV.DefaultExtension: string;
begin
 result := 'WAV';
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
  ChunkName : TChunkName;
  ChunkSize : Cardinal;
begin
 result := False;
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF' then exit;

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   if (ChunkSize > Size - Position) and not (ChunkSize = $FFFFFFFF) then exit;

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE' then exit;
  end;
 result := True;
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
 result := FTotalNrOfSamples;
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

function TCustomAudioFileWAV.GetVersion: Integer;
begin
 if assigned(FCartChunk)
  then result := FCartChunk.Version
  else result := 0;
end;

function TCustomAudioFileWAV.GetBitsPerSample: Byte;
begin
 result := FFormatChunk.BitsPerSample;
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
 with FFormatChunk do
  if BitsPerSample <> Value then
   begin
    BitsPerSample   := Value;
    FBytesPerSample := (BitsPerSample + 7) div 8;
    BlockAlign      := Channels * FBytesPerSample;
    BytesPerSecond  := BlockAlign * SampleRate;
//    BitsPerSampleChanged;
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
 inherited;
 with FFormatChunk do
  if Channels <> Value then
   begin
    Channels       := Value;
    BlockAlign     := FBytesPerSample * Value;
    BytesPerSecond := BlockAlign * SampleRate;
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

procedure TCustomAudioFileWAV.SetEncoding(const Value: TAudioEncoding);
begin
 // yet todo
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
 if FTotalNrOfSamples <> Value then
  begin
   inherited;
   FTotalNrOfSamples := Value;
  end;
end;

procedure TCustomAudioFileWAV.SetSampleRate(const Value: Double);
begin
 inherited;
 with FFormatChunk do
  if SampleRate <> Value then
   begin
    SampleRate := Round(Value);
    BytesPerSecond := BlockAlign * SampleRate;
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

procedure TCustomAudioFileWAV.SetVersion(const Value: Integer);
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

procedure TCustomAudioFileWAV.CheckCreateCartChunk;
begin
 // eventually create cart chunk
 if not assigned(FCartChunk)
  then FCartChunk := FCartChunk.Create;
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

procedure TCustomAudioFileWAV.ParseChunkInformation(const Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkEnd     : Cardinal;
begin
 with Stream do
  begin
   // clear all chunks
   if assigned(FFactChunk) then FreeAndNil(FFactChunk);

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
     FTotalNrOfSamples := SampleCount;
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
      then FTotalNrOfSamples := DataSize div FFormatChunk.BlockAlign;

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
   // eventually create cart chunk
   if not assigned(FBextChunk)
    then FBextChunk := TBextChunk.Create;

   FBextChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileWAV.ReadCartChunk(const Stream: TStream);
begin
 with Stream do
  begin
   // eventually create cart chunk
   if not assigned(FCartChunk)
    then FCartChunk := TCartChunk.Create;

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

// Load/Save

procedure TCustomAudioFileWAV.LoadFromStream(Stream: TStream);
begin
 inherited;
 CheckHeader(Stream);
 ParseChunkInformation(Stream);
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
   FFormatChunk.SaveToStream(Stream);

   // if exists, write the fact chunk
   if FFormatChunk.FormatTag <> etPCM then
    with TFactChunk.Create do
     try
      SampleFrames := FTotalNrOfSamples;
      SaveToStream(Stream);
     finally
      Free;
     end;


   // ToDo: write data here!

   WriteAudioDataToStream(Stream);

   // finally write filesize
   ChunkSize := Position - (ChunkStart + 8);
   Position  := ChunkStart + 4;
   Write(ChunkSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

function TCustomAudioFileWAV.CreateDataCoder: TCustomChannelDataCoder;
begin
 case FFormatChunk.FormatTag of
  etPcm:
   begin
    result := TChannel32DataCoderFixedPoint.Create;
    with TChannel32DataCoderFixedPoint(result), FFormatChunk
     do SetBitsAndSampleSize(ValidBitsPerSample, BlockAlign div Channels);
   end;
  etPcmFloat:
    case FFormatChunk.BlockAlign div FFormatChunk.Channels of
      2 : result := TChannel32DataCoderFloat16.Create;
      4 : result := TChannel32DataCoderFloat32.Create;
      8 : result := TChannel32DataCoderFloat64.Create;
     else result := nil
    end;
  etALaw: result := TChannel32DataCoderALaw.Create;
  etMuLaw: result := TChannel32DataCoderMuLaw.Create;
  else result := nil;
 end;

 // set blocksize
 if assigned(result) then
  with result do
   begin
    BlockSize := 16384;
    ChannelCount := FFormatChunk.Channels;
   end;
end;

procedure TCustomAudioFileWAV.ReadAudioDataFromStream(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
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
     while Samples + SampleFrames < Self.FTotalNrOfSamples do
      begin
       LoadFromStream(Stream);
       if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);

       Samples := Samples + SampleFrames;
      end;

      SampleFrames := Self.FTotalNrOfSamples - Samples;
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
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  ChunkEnd    : Cardinal;
  DataEncoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 // check if sample
 if SampleFrames > 0 then
  with Stream do
   begin
    FAudioDataPosition := Position;

    // write 'data' chunk name
    ChunkName := 'data';
    Write(ChunkName, 4);

    // write chunk size
    ChunkSize := FFormatChunk.BlockAlign * SampleFrames;
    Write(ChunkSize, 4);

    // calculate chunk end (to ensure the above value is correct)
    ChunkEnd := Stream.Position + ChunkSize;

    DataEncoder := CreateDataCoder;
    if not assigned(DataEncoder) then exit;

    if assigned(FOnBeginWrite)
     then FOnBeginWrite(Self);

    with DataEncoder do
     try
      Samples := 0;
      while Samples + SampleFrames < Self.FTotalNrOfSamples do
       begin
        if assigned(FOnEncode) then FOnEncode(Self, DataEncoder, Samples);
        SaveToStream(Stream);

        Samples := Samples + SampleFrames;
       end;

       SampleFrames := Self.FTotalNrOfSamples - Samples;
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
  RegisterFileFormat(TCustomAudioFileWAV);
//  RegisterWaveChunk(TQualityChunk);

end.
