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
    FChunkSize        : Cardinal;
    FTotalNrOfSamples : Cardinal;
    FFormatChunk      : TFormatChunk;
//    FBextChunk        : PBextRecord;
//    FCartChunk        : PCartRecord;
//    FFileTags         : TObjectList;
    FBytesPerSample   : Integer;
    procedure ReadAudioDataFromStream(const Stream: TStream);
    procedure WriteAudioDataToStream(const Stream: TStream);
    procedure ReadItaHeaderChunk(const Stream: TStream);
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
    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property BytesPerSample: Integer read FBytesPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
  end;

  TAudioFileWAV  = class(TCustomAudioFileWAV)
  published
    property SampleRate;
    property ChannelCount;
    property SampleFrames;
    property TotalTime;
(*
    property OnLoadData32;
    property OnLoadData64;
    property OnSaveData32;
    property OnSaveData64;
*)
    property BitsPerSample;
    property BytesPerSample;
    property Encoding;
  end;

var
  WaveChunkClasses: array of TWaveChunkClass;

implementation

resourcestring
  rcRIFFChunkNotFound  = 'This is not a RIFF file!';
  rcRIFFSizeMismatch   = 'Filesize mismatch';
  rcWAVEChunkNotFound  = 'This is not a WAVE file!';
  rcFMTChunkDublicate  = 'One format chunk has already been found!';
  rcFACTChunkDublicate = 'One fact chunk has already been found!';
  rcDATAChunkDublicate = 'Only one data chunk supported!';

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
 FFormatChunk := TFormatChunk.Create;
end;

destructor TCustomAudioFileWAV.Destroy;
begin
 FreeAndNil(FFormatChunk);
 inherited;
end;

function TCustomAudioFileWAV.GetChannels: Cardinal;
begin
 result := FFormatChunk.Channels;
end;

function TCustomAudioFileWAV.GetSampleFrames: Cardinal;
begin
 result := FTotalNrOfSamples;
end;

function TCustomAudioFileWAV.GetSampleRate: Double;
begin
 result := FFormatChunk.SampleRate;
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

procedure TCustomAudioFileWAV.SetEncoding(const Value: TAudioEncoding);
begin

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
  ChunksReaded : TWaveChunkTypes;
begin
 with Stream do
  begin
   // start parsing here
   ChunksReaded := [];

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
     if ChunkName = 'Smpl' then ReadSampleChunk(Stream) else
     if ChunkName = 'pad ' then ReadPadChunk(Stream) else
     if ChunkName = 'data' then ReadDataChunk(Stream)
      else ReadUnknownChunk(Stream);
    end;
  end;
end;

procedure TCustomAudioFileWAV.ReadFormatChunk(const Stream: TStream);
var
  ChunksReaded : TWaveChunkTypes;
begin
 with Stream do
  if ctFormat in ChunksReaded
   then raise EWavError.Create(rcFMTChunkDublicate)
   else
    begin
     FFormatChunk.LoadFromStream(Stream);
     ChunksReaded := ChunksReaded + [ctFormat];
    end;
end;

procedure TCustomAudioFileWAV.ReadFactChunk(const Stream: TStream);
var
  ChunksReaded : TWaveChunkTypes;
begin
 with Stream do
  if ctFact in ChunksReaded
   then raise EWavError.Create(rcFACTChunkDublicate)
   else
    begin
     // check whether fact chunk has already been created
     with TFactChunk.Create do
      try
       // now load fact chunk
       LoadFromStream(Stream);

       // now only use the sample count information
       FTotalNrOfSamples := SampleFrames;
      finally
       Free;
      end;
     ChunksReaded := ChunksReaded + [ctFact];
    end;
end;

procedure TCustomAudioFileWAV.ReadDataChunk(const Stream: TStream);
var
  DataSize     : Cardinal;
  ChunksReaded : TWaveChunkTypes;
begin
 with Stream do
  if ctData in ChunksReaded
   then raise EWavError.Create(rcDATAChunkDublicate)
   else
    begin
//     FDataPositions
     Position := Position + 4;
     Read(DataSize, 4);
     Position := Position + DataSize; // + (DataSize + 1) and $1;
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
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileWAV.ReadCartChunk(const Stream: TStream);
begin
 with Stream, TWavUnknownChunk.Create do
  try
   LoadFromStream(Stream);
  finally
   Free;
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

(*
   BufferSize
   OnLoadData32
*)

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
    result := TChannel32DataCoderFixed.Create;
    result.BlockSize := 16384;
    result.ChannelCount := FFormatChunk.Channels;
    with TChannel32DataCoderFixed(result), FFormatChunk
     do SetBitsAndSampleSize(SampleSize, (SampleSize + 7) div 8);
   end;
  etPcmFloat:
    begin
     result := TChannel32DataCoderFloat32.Create;
     result.BlockSize := 16384;
    end;
  else result := nil;
 end;
end;

procedure TCustomAudioFileWAV.ReadAudioDataFromStream(const Stream: TStream);
var
  Offset      : Integer;
  BlockAlign  : Integer;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Integer;
begin
 with Stream do
  begin
(*
   Position := FAudioDataPosition;

   // read offset
   Read(Offset, 4);
   FlipLong(Offset);

   // read block align (even if it is not used here)
   Read(BlockAlign, 4);
   FlipLong(BlockAlign);

   // advance offset
   Position := Position + Offset;

   DataDecoder := CreateDataCoder;
   if not assigned(DataDecoder) then exit;

   with DataDecoder do
    try
     Samples   := 0;
     while Samples + SampleFrames <= FCommonChunk.SampleFrames do
      begin
       LoadFromStream(Stream);
       if assigned(FOnDecode) then FOnDecode(Self, DataDecoder);
       Samples := Samples + SampleFrames;
      end;

      SampleFrames := FCommonChunk.SampleFrames - Samples;
      LoadFromStream(Stream);
      if assigned(FOnDecode) then FOnDecode(Self, DataDecoder);
    finally
     FreeAndNil(DataDecoder);
    end;
*)
  end;
end;

procedure TCustomAudioFileWAV.WriteAudioDataToStream(const Stream: TStream);
var
  ChunkName   : TChunkName;
  ChunkSize   : Cardinal;
  ChunkEnd    : Cardinal;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Integer;
const
  CZero: Cardinal = 0;
begin
(*
 // check if sample
 if SampleFrames > 0 then
  with Stream do
   begin
    // ToDo: write data here!
    ChunkName := 'SSND';
    Write(ChunkName, 4);

    ChunkSize := 8 + FCommonChunk.SampleFrames * FCommonChunk.Channels *
      (FCommonChunk.SampleSize + 7) div 8;
    Write(ChunkSize, 4);

    Write(CZero, 4); // offset
    Write(CZero, 4); // block align

    with FCommonChunk
     do ChunkEnd := Position + SampleFrames * (SampleSize div 8) * Channels;

    DataDecoder := CreateDataCoder;
    if not assigned(DataDecoder) then exit;

    with DataDecoder do
     try
      Samples   := 0;
      while Samples + SampleFrames <= FCommonChunk.SampleFrames do
       begin
        if assigned(FOnEncode) then FOnEncode(Self, DataDecoder);
        SaveToStream(Stream);

        Samples := Samples + SampleFrames;
       end;

       SampleFrames := FCommonChunk.SampleFrames - Samples;
       if assigned(FOnEncode) then FOnEncode(Self, DataDecoder);
       SaveToStream(Stream);
      finally
      FreeAndNil(DataDecoder);
     end;

    assert(Stream.Position = ChunkEnd);
    Position := ChunkEnd;
   end;
*)
end;



initialization
//  RegisterWaveChunk(TQualityChunk);

end.
