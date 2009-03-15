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
    FTotalNrOfSamples : Cardinal;
    FFormatChunk      : TFormatChunk;
//    FBextChunk        : PBextRecord;
//    FCartChunk        : PCartRecord;
//    FFileTags         : TObjectList;
    FBytesPerSample   : Integer;
  protected
    function GetBitsPerSample: Byte; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Cardinal; override;
    function GetSampleRate: Double; override;
    function GetSampleCount: Cardinal; override;

    procedure SetBitsPerSample(const Value: Byte); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Cardinal); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetSampleCount(const Value: Cardinal); override;
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
    property SampleCount;
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

function TCustomAudioFileWAV.GetSampleCount: Cardinal;
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

procedure TCustomAudioFileWAV.SetSampleCount(const Value: Cardinal);
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

// Load/Save

procedure TCustomAudioFileWAV.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Cardinal;
  ChunkEnd     : Cardinal;
  DataSize     : Cardinal;
  ChunksReaded : TWaveChunkTypes;
begin
 inherited;
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF'
    then raise EWavError.Create(rcRIFFChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   if (ChunkSize <> Size - Position) and not (ChunkSize = $FFFFFFFF)
    then raise EWavError.Create(rcRIFFSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE'
    then raise EWavError.Create(rcWAVEChunkNotFound);

   // start parsing here
   ChunksReaded := [];

   ChunkEnd := Position + ChunkSize - 4;
   while Position < ChunkEnd do
    begin
     Read(ChunkName, 4);
     if ChunkName = 'fmt ' then
      if ctFormat in ChunksReaded
       then raise EWavError.Create(rcFMTChunkDublicate)
       else
        begin
         FFormatChunk.LoadFromStream(Stream);
         ChunksReaded := ChunksReaded + [ctFormat];
        end else
     if ChunkName = 'fact' then
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
           FTotalNrOfSamples := SampleCount;
          finally
           Free;
          end;
         ChunksReaded := ChunksReaded + [ctFact];
        end else
     if ChunkName = 'data' then
      if ctData in ChunksReaded
       then raise EWavError.Create(rcDATAChunkDublicate)
       else
        begin
         Read(DataSize, 4);
         Position := Position + DataSize;
        end
     else
      begin
       with TUnknownChunk.Create do
        try
         LoadFromStream(Stream);
        finally
         Free;
        end;
      end;
    end;
  end;
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
      SampleCount := FTotalNrOfSamples;
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
   ChunkSize := Position - ChunkStart;
   Position  := ChunkStart + 4;
   Write(ChunkSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

initialization
//  RegisterWaveChunk(TQualityChunk);

end.
