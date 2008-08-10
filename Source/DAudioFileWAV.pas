unit DAudioFileWAV;

interface

uses
  Classes, Contnrs, SysUtils, DAVDCommon, DAudioFile, DWaveFileTypes,
  DChunkClasses, DChunkWaveFile;

type
  TWaveChunkType = (ctFormat, ctFact, ctData);
  TWaveChunkTypes = set of TWaveChunkType;

  TWaveChunkClass = class of TWaveChunk;

  TWaveChunk = class(TComponent)
  private
    fChunk           : TCustomChunk;
    fCollectionItem  : TCollectionItem;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    destructor Destroy; override;
    class function CanApplyTo(aComponent: TPersistent): Boolean; virtual;
    class function GetDisplayName: string; virtual;
    property CollectionItem: TCollectionItem read fCollectionItem;
    property Chunk: TCustomChunk read fChunk;
  end;

  TWaveChunkCollectionItem = class(TCollectionItem)
  private
    fSubPropertiesSize   : Integer;
    fSubProperties       : Pointer;
    fWaveChunkParameters : string;
    fWaveChunk           : TWaveChunk;
    fEnabled             : Boolean;
    fWaveChunkClass      : TWaveChunkClass;
    fWaveChunkClassName  : string;
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
    property WaveChunk: TWaveChunk read fWaveChunk write fWaveChunk;
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

  TMFCustomAudioFileWAV = class(TMFCustomAudioFile)
  private
    fFormatChunk      : TFormatChunk;
    fFactChunk        : TFactChunk;
//    fBextChunk        : PBextRecord;
//    fCartChunk        : PCartRecord;
    fFileTags         : TObjectList;
    fBytesPerSample   : Integer;
  protected
    function GetBitsPerSample: Integer; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Integer; override;
    function GetSampleRate: Double; override;
    function GetSampleCount: Integer; override;

    procedure SetBitsPerSample(const Value: Integer); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Integer); override;
    procedure SetSampleRate(const Value: Double); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property BitsPerSample : Integer read GetBitsPerSample write SetBitsPerSample;
    property BytesPerSample : Integer read fBytesPerSample;
    property Encoding : TAudioEncoding read GetEncoding write SetEncoding;
  end;

  TMFAudioFileWAV  = class(TMFCustomAudioFileWAV)
  published
    property SampleRate;
    property ChannelCount;
    property SampleCount;
    property BufferSize;
    property TotalTime;
    property OnLoadData32;
    property OnLoadData64;
    property OnSaveData32;
    property OnSaveData64;
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
 if assigned(fChunk)
  then FreeAndNil(fChunk);
 inherited;
end;

procedure TWaveChunk.AssignTo(Dest: TPersistent);
begin
 if (Dest is TWaveChunk) then
  begin
   TWaveChunk(Dest).fChunk.Assign(fChunk);
   TWaveChunk(Dest).fCollectionItem.Assign(fCollectionItem);
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
 fEnabled := True;
 fSubProperties := nil;
end;

destructor TWaveChunkCollectionItem.Destroy;
begin
 if fSubProperties <> nil then FreeMem(fSubProperties);
 if fWaveChunk <> nil then fWaveChunk.Free;
 inherited;
end;

procedure TWaveChunkCollectionItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TWaveChunkCollectionItem then
  with TWaveChunkCollectionItem(Dest) do
   begin
    Enabled := Self.Enabled;
    WaveChunkClassName := Self.WaveChunkClassName;
    fWaveChunk.Assign(Self.fWaveChunk);
   end
 else inherited;
end;

procedure TWaveChunkCollectionItem.DefineProperties(Filer: TFiler);
begin
 inherited;
 Filer.DefineBinaryProperty('WaveChunkParameters', ReadParams, WriteParams, (fWaveChunk <> nil));
end;

function TWaveChunkCollectionItem.GetDisplayName: string;
begin
 if fWaveChunk = nil
  then Result := 'Wave Chunk'
  else Result := fWaveChunk.GetDisplayName;
end;

function TWaveChunkCollectionItem.GetWaveChunkClass: TWaveChunkClass;
begin
 result := fWaveChunkClass;
end;

procedure TWaveChunkCollectionItem.Loaded;
var
  MS: TMemoryStream;
  Reader: TReader;
begin
 inherited;
 if fSubProperties <> nil then
  begin
   MS := TMemoryStream.Create;
   try
    MS.SetSize(fSubPropertiesSize);
    move(fSubProperties^, MS.Memory^, MS.Size);
    Reader := TReader.Create(MS, 4096);
    try
     Reader.IgnoreChildren := False;
     WaveChunkClassName := Reader.ReadString;
     Reader.ReadRootComponent(fWaveChunk);
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
   fSubPropertiesSize := S.Size;
   Getmem(fSubProperties, S.Size);
   S.Read(fSubProperties^, S.Size);
  end
 else fSubPropertiesSize := 0;
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
 raise Exception.Create(AWaveChunkClass.ClassName + ' has not been registered');

 FoundFilter:

 oldFilter := fWaveChunk;

 fWaveChunkClass := AWaveChunkClass;
 if fWaveChunkClass <> nil then
  begin
   fWaveChunk := AWaveChunkClass.Create(nil); // TWaveChunkCollection(GetOwner)
   fWaveChunk.FCollectionItem := Self;
   if oldFilter <> nil
    then fWaveChunk.Assign(oldFilter);
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
    raise Exception.Create(Value + ' has not been registered');
  end;

 fWaveChunkClass := TheClass;
 if fWaveChunk <> nil then
  begin
   fWaveChunk.Free;
   fWaveChunk := nil;
  end;
 FWaveChunkClassName := Value;
 if TheClass <> nil then
  begin
   fWaveChunk := TheClass.Create(nil); // TWaveChunkList(GetOwner).
   fWaveChunk.FCollectionItem := Self;
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
   WriteRootComponent(fWaveChunk);
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

{ TMFCustomAudioFileWAV }

constructor TMFCustomAudioFileWAV.Create(AOwner: TComponent);
begin
 inherited;
 fFormatChunk := TFormatChunk.Create;
end;

destructor TMFCustomAudioFileWAV.Destroy;
begin
 FreeAndNil(fFormatChunk);
 if assigned(fFactChunk) then FreeAndNil(fFactChunk);
 inherited;
end;

function TMFCustomAudioFileWAV.GetChannels: Integer;
begin
 result := fFormatChunk.Channels;
end;

function TMFCustomAudioFileWAV.GetSampleCount: Integer;
begin

end;

function TMFCustomAudioFileWAV.GetSampleRate: Double;
begin
 result := fFormatChunk.SampleRate;
end;

function TMFCustomAudioFileWAV.GetBitsPerSample: Integer;
begin
 result := fFormatChunk.BitsPerSample;
end;

function TMFCustomAudioFileWAV.GetEncoding: TAudioEncoding;
begin
 case fFormatChunk.FormatTag of
             etPCM : result := aeInteger;
        etPCMFLOAT : result := aeFloat;
         etMSADPCM : result := aeMSADPCM;
        etDVIADPCM : result := aeDVIADPCM;
//  etACM, etACMMPEG : result := aeACM;
              else   result := aeOther;
 end;
end;

procedure TMFCustomAudioFileWAV.SetBitsPerSample(const Value: Integer);
begin
 with fFormatChunk do
  if BitsPerSample <> Value then
   begin
    BitsPerSample   := Value;
    fBytesPerSample := (BitsPerSample + 7) div 8; 
    BlockAlign      := Channels * fBytesPerSample;
    BytesPerSecond  := BlockAlign * SampleRate;
//    BitsPerSampleChanged;
   end;
end;

procedure TMFCustomAudioFileWAV.SetChannels(const Value: Integer);
begin
 inherited;
 with fFormatChunk do
  if Channels <> Value then
   begin
    Channels := Value;
    BlockAlign := fBytesPerSample * Value;
    BytesPerSecond := BlockAlign * SampleRate;
   end;
end;

procedure TMFCustomAudioFileWAV.SetEncoding(const Value: TAudioEncoding);
begin

end;

procedure TMFCustomAudioFileWAV.SetSampleRate(const Value: Double);
begin
 inherited;
 with fFormatChunk do
  if SampleRate <> Value then
   begin
    SampleRate := Round(Value);
    BytesPerSecond := BlockAlign * SampleRate;
   end;
end;

// Load/Save

procedure TMFCustomAudioFileWAV.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Cardinal;
  ChunkEnd     : Cardinal;
  ChunksReaded : TWaveChunkTypes;
begin
 inherited;
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'RIFF'
    then raise Exception.Create(rcRIFFChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   if (ChunkSize <> Size - Position) and not (ChunkSize = $FFFFFFFF)
    then raise Exception.Create(rcRIFFSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   if ChunkName <> 'WAVE'
    then raise Exception.Create(rcWAVEChunkNotFound);

   // start parsing here
   ChunksReaded := [];

   ChunkEnd := Position + ChunkSize - 4;
   while Position < ChunkEnd do
    begin
     Read(ChunkName, 4);
     if ChunkName = 'fmt ' then
      if ctFormat in ChunksReaded
       then raise Exception.Create(rcFMTChunkDublicate)
       else
        begin
         fFormatChunk.LoadFromStream(Stream);
         ChunksReaded := ChunksReaded + [ctFormat];
        end else
     if ChunkName = 'fact' then
      if ctFact in ChunksReaded
       then raise Exception.Create(rcFACTChunkDublicate)
       else
        begin
         // check whether fact chunk has already been created
         if not assigned(fFactChunk) then fFactChunk := TFactChunk.Create;

         // now load fact chunk
         fFactChunk.LoadFromStream(Stream);
         ChunksReaded := ChunksReaded + [ctFact];
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

procedure TMFCustomAudioFileWAV.SaveToStream(Stream: TStream);
var
  ChunkName : TChunkName;
  ChunkSize : Cardinal;
begin
 inherited;
 with Stream do
  begin
   // first write 'RIFF' (resource interchange file format)
   ChunkName := 'RIFF';
   Write(ChunkName, 4);

   // write dummy filesize yet, since final size is still unknown
   ChunkSize := $FFFFFFFF;
   Write(ChunkSize, 4);

   // now specify the RIFF file to be a WAVE file
   ChunkName := 'WAVE';
   Write(ChunkName, 4);

  end;
end;

initialization
//  RegisterWaveChunk(TQualityChunk);

end.
