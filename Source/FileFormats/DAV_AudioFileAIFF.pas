unit DAV_AudioFileAIFF;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, Contnrs, SysUtils, DAV_Common, DAV_AudioFile, DAV_ChunkClasses,
  DAV_ChunkAIFFFile, DAV_ChannelDataCoder;

type
  TAiffChunkScan = (acsName, acsAuthor, acsCopyright, acsMarker,
    acsAudioRecording, acsComment, acsInstrument);
  TAiffChunkScans = set of TAiffChunkScan;

  TCustomAudioFileAIFF = class(TCustomAudioFile)
  private
    FIsCompressed        : Boolean;
    FChunkSize           : Cardinal;
    FCommonChunk         : TAIFFCommonChunk;
    FCommentChunk        : TAIFFCommentChunk;
    FNameChunk           : TAIFFNameChunk;
    FAuthorChunk         : TAIFFAuthorChunk;
    FCopyrightChunk      : TAIFFCopyrightChunk;
    FAudioRecordingChunk : TAIFFAudioRecordingChunk;
    FMarkerChunk         : TAIFFMarkerChunk;
    FInstrumentChunk     : TAIFFInstrumentChunk;
    FVersionChunk        : TAIFFFormatVersionChunk;
    FAiffChunkScans      : TAiffChunkScans;
    FAudioDataPosition   : Cardinal;
    function GetAESChannelStatusData: string;
    function GetAIFFName: string;
    function GetAuthor: string;
    function GetCopyright: string;
    procedure SetAESChannelStatusData(const Value: string);
    procedure SetAIFFName(const Value: string);
    procedure SetAuthor(const Value: string);
    procedure SetCopyright(const Value: string);
    procedure ReadAudioDataFromStream(const Stream: TStream);
  protected
    function GetBitsPerSample: Byte; virtual;
    function GetEncoding: TAudioEncoding; virtual;
    function GetChannels: Cardinal; override;
    function GetSampleRate: Double; override;
    function GetSampleFrames: Cardinal; override;

    procedure ProcessAESDChunk(const Stream: TStream); virtual;
    procedure ProcessANNOChunk(const Stream: TStream); virtual;
    procedure ProcessAPPLChunk(const Stream: TStream); virtual;
    procedure ProcessAUTHChunk(const Stream: TStream); virtual;
    procedure ProcessCOMMChunk(const Stream: TStream); virtual;
    procedure ProcessCOMTChunk(const Stream: TStream); virtual;
    procedure ProcessCOPYChunk(const Stream: TStream); virtual;
    procedure ProcessFVERChunk(const Stream: TStream); virtual;
    procedure ProcessINSTChunk(const Stream: TStream); virtual;
    procedure ProcessMARKChunk(const Stream: TStream); virtual;
    procedure ProcessNAMEChunk(const Stream: TStream); virtual;
    procedure ProcessNONEChunk(const Stream: TStream); virtual;
    procedure ProcessSSNDChunk(const Stream: TStream); virtual;
    procedure ProcessUnknownChunk(const Stream: TStream); virtual;

    procedure SetBitsPerSample(const Value: Byte); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
    procedure SetChannels(const Value: Cardinal); override;
    procedure SetSampleRate(const Value: Double); override;
    procedure SetSampleFrames(const Value: Cardinal); override;
    procedure ReadAndSkipSize(const Stream: TStream);
    procedure CheckHeader(const Stream: TStream);
    procedure ParseChunkInformation(const Stream: TStream);
    procedure WriteAudioDataToStream(const Stream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
    property AiffChunkScans: TAiffChunkScans read FAiffChunkScans write
      FAiffChunkScans default [acsName, acsAuthor, acsCopyright, acsMarker,
      acsComment, acsInstrument];
    property Name: string read GetAIFFName write SetAIFFName;
    property Author: string read GetAuthor write SetAuthor;
    property Copyright: string read GetCopyright write SetCopyright;
    property AESChannelStatusData: string read GetAESChannelStatusData write SetAESChannelStatusData;
  end;

  TAudioFileAIFF  = class(TCustomAudioFileAIFF)
  published
    property SampleRate;
    property ChannelCount;
    property SampleFrames;
    property TotalTime;
    property BitsPerSample;
    property Encoding;
    property AiffChunkScans;

    property OnEncode;
    property OnDecode;
  end;

  EAIFFError = class(Exception);

implementation

resourcestring
  RCStrFORMChunkNotFound      = 'This is not a AIFF file!';
  RCStrFORMSizeMismatch       = 'Filesize mismatch';
  RCStrAIFFChunkNotFound      = 'This is not a WAVE file!';
  RCStrFMTChunkDublicate      = 'One format chunk has already been found!';
  RCStrFACTChunkDublicate     = 'One fact chunk has already been found!';
  RCStrDATAChunkDublicate     = 'Only one data chunk supported!';
  RCStrIntegerEncodingOnly    = 'Audio encoding for AIFF is aeInteger only';
  RCStrOneVersionChunkOnly    = 'Only one version chunk allowed';
  RCStrOneCommentChunkOnly    = 'Only one comment chunk allowed';
  RCStrOneMarkerChunkOnly     = 'Only one marker chunk allowed';
  RCStrOneInstrumentChunkOnly = 'Only one instrument chunk allowed';
  RCStrOneCopyrightChunkOnly  = 'Only one copyright chunk allowed';
  RCStrOneNameChunkOnly       = 'Only one name chunk allowed';
  RCStrOneAuthorChunkOnly     = 'Only one author chunk allowed';
  RCStrOneAESChunkOnly        = 'Only one audio recording chunk allowed';
  RCStrNoSoundData = 'No sound data information found!';

{ TCustomAudioFileAIFF }

constructor TCustomAudioFileAIFF.Create(AOwner: TComponent);
begin
 inherited;
 FCommonChunk := TAIFFCommonChunk.Create;
 FAiffChunkScans := [acsName, acsAuthor, acsCopyright, acsMarker, acsComment,
   acsInstrument];
end;

destructor TCustomAudioFileAIFF.Destroy;
begin
 FreeAndNil(FCommonChunk);
 if assigned(FCommentChunk)             then FreeAndNil(FCommentChunk);
 if assigned(FMarkerChunk)              then FreeAndNil(FMarkerChunk);
 if assigned(FInstrumentChunk)          then FreeAndNil(FInstrumentChunk);
 if assigned(FVersionChunk)             then FreeAndNil(FVersionChunk);
 if assigned(FNameChunk)                then FreeAndNil(FNameChunk);
 if assigned(FAuthorChunk)              then FreeAndNil(FAuthorChunk);
 if assigned(FCopyrightChunk)           then FreeAndNil(FCopyrightChunk);
 if assigned(FAudioRecordingChunk)      then FreeAndNil(FAudioRecordingChunk);
 inherited;
end;

function TCustomAudioFileAIFF.GetAuthor: string;
begin
 if assigned(FAuthorChunk)
  then result := FAuthorChunk.Author
  else result := '';
end;

function TCustomAudioFileAIFF.GetBitsPerSample: Byte;
begin
 result := FCommonChunk.SampleSize;
end;

function TCustomAudioFileAIFF.GetChannels: Cardinal;
begin
 result := FCommonChunk.Channels;
end;

function TCustomAudioFileAIFF.GetCopyright: string;
begin
 if assigned(FCopyrightChunk)
  then result := FCopyrightChunk.Copyright
  else result := '';
end;

function TCustomAudioFileAIFF.GetEncoding: TAudioEncoding;
begin
 result := aeInteger;
end;

function TCustomAudioFileAIFF.GetAESChannelStatusData: string;
begin
 if assigned(FAudioRecordingChunk)
  then result := FAudioRecordingChunk.AESChannelStatusData
  else result := '';
end;

function TCustomAudioFileAIFF.GetAIFFName: string;
begin
 if assigned(FNameChunk)
  then result := FNameChunk.Name
  else result := '';
end;

function TCustomAudioFileAIFF.GetSampleFrames: Cardinal;
begin
 result := FCommonChunk.SampleFrames;
end;

function TCustomAudioFileAIFF.GetSampleRate: Double;
begin
 result := FCommonChunk.SampleRate;
end;

procedure TCustomAudioFileAIFF.SetAuthor(const Value: string);
begin
 if not assigned(FAuthorChunk)
  then FAuthorChunk := TAIFFAuthorChunk.Create;
 FAuthorChunk.Author := Value; 
end;

procedure TCustomAudioFileAIFF.SetBitsPerSample(const Value: Byte);
begin
 with FCommonChunk do
  if SampleSize <> Value then
   begin
    SampleSize := Value;
   end;
end;

procedure TCustomAudioFileAIFF.SetChannels(const Value: Cardinal);
begin
 with FCommonChunk do
  if Channels <> SmallInt(Value) then
   begin
    inherited;
    Channels := SmallInt(Value);
   end;
end;

procedure TCustomAudioFileAIFF.SetCopyright(const Value: string);
begin
 if not assigned(FCopyrightChunk)
  then FCopyrightChunk := TAIFFCopyrightChunk.Create;
 FCopyrightChunk.Copyright := Value;
end;

procedure TCustomAudioFileAIFF.SetEncoding(const Value: TAudioEncoding);
begin
 if Value <> aeInteger
  then raise EAIFFError.Create(RCStrIntegerEncodingOnly);
end;

procedure TCustomAudioFileAIFF.SetAESChannelStatusData(const Value: string);
begin
 if not assigned(FAudioRecordingChunk)
  then FAudioRecordingChunk := TAIFFAudioRecordingChunk.Create;
 FAudioRecordingChunk.AESChannelStatusData := Value;
end;

procedure TCustomAudioFileAIFF.SetAIFFName(const Value: string);
begin
 if not assigned(FNameChunk)
  then FNameChunk := TAIFFNameChunk.Create;
 FNameChunk.Name := Value;
end;

procedure TCustomAudioFileAIFF.SetSampleFrames(const Value: Cardinal);
begin
 with FCommonChunk do
  if SampleFrames <> Value then
   begin
    inherited;
    SampleFrames := Value;
   end;
end;

procedure TCustomAudioFileAIFF.SetSampleRate(const Value: Double);
begin
 with FCommonChunk do
  if SampleRate <> Value then
   begin
    inherited;
    SampleRate := Value;
   end;
end;

procedure TCustomAudioFileAIFF.CheckHeader(const Stream: TStream);
var
  ChunkName : TChunkName;
begin
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   if ChunkName <> 'FORM'
    then raise EAIFFError.Create(RCStrFORMChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(FChunkSize, 4);
   FlipLong(FChunkSize);
   if (FChunkSize > ((Size + 1) shr 1) shl 1 - Position) and not (FChunkSize = $FFFFFFFF)
    then raise EAIFFError.Create(RCStrFORMSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   FIsCompressed := ChunkName = 'AIFC';
   if (ChunkName <> 'AIFF') and (ChunkName <> 'AIFC')
    then raise EAIFFError.Create(RCStrAIFFChunkNotFound);
  end;
end;

procedure TCustomAudioFileAIFF.ParseChunkInformation(const Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkEnd     : Cardinal;
begin
 with Stream do
  begin
   // Remove existing optional chunk
   if assigned(FCommentChunk)             then FreeAndNil(FCommentChunk);
   if assigned(FMarkerChunk)              then FreeAndNil(FMarkerChunk);
   if assigned(FInstrumentChunk)          then FreeAndNil(FInstrumentChunk);
   if assigned(FVersionChunk)             then FreeAndNil(FVersionChunk);
   if assigned(FNameChunk)                then FreeAndNil(FNameChunk);
   if assigned(FAuthorChunk)              then FreeAndNil(FAuthorChunk);
   if assigned(FCopyrightChunk)           then FreeAndNil(FCopyrightChunk);
   if assigned(FAudioRecordingChunk)      then FreeAndNil(FAudioRecordingChunk);

   // reset current data positions
   FAudioDataPosition := 0;

   assert(Position = 12);
   ChunkEnd := Position + FChunkSize - 4;

   // start parsing here
   while Stream.Position < ChunkEnd do
    begin
     Read(ChunkName, 4);
     if ChunkName = 'FVER' then ProcessFVERChunk(Stream) else
     if ChunkName = 'COMM' then ProcessCOMMChunk(Stream) else
     if ChunkName = 'SSND' then ProcessSSNDChunk(Stream) else
     if ChunkName = 'MARK' then ProcessMARKChunk(Stream) else
     if ChunkName = 'COMT' then ProcessCOMTChunk(Stream) else
     if ChunkName = 'INST' then ProcessINSTChunk(Stream) else
     if ChunkName = 'AESD' then ProcessAESDChunk(Stream) else
     if ChunkName = 'APPL' then ProcessAPPLChunk(Stream) else
     if ChunkName = 'NAME' then ProcessNAMEChunk(Stream) else
     if ChunkName = 'AUTH' then ProcessAUTHChunk(Stream) else
     if ChunkName = '(c) ' then ProcessCOPYChunk(Stream) else
     if ChunkName = 'ANNO' then ProcessANNOChunk(Stream)
      else ProcessUnknownChunk(Stream);
    end;

   if (FCommonChunk.SampleFrames > 0) then
    if (FAudioDataPosition = 0)
     then raise EAIFFError.Create(RCStrNoSoundData)
     else ReadAudioDataFromStream(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessFVERChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FVersionChunk)
    then raise EAIFFError.Create(RCStrOneVersionChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   FVersionChunk := TAIFFFormatVersionChunk.Create;
   FVersionChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessCOMMChunk(const Stream: TStream);
begin
 with Stream do
  begin
   // load common chunk
   Position := Position - 4;
   FCommonChunk.ForceReadCompression := FIsCompressed;
   FCommonChunk.LoadFromStream(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessSSNDChunk(const Stream: TStream);
var
  DataSize     : Cardinal;
begin
 with Stream do
  begin
   Read(DataSize, 4);
   FlipLong(DataSize);

   // apply padding
   DataSize := ((DataSize + 1) shr 1) shl 1;

   // store SSND chunk position
   FAudioDataPosition := Position;

   Position := Position + DataSize;
  end;
end;

procedure TCustomAudioFileAIFF.ProcessCOMTChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FCommentChunk)
    then raise EAIFFError.Create(RCStrOneCommentChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsComment in FAiffChunkScans then
    begin
     // load comment chunk
     FCommentChunk := TAIFFCommentChunk.Create;
     FCommentChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessMARKChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FMarkerChunk)
    then raise EAIFFError.Create(RCStrOneMarkerChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsMarker in FAiffChunkScans then
    begin
     // load marker chunk
     FMarkerChunk := TAIFFMArkerChunk.Create;
     FMarkerChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessINSTChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FInstrumentChunk)
    then raise EAIFFError.Create(RCStrOneInstrumentChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsInstrument in FAiffChunkScans then
    begin
     // load instrument chunk
     FInstrumentChunk := TAIFFInstrumentChunk.Create;
     FInstrumentChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessNONEChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   exit;
   Position := Position - 4;
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ProcessAPPLChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   Position := Position - 4;
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ProcessAESDChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FAudioRecordingChunk)
    then raise EAIFFError.Create(RCStrOneAESChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsAudioRecording in FAiffChunkScans then
    begin
     // load name chunk
     FAudioRecordingChunk := TAIFFAudioRecordingChunk.Create;
     FAudioRecordingChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessNAMEChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FNameChunk)
    then raise EAIFFError.Create(RCStrOneNameChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsName in FAiffChunkScans then
    begin
     // load name chunk
     FNameChunk := TAIFFNameChunk.Create;
     FNameChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessAUTHChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FAuthorChunk)
    then raise EAIFFError.Create(RCStrOneAuthorChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsAuthor in FAiffChunkScans then
    begin
     // load author chunk
     FAuthorChunk := TAIFFAuthorChunk.Create;
     FAuthorChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessCOPYChunk(const Stream: TStream);
begin
 with Stream do
  begin
   if assigned(FCopyrightChunk)
    then raise EAIFFError.Create(RCStrOneCopyrightChunkOnly);

   // set position to chunk start
   Position := Position - 4;

   if acsCopyright in FAiffChunkScans then
    begin
     // load comment chunk
     FCopyrightChunk := TAIFFCopyrightChunk.Create;
     FCopyrightChunk.LoadFromStream(Stream);
    end
   else ReadAndSkipSize(Stream);
  end;
end;

procedure TCustomAudioFileAIFF.ProcessANNOChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   Position := Position - 4;
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ProcessUnknownChunk(const Stream: TStream);
begin
 with Stream, TAIFFUnknownChunk.Create do
  try
   Position := Position - 4;
   LoadFromStream(Stream);
  finally
   Free;
  end;
end;

procedure TCustomAudioFileAIFF.ReadAndSkipSize(const Stream: TStream);
var
  ChunkSize : Cardinal;
begin
 with Stream do
  begin
   Read(ChunkSize, SizeOf(Cardinal));
   Position := Position + ChunkSize;
  end;
end;

procedure TCustomAudioFileAIFF.LoadFromStream(Stream: TStream);
begin
 inherited;
 CheckHeader(Stream);
 ParseChunkInformation(Stream);
end;

procedure TCustomAudioFileAIFF.SaveToStream(Stream: TStream);
var
  ChunkName  : TChunkName;
  ChunkStart : Cardinal;
  ChunkSize  : Cardinal;
  TempSize   : Cardinal;
begin
 inherited;
 with Stream do
  begin
   // Store chunk start position, just in case the stream position is not 0;
   ChunkStart := Position;

   // first write 'RIFF' (resource interchange file format)
   ChunkName := 'FORM';
   Write(ChunkName, 4);

   // write dummy filesize yet, since final size is still unknown
   ChunkSize := $FFFFFFFF;
   Write(ChunkSize, 4);

   // now specify the RIFF file to be a WAVE file
   ChunkName := 'AIFF';
   Write(ChunkName, 4);

   // write format chunk
   FCommonChunk.SaveToStream(Stream);

   if assigned(FNameChunk) then FNameChunk.SaveToStream(Stream);
   if assigned(FAuthorChunk) then FAuthorChunk.SaveToStream(Stream);
   if assigned(FCopyrightChunk) then FCopyrightChunk.SaveToStream(Stream);
   if assigned(FAudioRecordingChunk) then FAudioRecordingChunk.SaveToStream(Stream);

   WriteAudioDataToStream(Stream);

   // finally write filesize
   ChunkSize := Position - (ChunkStart + 8);
   Position  := ChunkStart + 4;
   TempSize  := ChunkSize;
   FlipLong(TempSize);
   Write(TempSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

procedure TCustomAudioFileAIFF.ReadAudioDataFromStream(const Stream: TStream);
var
  Offset      : Integer;
  BlockAlign  : Integer;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Integer;
begin 
 with Stream do
  begin
   Position := FAudioDataPosition;

   // read offset
   Read(Offset, 4);
   FlipLong(Offset);

   // read block align (even if it is not used here)
   Read(BlockAlign, 4);
   FlipLong(BlockAlign);

   // advance offset
   Position := Position + Offset;

   case FCommonChunk.Compression of
    ctNotAvailable, ctNone:
     begin
      DataDecoder := TChannel32DataCoderFixed.Create;
      DataDecoder.BlockSize := 16384;
      DataDecoder.ChannelCount := FCommonChunk.Channels;
      with TChannel32DataCoderFixed(DataDecoder), FCommonChunk
       do SetBitsAndSampleSize(SampleSize, (SampleSize + 7) div 8);
     end;
    else exit;
   end;

   with DataDecoder do
    try
     Samples   := 0;
     while Samples + SampleFrames <= FCommonChunk.SampleFrames do
      begin
       LoadFromStream(Stream);

       Samples := Samples + SampleFrames;
      end;

      SampleFrames := FCommonChunk.SampleFrames - Samples;
      LoadFromStream(Stream);

    finally
     FreeAndNil(DataDecoder);
    end;
  end;
end;

procedure TCustomAudioFileAIFF.WriteAudioDataToStream(const Stream: TStream);
var
  ChunkName : TChunkName;
  ChunkSize : Cardinal;
const
  CZero: Cardinal = 0;
begin
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
    
   end;
end;

end.
