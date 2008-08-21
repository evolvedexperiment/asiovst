unit DAudioFileAIFF;

interface

uses
  Classes, Contnrs, SysUtils, DAVDCommon, DAudioFile, DChunkClasses,
  DChunkAIFFFile;

type
  TCustomAudioFileAIFF = class(TCustomAudioFile)
  private
    fCommonChunk     : TAIFFCommonChunk;
    fCommentChunk    : TAIFFCommentChunk;
    fMarkerChunk     : TAIFFMarkerChunk;
    fInstrumentChunk : TAIFFInstrumentChunk;
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
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
  end;

  TAudioFileAIFF  = class(TCustomAudioFileAIFF)
  published
    property SampleRate;
    property ChannelCount;
    property SampleCount;
    property TotalTime;
    property OnLoadData32;
    property OnLoadData64;
    property OnSaveData32;
    property OnSaveData64;
    property BitsPerSample;
    property Encoding;
  end;

implementation

resourcestring
  rcFORMChunkNotFound  = 'This is not a AIFF file!';
  rcFORMSizeMismatch   = 'Filesize mismatch';
  rcAIFFChunkNotFound  = 'This is not a WAVE file!';
  rcFMTChunkDublicate  = 'One format chunk has already been found!';
  rcFACTChunkDublicate = 'One fact chunk has already been found!';
  rcDATAChunkDublicate = 'Only one data chunk supported!';

{ TCustomAudioFileAIFF }

constructor TCustomAudioFileAIFF.Create(AOwner: TComponent);
begin
 inherited;
 fCommonChunk := TAIFFCommonChunk.Create;
end;

destructor TCustomAudioFileAIFF.Destroy;
begin
 FreeAndNil(fCommonChunk);
 if assigned(fCommentChunk)    then FreeAndNil(fCommentChunk);
 if assigned(fMarkerChunk)     then FreeAndNil(fMarkerChunk);
 if assigned(fInstrumentChunk) then FreeAndNil(fInstrumentChunk);
 inherited;
end;

function TCustomAudioFileAIFF.GetBitsPerSample: Byte;
begin
 result := fCommonChunk.SampleSize;
end;

function TCustomAudioFileAIFF.GetChannels: Cardinal;
begin
 result := fCommonChunk.Channels;
end;

function TCustomAudioFileAIFF.GetEncoding: TAudioEncoding;
begin
 result := aeInteger;
end;

function TCustomAudioFileAIFF.GetSampleCount: Cardinal;
begin
 result := fCommonChunk.SampleFrames;
end;

function TCustomAudioFileAIFF.GetSampleRate: Double;
begin
 result := fCommonChunk.SampleRate;
end;

procedure TCustomAudioFileAIFF.SetBitsPerSample(const Value: Byte);
begin
 with fCommonChunk do
  if SampleSize <> Value then
   begin
    SampleSize := Value;
   end;
end;

procedure TCustomAudioFileAIFF.SetChannels(const Value: Cardinal);
begin
 with fCommonChunk do
  if Channels <> Value then
   begin
    inherited;
    Channels := Value;
   end;
end;

procedure TCustomAudioFileAIFF.SetEncoding(const Value: TAudioEncoding);
begin
 if Value <> aeInteger
  then raise Exception.Create('Audio encoding for AIFF is aeInteger only');
end;

procedure TCustomAudioFileAIFF.SetSampleCount(const Value: Cardinal);
begin
 with fCommonChunk do
  if SampleFrames <> Value then
   begin
    inherited;
    SampleFrames := Value;
   end;
end;

procedure TCustomAudioFileAIFF.SetSampleRate(const Value: Double);
begin
 with fCommonChunk do
  if SampleRate <> Value then
   begin
    inherited;
    SampleRate := Value;
   end;
end;

procedure TCustomAudioFileAIFF.LoadFromStream(Stream: TStream);
var
  ChunkName    : TChunkName;
  ChunkSize    : Cardinal;
  ChunkEnd     : Cardinal;
  DataSize     : Cardinal;
begin
 inherited;
 with Stream do
  begin
   // check whether file is a resource interchange file format ('RIFF')
   Read(ChunkName, 4);
   FlipLong(ChunkName);
   if ChunkName <> 'FORM'
    then raise Exception.Create(rcFORMChunkNotFound);

   // check whether the real file size match the filesize stored inside the RIFF chunk
   Read(ChunkSize, 4);
   FlipLong(ChunkSize);
   if (ChunkSize <> Size - Position) and not (ChunkSize = $FFFFFFFF)
    then raise Exception.Create(rcFORMSizeMismatch);

   // now specify the RIFF file to be a WAVE file
   Read(ChunkName, 4);
   FlipLong(ChunkName);
   if ChunkName <> 'AIFF'
    then raise Exception.Create(rcAIFFChunkNotFound);

   // Remove existing optional chunk
   FreeAndNil(fCommentChunk);
   FreeAndNil(fMarkerChunk);
   FreeAndNil(fInstrumentChunk);

   // start parsing here
   ChunkEnd := Position + ChunkSize - 4;
   while Position < ChunkEnd do
    begin
     Read(ChunkName, 4);
     FlipLong(ChunkName);
     if ChunkName = 'COMM' then
      begin
       // load common chunk
       fCommonChunk.LoadFromStream(Stream);
      end else
     if ChunkName = 'SSND' then
      begin
       Read(DataSize, 4);
       FlipLong(DataSize);
       Position := Position + DataSize;
      end else
     if ChunkName = 'MARK' then
      begin
       if assigned(fMarkerChunk)
        then raise Exception.Create('Only one marker chunk allowed');

       // load marker chunk
       fMarkerChunk := TAIFFMArkerChunk.Create;
       fMarkerChunk.LoadFromStream(Stream);
      end else
     if ChunkName = 'COMT' then
      begin
       if assigned(fCommentChunk)
        then raise Exception.Create('Only one comment chunk allowed');

       // load comment chunk
       fCommentChunk := TAIFFCommentChunk.Create;
       fCommentChunk.LoadFromStream(Stream);
      end else
     if ChunkName = 'COMT' then
      begin
       if assigned(fInstrumentChunk)
        then raise Exception.Create('Only one instrument chunk allowed');

       // load comment chunk
       fInstrumentChunk := TAIFFInstrumentChunk.Create;
       fInstrumentChunk.LoadFromStream(Stream);
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

procedure TCustomAudioFileAIFF.SaveToStream(Stream: TStream);
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
   ChunkName := 'FORM';
   FlipLong(ChunkName);
   Write(ChunkName, 4);

   // write dummy filesize yet, since final size is still unknown
   ChunkSize := $FFFFFFFF;
   FlipLong(ChunkSize);
   Write(ChunkSize, 4);

   // now specify the RIFF file to be a WAVE file
   ChunkName := 'AIFF';
   FlipLong(ChunkSize);
   Write(ChunkName, 4);

   // write format chunk
   fCommonChunk.SaveToStream(Stream);


   // ToDo: write data here!

   // finally write filesize
   ChunkSize := Position - ChunkStart;
   Position  := ChunkStart + 4;
   FlipLong(ChunkSize);
   Write(ChunkSize, 4);

   // Reset Position to end of Stream;
   Position := ChunkStart + ChunkSize;
  end;
end;

end.