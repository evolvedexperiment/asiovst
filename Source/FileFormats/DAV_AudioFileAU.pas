unit DAV_AudioFileAU;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_AudioFile, DAV_ChannelDataCoder;

type
  TAUEncoding = (
    aueISDN = 1, auePCM8 = 2, auePCM16 = 3, auePCM24 = 4, auePCM32 = 5,
    aueIEEE32 = 6, aueIEEE64 = 7, aueFragmented = 8, aueDSPprogram = 9,
    aue8bitFixedPoint = 10, aue16bitFixedPoint = 11, aue24bitFixedPoint = 12,
    aue32bitFixedPoint = 13, aue16bitLinearEmphasis = 18,
    aue16bitLinearCompressed = 19, aue16bitLinearCompEmp = 20,
    aueMusicKitDSPCommands = 21, aueADPCM = 23, aueG722ADPCM = 24,
    aueG723_3bitADPCM = 25, aueG723_5bitADPCM = 26, aueALaw = 27);

  TAUHeader = record
    Magic      : Integer;       // = $2E736E64 = '.snd'
    Offset     : Integer;       // Offset to the data in bytes. The minimum valid number is 24 (decimal).
    DataSize   : Integer;       // data size in bytes. If unknown, the value -1 should be used.
    Encoding   : TAUEncoding;   // see TAUEncoding
    SampleRate : Integer;       // sample rate the number of samples/second (e.g., 8000)
    Channels   : Integer;       // the number of interleaved Channels (e.g., 1 for mono, 2 for stereo)
  end;

  TCustomAudioFileAU = class(TCustomAudioFile)
  private
    FAUHeader       : TAUHeader;
    FBytesPerSample : Byte;
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

    function CreateDataCoder: TCustomChannelDataCoder;
    procedure CheckHeader(Stream: TStream);
    procedure ReadAudioDataFromStream(Stream: TStream);
    procedure WriteAudioDataToStream(const Stream: TStream);
  public
    constructor Create(AOwner: TComponent); override;
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;

    // file format identifier
    class function DefaultExtension: string; override;
    class function CanLoad(const Stream: TStream): Boolean; override;
    class function Description: string; override;
    class function FileFormatFilter: string; override;

    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
  end;

  TAudioFileAU  = class(TCustomAudioFileAU)
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
    property Encoding;
  end;

  EAUError = class(Exception);

implementation

{ TCustomAudioFileAU }

class function TCustomAudioFileAU.CanLoad(const Stream: TStream): Boolean;
var
  Magic: TChunkName;
begin
 result := False;
 with Stream do
  begin
   if Size < SizeOf(TAUHeader)
    then exit;

   // Read Header
   Read(Magic, SizeOf(TChunkName));
   if Magic = '.snd'
    then result := True;
  end;
end;

constructor TCustomAudioFileAU.Create(AOwner: TComponent);
begin
 inherited;
 with FAUHeader do
  begin
   Magic      := $646E732E;
   Offset     := 24;
   DataSize   := 0;
   Encoding   := aueIEEE32;
   SampleRate := 0;
   Channels   := 0;
  end;
end;

class function TCustomAudioFileAU.DefaultExtension: string;
begin
 result := 'au';
end;

class function TCustomAudioFileAU.Description: string;
begin
 result := 'AU, NeXT/Sun sound file';
end;

class function TCustomAudioFileAU.FileFormatFilter: string;
begin
 result := Description + '|*.au;*.snd';
end;

function TCustomAudioFileAU.GetBitsPerSample: Byte;
begin
 case FAUHeader.encoding of
    aueISDN : result := 8;
    auePCM8 : result := 8;
   auePCM16 : result := 16;
   auePCM24 : result := 24;
   auePCM32 : result := 32;
  aueIEEE32 : result := 32;
  aueIEEE64 : result := 64;
   aueADPCM : result := 8;
    aueALaw : result := 8;
  else raise EAUError.Create('Unsupported');
 end;
end;

function TCustomAudioFileAU.GetChannels: Cardinal;
begin
 result := FAUHeader.Channels;
end;

function TCustomAudioFileAU.GetEncoding: TAudioEncoding;
begin
 case FAUHeader.Encoding of
    aueISDN  : Result := aeMuLaw;
    auePCM8,
   auePCM16,
   auePCM24,
   auePCM32  : Result := aeInteger;
  aueIEEE32,
  aueIEEE64  : Result := aeFloat;
    aueADPCM : Result := aeADPCM;
     aueALaw : Result := aeADPCM;
        else   Result := aeInteger;
 end;
end;

function TCustomAudioFileAU.GetSampleFrames: Cardinal;
begin
 result := FAUHeader.DataSize;
end;

function TCustomAudioFileAU.GetSampleRate: Double;
begin
 result := FAUHeader.SampleRate;
end;

procedure TCustomAudioFileAU.SetBitsPerSample(const Value: Byte);
begin
 with FAUHeader do
  begin
   case Value of
     8 : Encoding := auePCM8;
    16 : Encoding := auePCM16;
    24 : Encoding := auePCM24;
    32 : if Encoding <> aueIEEE32
          then Encoding := auePCM32;
    64 : Encoding := aueIEEE64;
   end;
   FBytesPerSample := Value shr 3;
  end;
end;

procedure TCustomAudioFileAU.SetSampleFrames(const Value: Cardinal);
begin
 inherited;
 with FAUHeader do
  if Cardinal(DataSize) <> Value then
   begin
    Cardinal(DataSize) := Value;
   end;
end;

procedure TCustomAudioFileAU.SetSampleRate(const Value: Double);
begin
 with FAUHeader do
  if Value <> SampleRate then
   begin
    inherited;
    SampleRate := round(Value);
   end;
end;

procedure TCustomAudioFileAU.SetChannels(const Value: Cardinal);
begin
 with FAUHeader do
  if Value <> Cardinal(Channels) then
   begin
    inherited;
    Channels := Value;
   end;
end;

procedure TCustomAudioFileAU.SetEncoding(const Value: TAudioEncoding);
begin
 with FAUHeader do
  case Value of
   aeInteger : case FBytesPerSample of
                1 : Encoding := auePCM8;
                2 : Encoding := auePCM16;
                3 : Encoding := auePCM24;
                4 : Encoding := auePCM32;
               end;
   aeFloat   : case FBytesPerSample of
                 4 : Encoding := aueIEEE32;
                 8 : Encoding := aueIEEE64;
                else Encoding := aueIEEE32;
               end;
   aeDVIADPCM,
   aeMSADPCM,
   aeADPCM   : Encoding := aueADPCM;
   aeALaw    : Encoding := aueALaw;
   aeMuLaw   : Encoding := aueISDN;
   else raise EAUError.Create('Invalid Encoding for *.au!');
  end;
end;

procedure TCustomAudioFileAU.LoadFromStream(Stream: TStream);
begin
 inherited;
 CheckHeader(Stream);
 if (FAUHeader.DataSize > 0)
  then ReadAudioDataFromStream(Stream);
end;

procedure TCustomAudioFileAU.SaveToStream(Stream: TStream);
var
  FlippedHeader : TAUHeader;
begin
 inherited;
 with Stream do
  begin
   FlippedHeader := FAUHeader;

   // Write Header
   with FlippedHeader do
    begin
     FlipLong(Offset);
     FlipLong(DataSize);
     FlipLong(Encoding);
     FlipLong(SampleRate);
     FlipLong(Channels);
     FlipLong(Magic);
    end;
   Write(FlippedHeader, SizeOf(TAUHeader));

   WriteAudioDataToStream(Stream);
  end;
end;

procedure TCustomAudioFileAU.CheckHeader(Stream: TStream);
begin
 with Stream do
  begin
   // Read Header
   Read(FAUHeader, SizeOf(TAUHeader));
   with FAUHeader do
    begin
     FlipLong(Offset);
     FlipLong(DataSize);
     FlipLong(Encoding);
     FlipLong(SampleRate);
     FlipLong(Channels);

     // some checks
     if Magic <> $646E732E
      then raise EAUError.Create('Not a Sound file!');
     assert(SampleRate > 0);
     assert(Channels > 0);

     if (DataSize > Size - Position)
      then raise EAUError.Create('Datasize larger than the file!');
    end;
  end;
end;

function TCustomAudioFileAU.CreateDataCoder: TCustomChannelDataCoder;
begin
 case FAUHeader.Encoding of
  auePCM8:
   begin
    result := TChannel32DataCoderFixedPoint.Create;
    result.BlockSize := 16384;
    result.ChannelCount := FAUHeader.Channels;
    with TChannel32DataCoderFixedPoint(result)
     do SetBitsAndSampleSize(8, 1);
   end;
  auePCM16:
   begin
    result := TChannel32DataCoderFixedPoint.Create;
    result.BlockSize := 16384;
    result.ChannelCount := FAUHeader.Channels;
    with TChannel32DataCoderFixedPoint(result)
     do SetBitsAndSampleSize(16, 2);
   end;
  auePCM24:
   begin
    result := TChannel32DataCoderFixedPoint.Create;
    result.BlockSize := 16384;
    result.ChannelCount := FAUHeader.Channels;
    with TChannel32DataCoderFixedPoint(result)
     do SetBitsAndSampleSize(24, 3);
   end;
  auePCM32:
   begin
    result := TChannel32DataCoderFixedPoint.Create;
    result.BlockSize := 16384;
    result.ChannelCount := FAUHeader.Channels;
    with TChannel32DataCoderFixedPoint(result)
     do SetBitsAndSampleSize(32, 4);
   end;
  aueIEEE32:
   begin
    result := TChannel32DataCoderFloat32.Create;
    result.BlockSize := 16384;
   end;
  aueIEEE64:
   begin
    result := TChannel32DataCoderFloat64.Create;
    result.BlockSize := 16384;
   end;
  aueISDN:
   begin
    result := TChannel32DataCoderMuLaw.Create;
    result.BlockSize := 16384;
   end;
  aueALaw:
   begin
    result := TChannel32DataCoderALaw.Create;
    result.BlockSize := 16384;
   end;
  else result := nil;
 end;
end;

procedure TCustomAudioFileAU.ReadAudioDataFromStream(Stream: TStream);
var
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
begin
 with Stream do
  begin
   // advance offset
   Position := Position + FAUHeader.Offset;

   if assigned(FOnBeginRead)
    then FOnBeginRead(Self);

   DataDecoder := CreateDataCoder;
   if not assigned(DataDecoder) then exit;

   with DataDecoder do
    try
     Samples := 0;
     while Samples + SampleFrames <= Cardinal(FAUHeader.DataSize) do
      begin
       LoadFromStream(Stream);
       if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
       Samples := Samples + SampleFrames;
      end;

      SampleFrames := Cardinal(FAUHeader.DataSize) - Samples;
      LoadFromStream(Stream);
      if assigned(FOnDecode) then FOnDecode(Self, DataDecoder, Samples);
    finally
     FreeAndNil(DataDecoder);
    end;
  end;
end;

procedure TCustomAudioFileAU.WriteAudioDataToStream(const Stream: TStream);
var
  ChunkEnd    : Cardinal;
  DataDecoder : TCustomChannelDataCoder;
  Samples     : Cardinal;
const
  CZero: Cardinal = 0;
begin
 // check if sample
 if SampleFrames > 0 then
  with Stream do
   begin
    with FAUHeader do
     case Encoding of
         aueISDN, auePCM8 : ChunkEnd := Position + 1 * DataSize * Channels;
                 auePCM16 : ChunkEnd := Position + 2 * DataSize * Channels;
                 auePCM24 : ChunkEnd := Position + 3 * DataSize * Channels;
      auePCM32, aueIEEE32 : ChunkEnd := Position + 4 * DataSize * Channels;
                aueIEEE64 : ChunkEnd := Position + 8 * DataSize * Channels;
      else raise Exception.Create('not yet implemented');          
     end;

    DataDecoder := CreateDataCoder;
    if not assigned(DataDecoder) then exit;

    with DataDecoder do
     try
      Samples   := 0;
      while Samples + SampleFrames <= Cardinal(FAUHeader.DataSize) do
       begin
        if assigned(FOnEncode) then FOnEncode(Self, DataDecoder, Samples);
        SaveToStream(Stream);

        Samples := Samples + SampleFrames;
       end;

       SampleFrames := Cardinal(FAUHeader.DataSize) - Samples;
       if assigned(FOnEncode) then FOnEncode(Self, DataDecoder, Samples);
       SaveToStream(Stream);
      finally
      FreeAndNil(DataDecoder);
     end;

    assert(Stream.Position = ChunkEnd);
    Position := ChunkEnd;
   end;
end;

initialization
  RegisterFileFormat(TAudioFileAU);

end.
