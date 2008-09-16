unit DAV_AudioFileAU;

interface

uses
  Classes, SysUtils, DAV_Common, DAV_AudioFile;

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
    fAUHeader       : TAUHeader;
    fBytesPerSample : Byte;
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
    procedure LoadFromStream(Stream: TStream); override;
    procedure SaveToStream(Stream: TStream); override;
    property BitsPerSample: Byte read GetBitsPerSample write SetBitsPerSample;
    property Encoding: TAudioEncoding read GetEncoding write SetEncoding;
  end;

  TAudioFileAU  = class(TCustomAudioFileAU)
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

{ TCustomAudioFileAU }

constructor TCustomAudioFileAU.Create(AOwner: TComponent);
begin
 inherited;
 with fAUHeader do
  begin
   Magic      := $646E732E;
   Offset     := 24;
   DataSize   := 0;
   Encoding   := aueIEEE32;
   SampleRate := 0;
   Channels   := 0;
  end;
end;

function TCustomAudioFileAU.GetBitsPerSample: Byte;
begin
 case fAUHeader.encoding of
    aueISDN : result := 8;
    auePCM8 : result := 8;
   auePCM16 : result := 16;
   auePCM24 : result := 24;
   auePCM32 : result := 32;
  aueIEEE32 : result := 32;
  aueIEEE64 : result := 64;
   aueADPCM : result := 8;
    aueALaw : result := 8;
  else raise Exception.Create('Unsupported');
 end;
end;

function TCustomAudioFileAU.GetChannels: Cardinal;
begin
 result := fAUHeader.Channels;
end;

function TCustomAudioFileAU.GetEncoding: TAudioEncoding;
begin
 case fAUHeader.Encoding of
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

function TCustomAudioFileAU.GetSampleCount: Cardinal;
begin
 result := fAUHeader.DataSize;
end;

function TCustomAudioFileAU.GetSampleRate: Double;
begin
 result := fAUHeader.SampleRate;
end;

procedure TCustomAudioFileAU.SetBitsPerSample(const Value: Byte);
begin
 with fAUHeader do
  begin
   case Value of
     8 : Encoding := auePCM8;
    16 : Encoding := auePCM16;
    24 : Encoding := auePCM24;
    32 : if Encoding <> aueIEEE32
          then Encoding := auePCM32;
    64 : Encoding := aueIEEE64;
   end;
   fBytesPerSample := Value shr 3;
  end;
end;

procedure TCustomAudioFileAU.SetSampleCount(const Value: Cardinal);
begin
 inherited;
 with fAUHeader do
  if DataSize <> Value then
   begin
    DataSize := Value;
   end;
end;

procedure TCustomAudioFileAU.SetSampleRate(const Value: Double);
begin
 with fAUHeader do
  if Value <> SampleRate then
   begin
    inherited;
    SampleRate := round(Value);
   end;
end;

procedure TCustomAudioFileAU.SetChannels(const Value: Cardinal);
begin
 with fAUHeader do
  if Value <> Channels then
   begin
    inherited;
    Channels := Value;
   end;
end;

procedure TCustomAudioFileAU.SetEncoding(const Value: TAudioEncoding);
begin
 with fAUHeader do
  case Value of
   aeInteger : case fBytesPerSample of
                1 : Encoding := auePCM8;
                2 : Encoding := auePCM16;
                3 : Encoding := auePCM24;
                4 : Encoding := auePCM32;
               end;
   aeFloat   : case fBytesPerSample of
                 4 : Encoding := aueIEEE32;
                 8 : Encoding := aueIEEE64;
                else Encoding := aueIEEE32;
               end;
   aeDVIADPCM,
   aeMSADPCM,
   aeADPCM   : Encoding := aueADPCM;
   aeALaw    : Encoding := aueALaw;
   aeMuLaw   : Encoding := aueISDN;
   else raise Exception.Create('Invalid Encoding for *.au!');
  end;
end;

procedure TCustomAudioFileAU.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // Read Header
   Read(fAUHeader, SizeOf(TAUHeader));
   with fAUHeader do
    begin
     FlipLong(Offset);
     FlipLong(DataSize);
     FlipLong(Encoding);
     FlipLong(SampleRate);
     FlipLong(Channels);

     // some checks
     if Magic <> $646E732E
      then raise Exception.Create('Not a Sound file!');
     assert(SampleRate > 0);
     assert(Channels > 0);
     assert(DataSize <= Size - Position);
    end;

   // ToDo: read data...

  end;
end;

procedure TCustomAudioFileAU.SaveToStream(Stream: TStream);
var
  FlippedHeader : TAUHeader;
begin
 inherited;
 with Stream do
  begin
   FlippedHeader := fAUHeader;

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
   Read(FlippedHeader, SizeOf(TAUHeader));

   // ToDo: write data...

  end;
end;

end.
