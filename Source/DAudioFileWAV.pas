unit DAudioFileWAV;

interface

uses
  Classes, Contnrs, SysUtils, DAVDCommon, DAudioFile;

type
  TWavEncoding = (etUnknown         =  $0,
                  etPCM             =  $1,
                  etMSADPCM         =  $2,
                  etPCMFLOAT        =  $3,
                  etIBMCVSD         =  $5,
                  etALAW            =  $6,
                  etMuLAW           =  $7,
                  etOKIADPCM        = $10,
                  etDVIADPCM        = $11,
                  etMediaSpaceADPCM = $12,
                  etSierraADPCM     = $13,
                  etG723ADPCM       = $14,
                  etDIGISTD         = $15,
                  etDIGIFIX         = $16,
                  etDiaLogicADPCM   = $17,
                  etYamahaADPCM     = $20,
                  etSONARC          = $21,
                  etTrueSpeech      = $22,
                  etECHOSC1         = $23,
                  etAF36            = $24,
                  etAPTX            = $25,
                  etAF10            = $26,
                  etDolbyAC2        = $30,
                  etGSM610          = $31,
                  etAntexADPCME     = $33,
                  etResVQLPC1       = $34,
                  etResVQLPC2       = $35,
                  etDigiADPCM       = $36,
                  etResCR10         = $37,
                  etVBXADPCM        = $38,
                  etIMAADPCM        = $39,
                  etG721ADPCM       = $40,
                  etMPEG            = $50,
                  etACM             = $55,
                  etXboxADPCM       = $69,
                  etExtended        = $FE,
                  etCreativeADPCM   = $200,
                  etFastSpeech8     = $202,
                  etFastSpeech10    = $203,
                  etFMTownsSND      = $300,
                  etOLIGSM          = $1000,
                  etOLIADPCM        = $1001,
                  etOLICELP         = $1002,
                  etOLISBC          = $1003,
                  etOLIOPR          = $1004,
                  etExperimental    = $FFFF);

  TWavFormatChunkEx = packed record
    cbSize          : Word;     // The size in bytes of the extra information
    SamplesPerBlock : Word;     // number of samples per channel per Block
    ChMask          : Integer;
    ExtFormat       : Word;
    ADPCMCoeffs     : Word;
    GUIDrest        : TGUID;         // was array [0..71] of Byte;
  end;

  TWavADPCMCoefficientSet = packed record
    Coefficient : array [0..1] of SmallInt;
  end;

  TWavADPCMInfoEx =  packed record
    BlockLength     : Word;
    SamplesPerBlock : Word;
    NumCoeff        : Word;
    CoefSets        : array [0..35] of TWavADPCMCoefficientSet; // is that enough?
  end;

  TWavFormatChunk = packed record
    FormatTag       : Word;     // format type
    Channels        : Word;     // number of channels (i.e. mono, stereo, etc.)
    SampleRate      : Integer;  // sample rate
    BytesPerSecond  : Integer;  // = SampleRate * BlockAlign
    BlockAlign      : Word;     // block size of data
    BitsPerSample   : Word;     // = 3, 4, 8, 16 or 32 Bits/sample
    case byte of
      0 : (Extended : TWavFormatChunkEx);
      1 : (ADPCM    : TWavADPCMInfoEx);
  end;

  TADPCM_State = packed record
    PrevSamp_l : SmallInt;
    Index_l    : Byte;
    PrevSamp_r : SmallInt;
    Index_r    : Byte;
  end;

  TADPCM_MS = packed record
    Predictor : array[0..1] of Byte;
    Delta     : array[0..1] of SmallInt;
    Samp1     : array[0..1] of SmallInt;
    Samp2     : array[0..1] of SmallInt;
  end;

  TMFCustomAudioFileWAV = class(TMFCustomAudioFile)
  private
    fFormatChunk      : TWavFormatChunk;
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
    procedure LoadFromFile(FileName: TFileName; HeaderOnly: Boolean = False); override;
    procedure SaveToFile(FileName: TFileName); override;
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

implementation

uses
  DAudioChunks;

{ TMFCustomAudioFileWAV }

constructor TMFCustomAudioFileWAV.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMFCustomAudioFileWAV.Destroy;
begin

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
(*
 case WavType of
             etPCM : result := aeInteger;
        etPCMFLOAT : result := aeFloat;
         etMSADPCM : result := aeMSADPCM;
        wtDVIADPCM : result := aeDVIADPCM;
  wtACM, wtACMMPEG : result := aeACM;
              else   result := aeOther;
 end;
*)
end;

procedure TMFCustomAudioFileWAV.SetBitsPerSample(const Value: Integer);
begin
 with fFormatChunk do
  if BitsPerSample <> Value then
   begin
    BitsPerSample := Value;
(*
    fBytesPerSample := Ceil(Value * 0.125);
    BlockAlign      := Channels * fBytesPerSample;
    BytesPerSecond  := BlockAlign * SampleRate;
    BitsPerSampleChanged;
*)
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

procedure TMFCustomAudioFileWAV.LoadFromFile(FileName: TFileName;
  HeaderOnly: Boolean);
begin
  inherited;

end;

procedure TMFCustomAudioFileWAV.SaveToFile(FileName: TFileName);
begin
  inherited;

end;

end.
