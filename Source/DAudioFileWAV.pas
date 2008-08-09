unit DAudioFileWAV;

interface

uses
  Classes, Contnrs, SysUtils, DAVDCommon, DAudioFile;

type
  TWavEncoding = (etUnsupported = -1, etPCM = 1, etMSADPCM = 2, etPCMFLOAT = 3,
                  etALAW = 6, etMuLAW = 7, etDVIADPCM = 17, etYamahaADPCM = 20,
                  etGSM = 49, etADPCM = 64, etACMMPEG = 80, etACM = 85 ,
                  etExtended = 254);

  TWavFormatChunkEx = packed record
    cbSize          : Word;     { The size in bytes of the extra information }
    SamplesPerBlock : Word;     { number of samples per channel per Block }
    ChMask          : Integer;
    ExtFormat       : TWavEncoding;  // was word
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
    CoefSets        : array [0..35] of TWavADPCMCoefficientSet; // Is that enough?
  end;

  TWavFormatChunk = packed record
    FormatTag       : Word;    { format type }
    Channels        : Word;    { number of channels (i.e. mono, stereo, etc.) }
    SampleRate      : Integer; { sample rate }
    BytesPerSecond  : Integer;
    BlockAlign      : Word;    { block size of data }
    BitsPerSample   : Word;    { = 3, 4, 8, 16 or 32 Bits/sample }
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
    procedure SetBitsPerSample(const Value: Integer); virtual;
    procedure SetEncoding(const Value: TAudioEncoding); virtual;
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

{ TMFCustomAudioFileWAV }

constructor TMFCustomAudioFileWAV.Create(AOwner: TComponent);
begin
  inherited;

end;

destructor TMFCustomAudioFileWAV.Destroy;
begin

  inherited;
end;

function TMFCustomAudioFileWAV.GetBitsPerSample: Integer;
begin

end;

function TMFCustomAudioFileWAV.GetEncoding: TAudioEncoding;
begin

end;

procedure TMFCustomAudioFileWAV.LoadFromFile(FileName: TFileName;
  HeaderOnly: Boolean);
begin
  inherited;

end;

procedure TMFCustomAudioFileWAV.SaveToFile(FileName: TFileName);
begin
  inherited;

end;

procedure TMFCustomAudioFileWAV.SetBitsPerSample(const Value: Integer);
begin

end;

procedure TMFCustomAudioFileWAV.SetEncoding(const Value: TAudioEncoding);
begin

end;

end.
