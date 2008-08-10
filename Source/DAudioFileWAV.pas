unit DAudioFileWAV;

interface

uses
  Classes, Contnrs, SysUtils, DAVDCommon, DAudioFile;

type
  TWavEncoding = (etUnknown          =  $0,
                  etPCM              =  $1,
                  etMSADPCM          =  $2,
                  etPCMFLOAT         =  $3,
                  etCompaqVSELP      =  $4,
                  etIBMCVSD          =  $5,
                  etALAW             =  $6,
                  etMuLAW            =  $7,
                  etMicrosoftDTS     =  $8,
                  etDRM              =  $9,
                  etWMA9Speech       =  $A,
                  etWMRTVoice        =  $B,
                  etOKIADPCM         = $10,
                  etDVIADPCM         = $11,
                  etMediaSpaceADPCM  = $12,
                  etSierraADPCM      = $13,
                  etG723ADPCM        = $14,
                  etDIGISTD          = $15,
                  etDIGIFIX          = $16,
                  etDiaLogicADPCM    = $17,
                  etMVADPCM          = $18,
                  etHPCU             = $19,
                  etHPDynamicVoice   = $1A,
                  etYamahaADPCM      = $20,
                  etSONARC           = $21,
                  etTrueSpeech       = $22,
                  etECHOSC1          = $23,
                  etAF36             = $24,
                  etAPTX             = $25,
                  etAF10             = $26,
                  etProsody1612      = $27,
                  etMergingTechLRC   = $28,
                  etDolbyAC2         = $30,
                  etGSM610           = $31,
                  etMSNAudio         = $32,
                  etAntexADPCME      = $33,
                  etResVQLPC1        = $34,
                  etResVQLPC2        = $35,
                  etDigiADPCM        = $36,
                  etResCR10          = $37,
                  etVBXADPCM         = $38,
                  etIMAADPCM         = $39,
                  etECHOSC3          = $3A,
                  etRockwellADPCM    = $3B,
                  etDIGITALK         = $3C,
                  etXebecMultimedia  = $3D,
                  etG721ADPCM        = $40,
                  etAntexG728CELP    = $41,
                  etMicrosoftMSG723  = $42,
                  etIBMAVCADPCM      = $43,
                  etITU-TG726        = $45,
                  etRT23orPAC        = $51,
                  etMPEG             = $50,
                  etInSoftRT24       = $52,
                  etInSoftPAC        = $53,
                  etMP3              = $55,
                  etCirrus           = $59,
                  etCirrusLogic      = $60,
                  etESSTechPCM       = $61,
                  etVoxwareInc       = $62,
                  CanopusATRAC       = $63,
                  etAPICOMG726ADPCM  = $64,
                  etAPICOMG722ADPCM  = $65,
                  etMicrosoftDSAT    = $66,
                  etMSDSATDISPLAY    = $67,
                  etXboxADPCM        = $69,
                  etVoxwareAC8       = $70,
                  etVoxwareAC10      = $71,
                  etVoxwareAC16      = $72,
                  etVoxwareAC20      = $73,
                  etVoxwareMetaVoice = $74,
                  etVoxwareMetaSound = $75,
                  etVoxwareRT29HW    = $76,
                  etVoxwareVR12      = $77,
                  etVoxwareVR18      = $78,
                  etVoxwareTQ40      = $79,
                  etVoxwareSC3       = $7A,
                  etVoxwareSC3       = $7B,
                  etSoundsoft        = $80,
                  etVoxwareTQ60      = $81,
                  etMicrosoftMSRT24  = $82,
                  etATandTG729A      = $83,
                  etMP_MVI_MV12      = $84,
                  etDF_G726          = $85,
                  etDF_GSM610        = $86,
                  etItrdSystemsAudio = $88,
                  etOnlive           = $89,
                  etM_FTSX20         = $8A,
                  etITSASG721ADPCM   = $8B,
                  etConvediaG729     = $8C,
                  etNSpC_Inc         = $8D,
                  etSiemensSBC24     = $91,
                  etSF_DolbyAC3APDIF = $92,
                  etMediaSonicG723   = $93,
                  etProsody8kbps     = $94,
                  etZyXELADPCM       = $97,
                  etPhilipsLPCBB     = $98,
                  etStuderProPacked  = $99,
                  etMaldenPhonyTalk  = $A0,
                  etRacalRecorderGSM = $A1,
                  etRecorderG720a    = $A2,
                  etRacalG723_1      = $A3,
                  etRacalTetraACELP  = $A4,
                  etNECAAC           = $B0,
                  etExtended         = $FE,
                  etAAC              = $FF,
                  etRhetorexADPCM    = $100,
                  etIBMuLaw          = $101,
                  etIBMaLaw          = $102,
                  etIBMADPCM         = $103,
                  etVivoG723         = $111,
                  etVivoSiren        = $112,
(*
Philips Speech Processing CELP                                          = $120,
Philips Speech Processing GRUNDIG                                       = $121, 
Digital G.723                                                           = $123, 
Sanyo LD ADPCM                                                          = $125, 
Sipro Lab ACEPLNET                                                      = $130, 
Sipro Lab ACELP4800                                                     = $131, 
Sipro Lab ACELP8V3                                                      = $132, 
Sipro Lab G.729                                                         = $133, 
Sipro Lab G.729A                                                        = $134, 
Sipro Lab Kelvin                                                        = $135, 
VoiceAge AMR                                                            = $136, 
Dictaphone G.726 ADPCM                                                  = $140, 
Qualcomm PureVoice                                                      = $150, 
Qualcomm HalfRate                                                       = $151, 
Ring Zero Systems TUBGSM                                                = $155, 
Microsoft Audio1                                                        = $160, 
Windows Media Audio V2 V7 V8 V9 / DivX audio (WMA) / Alex AC3 Audio     = $161, 
Windows Media Audio Professional V9                                     = $162, 
Windows Media Audio Lossless V9                                         = $163, 
WMA Pro over S/PDIF                                                     = $164, 
UNISYS NAP ADPCM                                                        = $170, 
UNISYS NAP ULAW                                                         = $171, 
UNISYS NAP ALAW                                                         = $172, 
UNISYS NAP 16K                                                          = $173, 
MM SYCOM ACM SYC008 SyCom Technologies                                  = $174, 
MM SYCOM ACM SYC701 G726L SyCom Technologies                            = $175, 
MM SYCOM ACM SYC701 CELP54 SyCom Technologies                           = $176, 
MM SYCOM ACM SYC701 CELP68 SyCom Technologies                           = $177, 
Knowledge Adventure ADPCM                                               = $178, 
Fraunhofer IIS MPEG2AAC                                                 = $180, 
Digital Theater Systems DTS DS                                          = $190,
*)
                  etCreativeADPCM   = $200,
                  etFastSpeech8     = $202,
                  etFastSpeech10    = $203,
(*
$210 = UHER ADPCM
$215 = Ulead DV ACM
$216 = Ulead DV ACM
$220 = Quarterdeck Corp.
$230 = I-Link VC
$240 = Aureal Semiconductor Raw Sport
$241 = ESST AC3
$250 = Interactive Products HSX
$251 = Interactive Products RPELP
$260 = Consistent CS2
$270 = Sony SCX
$271 = Sony SCY
$272 = Sony ATRAC3
$273 = Sony SPC
$280 = TELUM Telum Inc.
$281 = TELUMIA Telum Inc.
$285 = Norcom Voice Systems ADPCM
*)

                  etFMTownsSND      = $300,
                  etFujitsu1        = $301,
                  etFujitsu2        = $302,
                  etFujitsu3        = $303,
                  etFujitsu4        = $304,
                  etFujitsu5        = $305,
                  etFujitsu6        = $306,
                  etFujitsu7        = $307,
                  etFujitsu8        = $308,
                  etOLIGSM          = $1000,
                  etOLIADPCM        = $1001,
                  etOLICELP         = $1002,
                  etOLISBC          = $1003,
                  etOLIOPR          = $1004,
(*
$350 = Micronas Semiconductors, Inc. Development
$351 = Micronas Semiconductors, Inc. CELP833
$400 = Brooktree Digital
$401 = Intel Music Coder (IMC)
$402 = Ligos Indeo Audio
$450 = QDesign Music
$500 = On2 VP7 On2 Technologies
$501 = On2 VP6 On2 Technologies
$680 = AT&T VME VMPCM
$681 = AT&T TCP
$700 = YMPEG Alpha (dummy for MPEG-2 compressor)
$8ae = ClearJump LiteWave (lossless)
$1100 = Lernout & Hauspie
$1101 = Lernout & Hauspie CELP codec
$1102 = Lernout & Hauspie SBC codec
$1103 = Lernout & Hauspie SBC codec
$1104 = Lernout & Hauspie SBC codec
$1400 = Norris Comm. Inc.
$1401 = ISIAudio
$1500 = AT&T Soundspace Music Compression
$181c = VoxWare RT24 speech codec
$181e = Lucent elemedia AX24000P Music codec
$1971 = Sonic Foundry LOSSLESS
$1979 = Innings Telecom Inc. ADPCM
$1c07 = Lucent SX8300P speech codec
$1c0c = Lucent SX5363S G.723 compliant codec
$1f03 = CUseeMe DigiTalk (ex-Rocwell)
$1fc4 = NCT Soft ALF2CD ACM
$2000 = FAST Multimedia DVM
$2001 = Dolby DTS (Digital Theater System)
$2002 = RealAudio 1 / 2 14.4
$2003 = RealAudio 1 / 2 28.8
$2004 = RealAudio G2 / 8 Cook (low bitrate)
$2005 = RealAudio 3 / 4 / 5 Music (DNET)
$2006 = RealAudio 10 AAC (RAAC)
$2007 = RealAudio 10 AAC+ (RACP)
$2500 = Reserved range to $2600 Microsoft
$3313 = makeAVIS (ffvfw fake AVI sound from AviSynth scripts)
$4143 = Divio MPEG-4 AAC audio
$4201 = Nokia adaptive multirate
$4243 = Divio G726 Divio, Inc.
$434c = LEAD Speech
$564c = LEAD Vorbis
$5756 = WavPack Audio
$674f = Ogg Vorbis (mode 1)
$6750 = Ogg Vorbis (mode 2)
$6751 = Ogg Vorbis (mode 3)
$676f = Ogg Vorbis (mode 1+)
$6770 = Ogg Vorbis (mode 2+)
$6771 = Ogg Vorbis (mode 3+)
$7000 = 3COM NBX 3Com Corporation
$706d = FAAD AAC
$7a21 = GSM-AMR (CBR, no SID)
$7a22 = GSM-AMR (VBR, including SID)
$a100 = Comverse Infosys Ltd. G723 1
$a101 = Comverse Infosys Ltd. AVQSBC
$a102 = Comverse Infosys Ltd. OLDSBC
$a103 = Symbol Technologies G729A
$a104 = VoiceAge AMR WB VoiceAge Corporation
$a105 = Ingenient Technologies Inc. G726
$a106 = ISO/MPEG-4 advanced audio Coding
$a107 = Encore Software Ltd G726
$a109 = Speex ACM Codec xiph.org
$dfac = DebugMode SonicFoundry Vegas FrameServer ACM Codec
*)
                  etFLAC            = $F1AC
                  etExtensible      = $FFFE
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
