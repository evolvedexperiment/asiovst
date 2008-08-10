unit DAudioChunks;

interface

uses
  Classes, SysUtils;

type
  TChunkName = array [0..3] of Char;

  TCustomChunk = class(TInterfacedPersistent, IStreamPersist)
  protected
    fChunkName : TChunkName;
    fChunkSize : Integer;
    function GetChunkName: string; virtual;
    procedure SetChunkName(const Value: string); virtual;
    function GetChunkSize: Integer; virtual;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; virtual;
    procedure LoadFromStream(Stream : TStream); virtual;
    procedure SaveToStream(Stream : TStream); virtual;
    procedure LoadFromFile(FileName : TFileName); virtual;
    procedure SaveToFile(FileName : TFileName); virtual;
    property ChunkName : string read GetChunkName write SetChunkName;
    property ChunkSize : Integer read GetChunkSize;
  end;

  TUnknownChunk = class(TCustomChunk)
  private
    function GetData(index: Integer): Byte;
    procedure SetData(index: Integer; const Value: Byte);
  protected
    fData : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
  public
    property Data[index : Integer]: Byte read GetData write SetData;
  end;

  TDefinedChunk = class(TCustomChunk)
  protected
    fFilePosition : Cardinal;
    procedure SetChunkName(const Value: string); override;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    class function GetClassChunkName : TChunkName; virtual; abstract;
  published
    property FilePosition : Cardinal read fFilePosition;
  end;

  TDefinedChunkClass = class of TDefinedChunk;

  TFixedDefinedChunk = class(TDefinedChunk)
  private
    function GetStartAddress: Pointer;
    procedure SetStartAddress(const Value: Pointer);
  protected
    fStartAddresses : array of Pointer;
    procedure AssignTo(Dest: TPersistent); override;
    function GetChunkSize: Integer; override;
    property StartAddress: Pointer read GetStartAddress write SetStartAddress;
  public
    class function GetClassChunkSize : Integer; virtual; abstract;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TCustomBinaryChunk = class(TDefinedChunk)
  protected
    fBinaryData : Array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  TCustomTextChunk = class(TDefinedChunk)
  protected
    fText  : string;
    procedure SetText(const Value: string);
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Fact Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TFactRecord = packed record
    FileSize : Cardinal;
  end;

  TFactChunk = class(TFixedDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    FactRecord : TFactRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property FileSize: Cardinal read FactRecord.FileSize write FactRecord.FileSize;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Quality Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  // -> see: http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s2_tcm6-10482.pdf

  TQualityChunkRecord = packed record
    FileSecurityReport : Cardinal; // FileSecurityCode of quality report
    FileSecurityWave   : Cardinal; // FileSecurityCode of BWF wave data
(*
    CHAR BasicData[ ];             // ASCII: << Basic data >>
    CHAR StartModulation[];        // ASCII: << Start modulation data >>
    CHAR QualityEvent[ ];          // ASCII: << Quality event data >>
    CHAR QualityParameter[ ];      // ASCII: << Quality parameter data >>
    CHAR EndModulation[];          // ASCII: << End modulation data >>
    CHAR QualityParameter[ ];      // ASCII: << Quality parameter data >>
    CHAR OperatorComment[ ];       // ASCII: << Comments of operator >>
    CHAR CueSheet[ ];              // ASCII: << Cue sheet data >>
*)
  end;

  TQualityChunk = class(TCustomBinaryChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Link Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  // -> see: http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s4_tcm6-10484.pdf

  TBWFLinkChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property XMLData: string read fText write fText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// AXML Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  // -> see: http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s5_tcm6-10485.pdf

  TBWFAXMLChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  published
    property XMLData: string read fText write fText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////// Custom Cued Text Chunk (Label or Note) //////////////////
  ////////////////////////////////////////////////////////////////////////////

  TCustomCuedTextChunk = class(TDefinedChunk)
  private
    procedure CalculateChunkSize;
  protected
    fText  : string;
    fCueID : Cardinal;
    procedure SetText(const Value: string);
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Label Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TLabelChunk = class(TCustomCuedTextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
  published
    property Text: string read fText write fText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Note Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TNoteChunk = class(TCustomCuedTextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
  published
    property Note: string read fText write fText;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Labeled Text Chunk ////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TLabeledTextRecord = packed record
    CuePointID   : Cardinal;
    SampleLength : Cardinal;
    PurposeID    : Cardinal;
    Country      : Word;
    Language     : Word;
    Dialect      : Word;
    CodePage     : Word;
  end;

  TLabeledTextChunk = class(TDefinedChunk)
  private
    procedure CalculateChunkSize;
  protected
    fText  : string;
    procedure SetText(const Value: string);
    procedure AssignTo(Dest: TPersistent); override;
  public
    LabeledTextRecord : TLabeledTextRecord;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property Text: string read fText write fText;
    property CuePointID: Cardinal read LabeledTextRecord.CuePointID write LabeledTextRecord.CuePointID;
    property SampleLength: Cardinal read LabeledTextRecord.SampleLength write LabeledTextRecord.SampleLength;
    property PurposeID: Cardinal read LabeledTextRecord.PurposeID write LabeledTextRecord.PurposeID;
    property Country: Word read LabeledTextRecord.Country write LabeledTextRecord.Country;
    property Language: Word read LabeledTextRecord.Language write LabeledTextRecord.Language;
    property Dialect: Word read LabeledTextRecord.Dialect write LabeledTextRecord.Dialect;
    property CodePage: Word read LabeledTextRecord.CodePage write LabeledTextRecord.CodePage;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////// Cued File Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TCuedFileChunk = class(TDefinedChunk)
  private
    procedure CalculateChunkSize;
  protected
    fCueID      : Cardinal;
    fMediaType  : Cardinal;
    fBinaryData : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  end;

(*
  ////////////////////////////////////////////////////////////////////////////
  /////////////////////// Associated Data List Chunk /////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAssociatedDataListRecord = packed record
    TypeID : TChunkName;
  end;

  TAssociatedDataListChunk = class(TDefinedChunk)
  private
    procedure CalculateChunkSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    TextListRecord : TTextListRecord;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property TypeID: string read GetTypeID write SetTypeID;
  end;
*)

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Playlist Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TPlaylistSegmentRecord = packed record
    CuePointID      : Cardinal;
    LengthInSamples : Cardinal;
    NumberOfRepeats : Cardinal;
  end;

  TPlaylistSegmentItem = class(TCollectionItem)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    PlaylistSegment : TPlaylistSegmentRecord;
  published
    property CuePointID: Cardinal read PlaylistSegment.CuePointID write PlaylistSegment.CuePointID;
    property LengthInSamples: Cardinal read PlaylistSegment.LengthInSamples write PlaylistSegment.LengthInSamples;
    property NumberOfRepeats: Cardinal read PlaylistSegment.NumberOfRepeats write PlaylistSegment.NumberOfRepeats;
  end;

  TPlaylistChunk = class(TDefinedChunk)
  private
    fCount            : Cardinal; 
    fPlaylistSegments : TOwnedCollection;
    procedure CalculateChunkSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Silent Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TSilentRecord = packed record
    NumberOfSilentSamples : Cardinal;
  end;

  TSilentChunk = class(TFixedDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    SilentRecord : TSilentRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property NumberOfSilentSamples: Cardinal read SilentRecord.NumberOfSilentSamples write SilentRecord.NumberOfSilentSamples;
  end;

(*
  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Wavelist Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TWavelistRecord = packed record
    NumberOfSilentSamples : Cardinal;
  end;

  TWavelistChunk = class(TFixedDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    WavelistRecord : TSilentRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property NumberOfSilentSamples: Cardinal read SilentRecord.NumberOfSilentSamples write SilentRecord.NumberOfSilentSamples;
  end;
*)

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////////// Cue Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TCuePointRecord = packed record
    CuePointName   : Cardinal;
    CuePointPos    : Cardinal;
    CuePointChunk  : TChunkName;
    FilePosStart   : Cardinal;
    BlockStartPos  : Cardinal;
    SampleOffset   : Cardinal;
  end;

  TCueItem = class(TCollectionItem)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    CuePointRecord: TCuePointRecord;
  published
    property CuePointName: Cardinal read CuePointRecord.CuePointName write CuePointRecord.CuePointName;
    property CuePointSamplePosition: Cardinal read CuePointRecord.CuePointPos write CuePointRecord.CuePointPos;
    property FileStartPosition: Cardinal read CuePointRecord.FilePosStart write CuePointRecord.FilePosStart;
    property RelativeBlockStartPosition: Cardinal read CuePointRecord.BlockStartPos write CuePointRecord.BlockStartPos;
    property RelativeBlockSampleOffset: Cardinal read CuePointRecord.SampleOffset write CuePointRecord.SampleOffset;
  end;

  TCueChunk = class(TDefinedChunk)
  private
    fCount         : Cardinal;
    fCueCollection : TOwnedCollection;
    procedure CalculateChunkSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////// Sampler Chunk /////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TSMPTEFormat = (soZero = 0, so24 = 24, so25 = 25, so30Drop = 29, so30 = 30);

  TMidiManufacturer = (mmUnknown            = $00,
                       mmSequentialCircuits = $01,
                       mmBigBriar           = $02,
                       mmOctavePlateau      = $03,
                       mmMoog               = $04,
                       mmPassportDesigns    = $05,
                       mmLexicon            = $06,
                       mmKurzweil           = $07,
                       mmFender             = $08,
                       mmGulbransen         = $09,
                       mmDeltaLabs          = $0A,
                       mmSoundComp          = $0B,
                       mmGeneralElectro     = $0C,
                       mmTechmar            = $0D,
                       mmMatthewsResearch   = $0E,
                       mmOberheim           = $10,
                       mmPAIA               = $11,
                       mmSimmons            = $12,
                       mmDigiDesign         = $13,
                       mmFairlight          = $14,
                       mmJLCooper           = $15,
                       mmLowery             = $16,
                       mmLin                = $17,
                       mmEmu                = $18,
                       mmPeavey             = $1B,
                       mmBonTempi           = $20,
                       mmSIEL               = $21,
                       mmSyntheAxe          = $23,
                       mmHohner             = $24,
                       mmCrumar             = $25,
                       mmSolton             = $26,
                       mmJellinghausMs      = $27,
                       mmCTS                = $28,
                       mmPPG                = $29,
                       mmElka               = $2F,
                       mmCheetah            = $36,
                       mmWaldorf            = $3E,
                       mmKawai              = $40,
                       mmRoland             = $41,
                       mmKorg               = $42,
                       mmYamaha             = $43,
                       mmCasio              = $44,
                       mmKamiyaStudio       = $46,
                       mmAkai               = $47,
                       mmVictor             = $48,
                       mmFujitsu            = $4B,
                       mmSony               = $4C,
                       mmTeac               = $4E,
                       mmMatsushita1        = $50,
                       mmFostex             = $51,
                       mmZoom               = $52,
                       mmMatsushita2        = $54,
                       mmSuzuki             = $55,
                       mmFujiSound          = $56,
                       mmAcousticTecLab     = $57);

  TSamplerRecord = packed record
    Manufacturer       : Cardinal;
    Product            : Cardinal;
    SamplePeriod       : Cardinal;
    MIDIUnityNote      : Cardinal;
    MIDIPitchFraction  : Cardinal;
    SMPTEFormat        : Cardinal; // 0, 24, 25, 29, 30
    SMPTEOffset        : Cardinal;
    NumSampleLoops     : Cardinal;
    SamplerData        : Cardinal;
  end;

  TLoopRecord = packed record
    CuePointID : Cardinal;
    LoopType   : Cardinal;
    LoopStart  : Cardinal;
    LoopEnd    : Cardinal;
    Fraction   : Cardinal;
    PlayCount  : Cardinal;
  end;

  TLoopItem = class(TCollectionItem)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    LoopRecord : TLoopRecord;
  published
    property CuePointID: Cardinal read LoopRecord.CuePointID write LoopRecord.CuePointID;
    property LoopType: Cardinal read LoopRecord.LoopType write LoopRecord.LoopType;
    property LoopStart: Cardinal read LoopRecord.LoopStart write LoopRecord.LoopStart;
    property LoopEnd: Cardinal read LoopRecord.LoopEnd write LoopRecord.LoopEnd;
    property Fraction: Cardinal read LoopRecord.Fraction write LoopRecord.Fraction;
    property PlayCount: Cardinal read LoopRecord.PlayCount write LoopRecord.PlayCount;
  end;

  TSamplerChunk = class(TDefinedChunk)
  private
    fLoopCollection : TOwnedCollection;
    function GetManufacturer: TMidiManufacturer;
    function GetSMPTEFormat: TSMPTEFormat;
    procedure CalculateChunkSize;
    procedure SetManufacturer(const Value: TMidiManufacturer);
    procedure SetSMPTEFormat(const Value: TSMPTEFormat);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    SamplerRecord : TSamplerRecord;
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property Manufacturer: TMidiManufacturer read GetManufacturer write SetManufacturer;
    property Product: Cardinal read SamplerRecord.Product write SamplerRecord.Product;
    property SamplePeriod: Cardinal read SamplerRecord.SamplePeriod write SamplerRecord.SamplePeriod;
    property MIDIUnityNote: Cardinal read SamplerRecord.MIDIUnityNote write SamplerRecord.MIDIUnityNote;
    property MIDIPitchFraction: Cardinal read SamplerRecord.MIDIPitchFraction write SamplerRecord.MIDIPitchFraction;
    property SMPTEFormat: TSMPTEFormat read GetSMPTEFormat write SetSMPTEFormat;
    property SMPTEOffset: Cardinal read SamplerRecord.SMPTEOffset write SamplerRecord.SMPTEOffset;
    property NumSampleLoops: Cardinal read SamplerRecord.NumSampleLoops write SamplerRecord.NumSampleLoops;
    property SamplerData: Cardinal read SamplerRecord.SamplerData write SamplerRecord.SamplerData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////// Instrument Chunk /////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TInstrumentRecord = packed record
    UnshiftedNote : Byte;
    FineTune      : ShortInt;
    Gain_dB       : ShortInt;
    LowNote       : Byte;
    HighNote      : Byte;
    LowVelocity   : Byte;
    HighVelocity  : Byte;
  end;

  TInstrumentChunk = class(TFixedDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    InstrumentRecord : TInstrumentRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property UnshiftedNote: Byte read InstrumentRecord.UnshiftedNote write InstrumentRecord.UnshiftedNote;
    property FineTune: ShortInt read InstrumentRecord.FineTune write InstrumentRecord.FineTune;
    property Gain_dB: ShortInt read InstrumentRecord.Gain_dB write InstrumentRecord.Gain_dB;
    property LowNote: Byte read InstrumentRecord.LowNote write InstrumentRecord.LowNote;
    property HighNote: Byte read InstrumentRecord.HighNote write InstrumentRecord.HighNote;
    property LowVelocity: Byte read InstrumentRecord.LowVelocity write InstrumentRecord.LowVelocity;
    property HighVelocity: Byte read InstrumentRecord.HighVelocity write InstrumentRecord.HighVelocity;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Level Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TLevelChunkRecord = packed record
    dwVersion        : Cardinal; // version information
    dwFormat         : Cardinal; // format of a peak point
                                 //   1 = unsigned char
                                 //   2 = unsigned short
    dwPointsPerValue : Cardinal; // 1 = only positive peak point
                                 // 2 = positive AND negative peak point
    dwBlockSize      : Cardinal; // frames per value
    dwPeakChannels   : Cardinal; // number of channels
    dwNumPeakFrames  : Cardinal; // number of peak frames
    dwPosPeakOfPeaks : Cardinal; // audio sample frame index or 0xFFFFFFFF if unknown
    dwOffsetToPeaks  : Cardinal; // should usually be equal to the size of this header, but could also be higher
    strTimestamp     : array [0..27] of char; // ASCII: time stamp of the peak data
    reserved         : array [0..59] of char; // reserved set to 0x00
    // CHAR peak_envelope_data[] // the peak point data
  end;

  // chunk not yet created...
  // see: http://www.ebu.ch/CMSimages/en/tec_doc_t3285_s3_tcm6-10483.pdf


  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Bext Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TBextRecord = packed record
    Description        : array [0..255] of Char;
    Originator         : array [0..31]  of Char;
    OriginatorRef      : array [0..31]  of Char;
    OriginationDate    : array [0..9]   of Char;
    OriginationTime    : array [0..7]   of Char;
    TimeRefLow         : Integer;
    TimeRefHigh        : Integer;
    Version            : Word;
  end;
  PBextRecord = ^TBextRecord;

  TCustomBextChunk = class(TFixedDefinedChunk)
  private
    function GetDescription: string;
    function GetOriginationDate: string;
    function GetOriginationTime: string;
    function GetOriginator: string;
    function GetOriginatorRef: string;
    procedure SetDescription(const Value: string);
    procedure SetOriginationDate(const Value: string);
    procedure SetOriginationTime(const Value: string);
    procedure SetOriginator(const Value: string);
    procedure SetOriginatorRef(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    BextRecord : TBextRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
  published
    property Description: string read GetDescription write SetDescription;
    property Originator: string read GetOriginator write SetOriginator;
    property OriginatorRef: string read GetOriginatorRef write SetOriginatorRef;
    property OriginationDate: string read GetOriginationDate write SetOriginationDate;
    property OriginationTime: string read GetOriginationTime write SetOriginationTime;
    property TimeRefLow: Integer read BextRecord.TimeRefLow write BextRecord.TimeRefLow;
    property TimeRefHigh: Integer read BextRecord.TimeRefHigh write BextRecord.TimeRefHigh;
    property Version: Word read BextRecord.Version write BextRecord.Version;
  end;

  TBextChunk = class(TCustomBextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
  end;

  TBextChunkOld = class(TCustomBextChunk)
  public
    class function GetClassChunkName : TChunkName; override;
    procedure SaveToStream(Stream: TStream); override;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Cart Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TCartRecord = packed record
    Version            : Integer;
    Title              : array [0..63] of Char;
    Artist             : array [0..63] of Char;
    CutID              : array [0..63] of Char;
    ClientID           : array [0..63] of Char;
    Category           : array [0..63] of Char;
    Classification     : array [0..63] of Char;
    OutCue             : array [0..63] of Char;
    StartDate          : array [0..9] of Char;
    StartTime          : array [0..7] of Char;
    EndDate            : array [0..9] of Char;
    EndTime            : array [0..7] of Char;
    ProducerAppID      : array [0..63] of Char;
    ProducerAppVersion : array [0..63] of Char;
    UserDef            : array [0..63] of Char;
    dbLevelReference   : Integer;
  end;
  PCartRecord = ^TCartRecord;

  TCartChunk = class(TFixedDefinedChunk)
  private
    function GetArtist: string;
    function GetCategory: string;
    function GetClassification: string;
    function GetClientID: string;
    function GetCutID: string;
    function GetEndDate: string;
    function GetEndTime: string;
    function GetOutCue: string;
    function GetProducerAppID: string;
    function GetProducerAppVersion: string;
    function GetStartDate: string;
    function GetStartTime: string;
    function GetTitle: string;
    function GetUserDef: string;
    procedure SetArtist(const Value: string);
    procedure SetCategory(const Value: string);
    procedure SetClassification(const Value: string);
    procedure SetClientID(const Value: string);
    procedure SetCutID(const Value: string);
    procedure SetEndDate(const Value: string);
    procedure SetEndTime(const Value: string);
    procedure SetOutCue(const Value: string);
    procedure SetProducerAppID(const Value: string);
    procedure SetProducerAppVersion(const Value: string);
    procedure SetStartDate(const Value: string);
    procedure SetStartTime(const Value: string);
    procedure SetTitle(const Value: string);
    procedure SetUserDef(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    CartRecord : TCartRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property Version: Integer read CartRecord.Version write CartRecord.Version;
    property Title: string read GetTitle write SetTitle;
    property Artist: string read GetArtist write SetArtist;
    property CutID: string read GetCutID write SetCutID;
    property ClientID: string read GetClientID write SetClientID;
    property Category: string read GetCategory write SetCategory;
    property Classification: string read GetClassification write SetClassification;
    property OutCue: string read GetOutCue write SetOutCue;
    property StartDate: string read GetStartDate write SetStartDate;
    property StartTime: string read GetStartTime write SetStartTime;
    property EndDate: string read GetEndDate write SetEndDate;
    property EndTime: string read GetEndTime write SetEndTime;
    property ProducerAppID: string read GetProducerAppID write SetProducerAppID;
    property ProducerAppVersion: string read GetProducerAppVersion write SetProducerAppVersion;
    property UserDef: string read GetUserDef write SetUserDef;
    property dbLevelReference: Integer read CartRecord.dbLevelReference write CartRecord.dbLevelReference;
  end;

implementation

{ TCustomChunk }

constructor TCustomChunk.Create;
begin
 fChunkName := '';
 fChunkSize := 0;
end;

procedure TCustomChunk.AssignTo(Dest: TPersistent);
begin
 if Dest is TCustomChunk then
  begin
   TCustomChunk(Dest).fChunkName := fChunkName;
   TCustomChunk(Dest).fChunkSize := fChunkSize;
  end
 else inherited;
end;

function TCustomChunk.GetChunkName: string;
begin
 result := fChunkName;
end;

function TCustomChunk.GetChunkSize: Integer;
begin
 result := fChunkSize;
end;

procedure TCustomChunk.LoadFromFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   LoadFromStream(FileStream);
  finally
   Free;
  end;
end;

procedure TCustomChunk.SaveToFile(FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmCreate);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   Free;
  end;
end;

procedure TCustomChunk.LoadFromStream(Stream: TStream);
begin
 Stream.Read(fChunkSize, 4);
end;

procedure TCustomChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   Write(fChunkName[0], 4);
   Write(fChunkSize, 4);
  end;
end;

procedure TCustomChunk.SetChunkName(const Value: string);
var
  ChunkNameSize : Integer;
begin
 ChunkNameSize := Length(Value);
 if ChunkNameSize > 3 then ChunkNameSize := 4;
 Move(Value[1], fChunkName[0], ChunkNameSize);
end;

{ TUnknownChunk }

procedure TUnknownChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TUnknownChunk then
  begin
   SetLength(TUnknownChunk(Dest).fData, Length(fData));
   Move(fData[0], TUnknownChunk(Dest).fData[0], Length(fData));
  end;
end;

function TUnknownChunk.GetData(index: Integer): Byte;
begin
 if (index >= 0) and (index < Length(fData))
  then result := fData[index]
  else result := 0;
end;

procedure TUnknownChunk.LoadFromStream(Stream: TStream);
begin
 with Stream do
  begin
   Position := Position - 4;
   Read(fChunkName, 4);
   inherited;
   assert(fChunkSize < Size);
   SetLength(fData, fChunkSize);
   Read(fData[0], fChunkSize);
  end;
end;

procedure TUnknownChunk.SaveToStream(Stream: TStream);
begin
 with Stream do
  begin
   fChunkSize := Length(fData);
   inherited;
   Write(fData[0], fChunkSize);
  end;
end;

procedure TUnknownChunk.SetData(index: Integer; const Value: Byte);
begin
 if (index >= 0) and (index < Length(fData))
  then fData[index] := Value;
end;

{ TDefinedChunk }

constructor TDefinedChunk.Create;
begin
 inherited;
 fFilePosition := 0;
 fChunkName := GetClassChunkName;
end;

procedure TDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TDefinedChunk
  then TDefinedChunk(Dest).fFilePosition := fFilePosition;
end;

procedure TDefinedChunk.LoadFromStream(Stream: TStream);
var
  TempChunkName : TChunkName;
begin
 with Stream do
  begin
   // Assume chunk name fits the defined one
   Position := Position - 4;
   Read(TempChunkName, 4);
   assert(TempChunkName = fChunkName);
   inherited;
  end;
end;

procedure TDefinedChunk.SetChunkName(const Value: string);
begin
 inherited;
 if Value <> fChunkName
  then raise Exception.Create('Chunk name must always be ''' + fChunkName + '''');
end;

{ TFixedDefinedChunk }

constructor TFixedDefinedChunk.Create;
begin
 inherited;
 SetLength(fStartAddresses, 1);
 fChunkSize := GetClassChunkSize;
end;

procedure TFixedDefinedChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TFixedDefinedChunk then
  begin
   SetLength(TFixedDefinedChunk(Dest).fStartAddresses, Length(fStartAddresses));
   Move(fStartAddresses[0], TFixedDefinedChunk(Dest).fStartAddresses[0], Length(fStartAddresses) * SizeOf(Pointer));
  end;
end;

function TFixedDefinedChunk.GetChunkSize: Integer;
begin
 result := GetClassChunkSize;
end;

function TFixedDefinedChunk.GetStartAddress: Pointer;
begin
 result := fStartAddresses[0];
end;

procedure TFixedDefinedChunk.SetStartAddress(const Value: Pointer);
begin
 fStartAddresses[0] := Value;
end;

procedure TFixedDefinedChunk.LoadFromStream(Stream: TStream);
var
  BytesReaded : Integer;
begin
 inherited;
 with Stream do
  if fChunkSize <= GetClassChunkSize
   then Read(fStartAddresses[0]^, fChunkSize)
   else
    begin
     BytesReaded := Read(fStartAddresses[0]^, GetClassChunkSize);
     assert(BytesReaded = GetClassChunkSize);
     Position := Position + fChunkSize - GetClassChunkSize;
    end;
end;

procedure TFixedDefinedChunk.SaveToStream(Stream: TStream);
var
  BytesWritten: Integer;
begin
 fChunkSize := GetClassChunkSize;
 inherited;
 try
  BytesWritten := Stream.Write(fStartAddresses[0]^, GetClassChunkSize);
  assert(BytesWritten = fChunkSize);
 except
  raise Exception.Create('Wrong Start Addess of Chunk: ' + ChunkName);
 end;
end;

{ TCustomBinaryChunk }

procedure TCustomBinaryChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomBinaryChunk then
  begin
   SetLength(TCustomBinaryChunk(Dest).fBinaryData, Length(fBinaryData));
   Move(fBinaryData, TCustomBinaryChunk(Dest).fBinaryData, SizeOf(fBinaryData));
  end;
end;

procedure TCustomBinaryChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(fBinaryData, fChunkSize);
 Stream.Read(fBinaryData[0], Length(fBinaryData));
end;

procedure TCustomBinaryChunk.SaveToStream(Stream: TStream);
begin
 fChunkSize := Length(fBinaryData);
 inherited;
 Stream.Write(fBinaryData[0], fChunkSize);
end;

{ TCustomTextChunk }

procedure TCustomTextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomTextChunk then
  begin
   TCustomTextChunk(Dest).fText  := fText;
  end;
end;

procedure TCustomTextChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 SetLength(fText, fChunkSize);
 Stream.Read(fText[1], Length(fText));
end;

procedure TCustomTextChunk.SaveToStream(Stream: TStream);
begin
 fChunkSize := Length(fText);
 inherited;
 Stream.Write(fText[1], fChunkSize);
end;

procedure TCustomTextChunk.SetText(const Value: string);
begin
 if fText <> Value then
  begin
   fText := Value;
   fChunkSize := Length(fText);
  end;
end;

{ TFactChunk }

constructor TFactChunk.Create;
begin
 inherited;
 StartAddress := @FactRecord;
end;

procedure TFactChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TFactChunk
  then TFactChunk(Dest).FactRecord := FactRecord;
end;

class function TFactChunk.GetClassChunkName: TChunkName;
begin
 result := 'fact';
end;

class function TFactChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TFactRecord);
end;

{ TBWFLinkChunk }

class function TBWFLinkChunk.GetClassChunkName: TChunkName;
begin
 result := 'link';
end;

{ TBWFAXMLChunk }

class function TBWFAXMLChunk.GetClassChunkName: TChunkName;
begin
 result := 'axml';
end;

{ TQualityChunk }

class function TQualityChunk.GetClassChunkName: TChunkName;
begin
 result := 'qlty';
end;

{ TSilentChunk }

constructor TSilentChunk.Create;
begin
 inherited;
 StartAddress := @SilentRecord;
end;

procedure TSilentChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TSilentChunk
  then TSilentChunk(Dest).SilentRecord := SilentRecord;
end;

class function TSilentChunk.GetClassChunkName: TChunkName;
begin
 result := 'slnt';
end;

class function TSilentChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TSilentRecord);
end;

{ TCustomCuedTextChunk }

procedure TCustomCuedTextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomCuedTextChunk then
  begin
   TCustomCuedTextChunk(Dest).fText  := fText;
   TCustomCuedTextChunk(Dest).fCueID := fCueID;
  end;
end;

procedure TCustomCuedTextChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   SetLength(fText, fChunkSize - SizeOf(Cardinal));
   Read(fCueID, SizeOf(Cardinal));
   Read(fText[1], Length(fText));
  end;
end;

procedure TCustomCuedTextChunk.SaveToStream(Stream: TStream);
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   Write(fCueID, SizeOf(Cardinal));
   Write(fText[1], fChunkSize);
  end;
end;

procedure TCustomCuedTextChunk.SetText(const Value: string);
begin
 fText := Value;
 CalculateChunkSize;
end;

procedure TCustomCuedTextChunk.CalculateChunkSize;
begin
 fChunkSize := Length(fText) + SizeOf(Cardinal);
end;

{ TLabelChunk }

class function TLabelChunk.GetClassChunkName: TChunkName;
begin
 result := 'labl';
end;

{ TNoteChunk }

class function TNoteChunk.GetClassChunkName: TChunkName;
begin
 result := 'note';
end;

{ TLabeledTextChunk }

procedure TLabeledTextChunk.CalculateChunkSize;
begin
 fChunkSize := Length(fText) + SizeOf(TLabeledTextRecord);
end;

procedure TLabeledTextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TLabeledTextChunk then
  begin
   TLabeledTextChunk(Dest).fText             := fText;
   TLabeledTextChunk(Dest).LabeledTextRecord := LabeledTextRecord;
  end;
end;

procedure TLabeledTextChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   SetLength(fText, fChunkSize - SizeOf(TLabeledTextRecord));
   Read(LabeledTextRecord, SizeOf(TLabeledTextRecord));
   Read(fText[1], Length(fText));
  end;
end;

procedure TLabeledTextChunk.SaveToStream(Stream: TStream);
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   Write(LabeledTextRecord, SizeOf(TLabeledTextRecord));
   Write(fText[1], fChunkSize);
  end;
end;

procedure TLabeledTextChunk.SetText(const Value: string);
begin
 fText := Value;
 CalculateChunkSize;
end;

{ TCuedFileChunk }

procedure TCuedFileChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCuedFileChunk then
  begin
   TCuedFileChunk(Dest).fCueID            := fCueID;
   TCuedFileChunk(Dest).fMediaType        := fMediaType;

   // copy binary data:
   SetLength(TCuedFileChunk(Dest).fBinaryData, Length(fBinaryData));
   Move(fBinaryData[0], TCuedFileChunk(Dest).fBinaryData[0], Length(fBinaryData));
  end;
end;

procedure TCuedFileChunk.CalculateChunkSize;
begin
 fChunkSize := SizeOf(fCueID) +  SizeOf(fMediaType) + Length(fBinaryData);
end;

procedure TCuedFileChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   Read(fCueID, SizeOf(fCueID));
   Read(fMediaType, SizeOf(fMediaType));

   // read binary data:
   SetLength(fBinaryData, fChunkSize - SizeOf(fCueID) - SizeOf(fMediaType));
   Read(fBinaryData[0], Length(fBinaryData));
  end;
end;

procedure TCuedFileChunk.SaveToStream(Stream: TStream);
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   Write(fCueID, SizeOf(fCueID));
   Write(fMediaType, SizeOf(fMediaType));

   // write binary data:
   Write(fBinaryData[0], Length(fBinaryData));
  end;
end;

{ TPlaylistSegmentItem }

procedure TPlaylistSegmentItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TPlaylistSegmentItem
  then TPlaylistSegmentItem(Dest).PlaylistSegment := PlaylistSegment
  else inherited;
end;

{ TPlaylistChunk }

constructor TPlaylistChunk.Create;
begin
 inherited;
 fPlaylistSegments := TOwnedCollection.Create(Self, TPlaylistSegmentItem);
end;

destructor TPlaylistChunk.Destroy;
begin
 FreeAndNil(fPlaylistSegments);
 inherited;
end;

procedure TPlaylistChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TPlaylistChunk then
  begin
   TPlaylistChunk(Dest).fCount := fCount;
   TPlaylistChunk(Dest).fPlaylistSegments.Assign(fPlaylistSegments);
  end;
end;

procedure TPlaylistChunk.CalculateChunkSize;
begin
 fChunkSize := SizeOf(Cardinal) + fCount * SizeOf(TPlaylistSegmentRecord);
end;

procedure TPlaylistChunk.LoadFromStream(Stream: TStream);
var
  l : Integer;
begin
 inherited;
 with Stream do
  begin
   Read(fCount, SizeOf(Cardinal));

   // clear all eventually existing playlist segments
   fPlaylistSegments.Clear;

   // load every single playlist segment and add to playlist collection
   for l := 0 to fCount - 1 do
    with TPlaylistSegmentItem(fPlaylistSegments.Add)
     do Read(PlaylistSegment, SizeOf(TPlaylistSegmentRecord));

  end;
end;

procedure TPlaylistChunk.SaveToStream(Stream: TStream);
var
  l : Integer;
begin
 // update fCount:
 fCount := fPlaylistSegments.Count;

 // now recalculate the chunk size:
 CalculateChunkSize;

 // write chunk name & size
 inherited;

 with Stream do
  begin
   // write sampler header
   Write(fCount, SizeOf(Cardinal));

   // write every single playlist segment and add to playlist collection
   for l := 0 to fCount - 1 do
    with TPlaylistSegmentItem(fPlaylistSegments.Items[l])
     do Write(PlaylistSegment, SizeOf(TPlaylistSegmentRecord));
  end;
end;

{ TCueItem }

procedure TCueItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TCueItem
  then TCueItem(Dest).CuePointRecord := CuePointRecord
  else inherited;
end;

{ TCueChunk }

constructor TCueChunk.Create;
begin
 inherited;
 fCueCollection := TOwnedCollection.Create(Self, TCueItem);
end;

destructor TCueChunk.Destroy;
begin
 FreeAndNil(fCueCollection);
 inherited;
end;

procedure TCueChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCueChunk then
  begin
   TCueChunk(Dest).fCount := fCount;
   TCueChunk(Dest).fCueCollection.Assign(fCueCollection);
  end;
end;

procedure TCueChunk.CalculateChunkSize;
begin
 fChunkSize := SizeOf(Cardinal) + fCount * SizeOf(TCuePointRecord);
end;

procedure TCueChunk.LoadFromStream(Stream: TStream);
var
  l : Integer;
begin
 inherited;
 with Stream do
  begin
   Read(fCount, SizeOf(Cardinal));

   // clear all eventually existing cues
   fCueCollection.Clear;

   // load every single playlist segment and add to playlist collection
   for l := 0 to fCount - 1 do
    with TCueItem(fCueCollection.Add)
     do Read(CuePointRecord, SizeOf(TCuePointRecord));
  end;
end;

procedure TCueChunk.SaveToStream(Stream: TStream);
var
  l : Integer;
begin
 // update fCount:
 fCount := fCueCollection.Count;

 // now recalculate the chunk size:
 CalculateChunkSize;

 // write chunk name & size
 inherited;

 with Stream do
  begin
   // write sampler header
   Write(fCount, SizeOf(Cardinal));

   // write every single playlist segment and add to playlist collection
   for l := 0 to fCount - 1 do
    with TCueItem(fCueCollection.Items[l])
     do Write(CuePointRecord, SizeOf(TCuePointRecord));
  end;
end;

{ TLoopItem }

procedure TLoopItem.AssignTo(Dest: TPersistent);
begin
 if Dest is TLoopItem
  then TLoopItem(Dest).LoopRecord := LoopRecord
  else inherited;
end;

{ TSamplerChunk }

constructor TSamplerChunk.Create;
begin
 inherited;
 fLoopCollection := TOwnedCollection.Create(Self, TLoopItem);
end;

destructor TSamplerChunk.Destroy;
begin
 FreeAndNil(fLoopCollection);
 inherited;
end;

procedure TSamplerChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TSamplerChunk then
  begin
   TSamplerChunk(Dest).SamplerRecord := SamplerRecord;
   TSamplerChunk(Dest).fLoopCollection.Assign(fLoopCollection);
  end;
end;

procedure TSamplerChunk.CalculateChunkSize;
begin
 fChunkSize := SizeOf(TSamplerRecord) +
               SamplerRecord.NumSampleLoops * SizeOf(TLoopRecord) +
               SamplerRecord.SamplerData;
end;

function TSamplerChunk.GetManufacturer: TMidiManufacturer;
begin
 result := TMidiManufacturer(SamplerRecord.Manufacturer)
end;

function TSamplerChunk.GetSMPTEFormat: TSMPTEFormat;
begin
 result := TSMPTEFormat(SamplerRecord.SMPTEFormat);
end;

procedure TSamplerChunk.LoadFromStream(Stream: TStream);
var
  l : Integer;
begin
 inherited;
 with Stream do
  begin
   Read(SamplerRecord, SizeOf(TSamplerRecord));

   // clear all eventually existing loop points
   fLoopCollection.Clear;

   // load every single loop and add to loop collection
   for l := 0 to SamplerRecord.NumSampleLoops - 1 do
    with TLoopItem(fLoopCollection.Add)
     do Read(LoopRecord, SizeOf(TLoopRecord));

   // read rest, should only be SamplerRecord.SamplerData
   assert(fChunkSize - SizeOf(TSamplerRecord) = SamplerRecord.SamplerData);
   Position := Position + fChunkSize - SizeOf(TSamplerRecord);
  end;
end;

procedure TSamplerChunk.SaveToStream(Stream: TStream);
var
  l : Integer;
begin
 // make sure some entries are correct:
 SamplerRecord.NumSampleLoops := fLoopCollection.Count;
 SamplerRecord.SamplerData    := 0;

 // now recalculate the chunk size:
 CalculateChunkSize;

 // write chunk name & size
 inherited;

 with Stream do
  begin
   // write sampler header
   Write(SamplerRecord, SizeOf(TSamplerRecord));

   // write every single loop and add to loop collection
   for l := 0 to SamplerRecord.NumSampleLoops - 1 do
    with TLoopItem(fLoopCollection.Items[l])
     do Write(LoopRecord, SizeOf(TLoopRecord));
  end;
end;

procedure TSamplerChunk.SetManufacturer(const Value: TMidiManufacturer);
begin
 SamplerRecord.Manufacturer := Cardinal(Value);
end;

procedure TSamplerChunk.SetSMPTEFormat(const Value: TSMPTEFormat);
begin
 SamplerRecord.SMPTEFormat := Cardinal(Value);
end;

{ TInstrumentChunk }

constructor TInstrumentChunk.Create;
begin
 inherited;
 StartAddress := @InstrumentRecord;
end;

procedure TInstrumentChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TInstrumentChunk
  then TInstrumentChunk(Dest).InstrumentRecord := InstrumentRecord;
end;

class function TInstrumentChunk.GetClassChunkName: TChunkName;
begin
 result := 'inst';
end;

class function TInstrumentChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TInstrumentRecord);
end;

{ TCustomBextChunk }

constructor TCustomBextChunk.Create;
begin
 inherited;
 StartAddress := @BextRecord;
end;

procedure TCustomBextChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCustomBextChunk
  then TCustomBextChunk(Dest).BextRecord := BextRecord;
end;

class function TCustomBextChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TBextRecord);
end;

// Some Wrapper Functions
function TCustomBextChunk.GetDescription: string; begin result := BextRecord.Description; end;
function TCustomBextChunk.GetOriginationDate: string; begin result := BextRecord.OriginationDate; end;
function TCustomBextChunk.GetOriginationTime: string; begin result := BextRecord.OriginationTime; end;
function TCustomBextChunk.GetOriginator: string; begin result := BextRecord.Originator; end;
function TCustomBextChunk.GetOriginatorRef: string; begin result := BextRecord.OriginatorRef; end;

procedure TCustomBextChunk.SetDescription(const Value: string);
begin
 with BextRecord do
  if Length(Value) < SizeOf(Description)
   then Move(Value[1], Description, Length(Value))
   else Move(Value[1], Description, SizeOf(Description));
end;

procedure TCustomBextChunk.SetOriginationDate(const Value: string);
begin
 with BextRecord do
  if Length(Value) < SizeOf(OriginationDate)
   then Move(Value[1], OriginationDate, Length(Value))
   else Move(Value[1], OriginationDate, SizeOf(OriginationDate));
end;

procedure TCustomBextChunk.SetOriginationTime(const Value: string);
begin
 with BextRecord do
  if Length(Value) < SizeOf(OriginationTime)
   then Move(Value[1], OriginationTime, Length(Value))
   else Move(Value[1], OriginationTime, SizeOf(OriginationTime));
end;

procedure TCustomBextChunk.SetOriginator(const Value: string);
begin
 with BextRecord do
  if Length(Value) < SizeOf(Originator)
   then Move(Value[1], Originator, Length(Value))
   else Move(Value[1], Originator, SizeOf(Originator));
end;

procedure TCustomBextChunk.SetOriginatorRef(const Value: string);
begin
 with BextRecord do
  if Length(Value) < SizeOf(OriginatorRef)
   then Move(Value[1], OriginatorRef, Length(Value))
   else Move(Value[1], OriginatorRef, SizeOf(OriginatorRef));
end;

{ TBextChunk }

class function TBextChunk.GetClassChunkName: TChunkName;
begin
 result := 'bext';
end;

{ TBextChunkOld }

class function TBextChunkOld.GetClassChunkName: TChunkName;
begin
 result := 'BEXT';
end;

procedure TBextChunkOld.SaveToStream(Stream: TStream);
begin
 raise Exception.Create('the uppercase version of the bext chunk should not be written anymore!'#10#13'Please use the TBextChunk version');
end;

{ TCartChunkTag }

constructor TCartChunk.Create;
begin
 inherited;
 StartAddress := @CartRecord;
end;

procedure TCartChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TCartChunk
  then TCartChunk(Dest).CartRecord := CartRecord;
end;

class function TCartChunk.GetClassChunkName: TChunkName;
begin
 result := 'cart';
end;

class function TCartChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TCartRecord);
end;

// Some Wrapper Functions 
function TCartChunk.GetArtist: string; begin result := CartRecord.Artist; end;
function TCartChunk.GetCategory: string; begin result := CartRecord.Category; end;
function TCartChunk.GetClassification: string; begin result := CartRecord.Classification; end;
function TCartChunk.GetClientID: string; begin result := CartRecord.ClientID; end;
function TCartChunk.GetCutID: string; begin result := CartRecord.CutID; end;
function TCartChunk.GetEndDate: string; begin result := CartRecord.EndDate; end;
function TCartChunk.GetEndTime: string; begin result := CartRecord.EndTime; end;
function TCartChunk.GetOutCue: string; begin result := CartRecord.OutCue; end;
function TCartChunk.GetProducerAppID: string; begin result := CartRecord.ProducerAppID; end;
function TCartChunk.GetProducerAppVersion: string; begin result := CartRecord.ProducerAppVersion; end;
function TCartChunk.GetStartDate: string; begin result := CartRecord.StartDate; end;
function TCartChunk.GetStartTime: string; begin result := CartRecord.StartTime; end;
function TCartChunk.GetTitle: string; begin result := CartRecord.Title; end;
function TCartChunk.GetUserDef: string; begin result := CartRecord.UserDef; end;

procedure TCartChunk.SetArtist(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(Artist)
   then Move(Value[1], Artist, Length(Value))
   else Move(Value[1], Artist, SizeOf(Artist));
end;

procedure TCartChunk.SetCategory(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(Category)
   then Move(Value[1], Category, Length(Value))
   else Move(Value[1], Category, SizeOf(Category));
end;

procedure TCartChunk.SetClassification(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(Classification)
   then Move(Value[1], Classification, Length(Value))
   else Move(Value[1], Classification, SizeOf(Classification));
end;

procedure TCartChunk.SetClientID(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(ClientID)
   then Move(Value[1], ClientID, Length(Value))
   else Move(Value[1], ClientID, SizeOf(ClientID));
end;

procedure TCartChunk.SetCutID(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(CutID)
   then Move(Value[1], CutID, Length(Value))
   else Move(Value[1], CutID, SizeOf(CutID));
end;

procedure TCartChunk.SetEndDate(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(EndDate)
   then Move(Value[1], EndDate, Length(Value))
   else Move(Value[1], EndDate, SizeOf(EndDate));
end;

procedure TCartChunk.SetEndTime(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(EndTime)
   then Move(Value[1], EndTime, Length(Value))
   else Move(Value[1], EndTime, SizeOf(EndTime));
end;

procedure TCartChunk.SetOutCue(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(OutCue)
   then Move(Value[1], OutCue, Length(Value))
   else Move(Value[1], OutCue, SizeOf(OutCue));
end;

procedure TCartChunk.SetProducerAppID(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(ProducerAppID)
   then Move(Value[1], ProducerAppID, Length(Value))
   else Move(Value[1], ProducerAppID, SizeOf(ProducerAppID));
end;

procedure TCartChunk.SetProducerAppVersion(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(ProducerAppVersion)
   then Move(Value[1], ProducerAppVersion, Length(Value))
   else Move(Value[1], ProducerAppVersion, SizeOf(ProducerAppVersion));
end;

procedure TCartChunk.SetStartDate(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(StartDate)
   then Move(Value[1], StartDate, Length(Value))
   else Move(Value[1], StartDate, SizeOf(StartDate));
end;

procedure TCartChunk.SetStartTime(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(StartTime)
   then Move(Value[1], StartTime, Length(Value))
   else Move(Value[1], StartTime, SizeOf(StartTime));
end;

procedure TCartChunk.SetTitle(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(Title)
   then Move(Value[1], Title, Length(Value))
   else Move(Value[1], Title, SizeOf(Title));
end;

procedure TCartChunk.SetUserDef(const Value: string);
begin
 with CartRecord do
  if Length(Value) < SizeOf(UserDef)
   then Move(Value[1], UserDef, Length(Value))
   else Move(Value[1], UserDef, SizeOf(UserDef));
end;

end.
