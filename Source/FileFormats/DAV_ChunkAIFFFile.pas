unit DAV_ChunkAIFFFile;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_ChunkClasses;

const
  AIFCVersion1 = $A2805140;

type
   TAIFFCompressionType = (ctNotAvailable, ctNone, ctACE2, ctACE8, ctMACE3,
                           ctMACE6, ctUnknown);

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Common Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommonRecord = packed record
    Channels        : SmallInt;
    SampleFrames    : Cardinal;
    SampleSize      : SmallInt;
    SampleRate      : Extended;
  end;

  TAIFFCommonChunk = class(TDefinedChunk) // 'COMM'
  private
    FCompressionType: TAIFFCompressionType;
    FCompressionName: string;
    procedure SetChannels(const Value: SmallInt);
    procedure SetSampleRate(const Value: Extended);
    procedure SetSampleSize(const Value: SmallInt);
    procedure CalculateChunkSize;
    procedure SetCompressionType(const Value: TAIFFCompressionType);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFCommonRecord : TAIFFCommonRecord;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property Channels: SmallInt read AIFFCommonRecord.Channels write SetChannels;
    property SampleFrames: Cardinal read AIFFCommonRecord.SampleFrames write AIFFCommonRecord.SampleFrames;
    property SampleSize: SmallInt read AIFFCommonRecord.SampleSize write SetSampleSize;
    property SampleRate: Extended read AIFFCommonRecord.SampleRate write SetSampleRate;
    property Compression: TAIFFCompressionType read FCompressionType write SetCompressionType;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////////// Form Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFFormRecord = packed record
    FormType: TChunkName; // type of file
  end;

  TAIFFFormChunk = class(TFixedDefinedChunk)
  private
    function GetFormType: string;
    procedure SetFormType(Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFFormRecord : TAIFFFormRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property FormType: string read GetFormType write SetFormType;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Format Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFFormatVersionRecord = packed record
    TimeStamp : Cardinal;  // date of format version
  end;

  TAIFFFormatVersionChunk = class(TFixedDefinedChunk)
  private
    procedure SetTimeStamp(const Value: Cardinal);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
  public
    AIFFFormatVersionRecord : TAIFFFormatVersionRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property TimeStamp: Cardinal read AIFFFormatVersionRecord.TimeStamp write SetTimeStamp;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Sound Data Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFSoundDataRecord = packed record
    Offset    : Cardinal;
    BlockSize : Cardinal;
  end;

  TAIFFSoundDataChunk = class(TDefinedChunk) // 'SSND'
  private
    procedure CalculateChunkSize;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFSoundDataRecord : TAIFFSoundDataRecord;
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property Offset: Cardinal read AIFFSoundDataRecord.Offset write AIFFSoundDataRecord.Offset;
    property BlockSize: Cardinal read AIFFSoundDataRecord.BlockSize write AIFFSoundDataRecord.BlockSize;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Marker Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TMarkerID = Byte;

  TAIFFMarkerRecord = packed record
    MarkerID   : TMarkerID;
    Position   : Cardinal;
  end;

  TAIFFMarkerItem = class(TCollectionItem)
  private
    FMarkerName : string;
    procedure SetMarkerID(const Value: TMarkerID);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    MarkerRecord : TAIFFMarkerRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property DisplayName;
  published
    property MarkerID: TMarkerID read MarkerRecord.MarkerID write SetMarkerID;
    property Position: Cardinal read MarkerRecord.Position write MarkerRecord.Position;
    property MarkerName: string read FMarkerName write FMarkerName;
  end;

  TAIFFMarkerChunk = class(TDefinedChunk) // 'MARK'
  private
    FMarkers : TOwnedCollection;
    procedure CalculateChunkSize;
    function GetMarkerCount: Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property MarkerCount: Byte read GetMarkerCount;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////// Comments Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommentRecord = packed record
    TimeStamp  : Cardinal;
    MarkerID   : TMarkerID;
  end;

  TAIFFCommentItem = class(TCollectionItem)
  private
    FComment : string;
    procedure SetMarkerID(const Value: TMarkerID);
  protected
    function GetDisplayName: string; override;
    procedure AssignTo(Dest: TPersistent); override;
    procedure SetDisplayName(const Value: string); override;
  public
    CommentRecord : TAIFFCommentRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
    property DisplayName;
  published
    property MarkerID: TMarkerID read CommentRecord.MarkerID write SetMarkerID;
    property TimeStamp: Cardinal read CommentRecord.TimeStamp write CommentRecord.TimeStamp;
    property Comment: string read FComment write FComment;
  end;

  TAIFFCommentChunk = class(TDefinedChunk) // 'COMT'
  private
    FComments : TOwnedCollection;
    procedure CalculateChunkSize;
    function GetCommentCount: Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
  published
    property CommentCount: Byte read GetCommentCount;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Instrument Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFInstrumentRecord = packed record // 'INST'
    BaseNote     : Byte;
    Detune       : ShortInt;
    LowNote      : Byte;
    HighNote     : Byte;
    LowVelocity  : Byte;
    HighVelocity : Byte;
    Gain         : ShortInt;
    SustainLoop  : TMarkerID;
    ReleaseLoop  : TMarkerID;
  end;

  TAIFFInstrumentChunk = class(TFixedDefinedChunk)
  private
    procedure SetDetune(Value: ShortInt);
    procedure SetHighVelocity(const Value: Byte);
    procedure SetLowVelocity(const Value: Byte);
    procedure SetBaseNote(const Value: Byte);
    procedure SetHighNote(const Value: Byte);
    procedure SetLowNote(const Value: Byte);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  published
  public
    AIFFInstrumentRecord : TAIFFInstrumentRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property BaseNote: Byte read AIFFInstrumentRecord.BaseNote write SetBaseNote;
    property Detune: ShortInt read AIFFInstrumentRecord.Detune write SetDetune;
    property LowNote: Byte read AIFFInstrumentRecord.LowNote write SetLowNote;
    property HighNote: Byte read AIFFInstrumentRecord.HighNote write SetHighNote;
    property LowVelocity: Byte read AIFFInstrumentRecord.LowVelocity write SetLowVelocity;
    property HighVelocity: Byte read AIFFInstrumentRecord.HighVelocity write SetHighVelocity;
    property Gain: ShortInt read AIFFInstrumentRecord.Gain write AIFFInstrumentRecord.Gain;
    property SustainLoop: TMarkerID read AIFFInstrumentRecord.SustainLoop write AIFFInstrumentRecord.SustainLoop;
    property ReleaseLoop: TMarkerID read AIFFInstrumentRecord.ReleaseLoop write AIFFInstrumentRecord.ReleaseLoop;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// MIDI Chunk /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFMIDIChunk = class(TDefinedChunk)
  private
    function GetMIDIData(index: Integer): Byte;
    procedure SetMIDIData(index: Integer; const Value: Byte);
  protected
    FMIDIData : array of Byte;
    procedure AssignTo(Dest: TPersistent); override;
  public
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  public
    class function GetClassChunkName: TChunkName; override;
    property MIDIData[index : Integer]: Byte read GetMIDIData write SetMIDIData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////// Audio Recording Chunk ////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFAudioRecordingRecord = packed record 
    AESChannelStatusData : array [0..23] of char;
  end;

  TAIFFAudioRecordingChunk = class(TFixedDefinedChunk) // 'AESD'
  private
    function GetAESChannelStatusData: string;
    procedure SetAESChannelStatusData(const Value: string);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AudioRecordingRecord : TAIFFAudioRecordingRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property AESChannelStatusData: string read GetAESChannelStatusData write SetAESChannelStatusData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////// Application Specific Chunk //////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFApplicationSpecificChunk = class(TDefinedChunk) // 'APPL'
  private
    function GetApplicationSignature: string;
    function GetData(index: Integer): Byte;
    procedure CalculateChunkSize;
    procedure SetApplicationSignature(const Value: string);
    procedure SetData(index: Integer; const Value: Byte);
  protected
    FApplicationSignature : TChunkName;
    FApplicationData      : string;
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;
    property ApplicationData[index : Integer]: Byte read GetData write SetData;
  published
    property ApplicationSignature: string read GetApplicationSignature write SetApplicationSignature;
    property ApplicationDataAsString: string read FApplicationData write FApplicationData;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Text Chunks /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFNameChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAIFFAuthorChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAIFFCopyrightChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;

  TAIFFAnnotationChunk = class(TCustomTextChunk)
  public
    class function GetClassChunkName: TChunkName; override;
  end;


implementation

{ TAIFFCommonChunk }

constructor TAIFFCommonChunk.Create;
begin
 inherited;

 // set defaults

 with AIFFCommonRecord do
  begin
   Channels       := 1;       // one channel
   SampleRate     := 44100;   // 44.1 kHz (CD quality)
   SampleSize     := 16;      // 16bit
   SampleFrames   := 0;       // no data yet
  end;
 FCompressionType := ctNotAvailable;

 CalculateChunkSize; 
end;

class function TAIFFCommonChunk.GetClassChunkName: TChunkName;
begin
 result := 'COMM';
end;

procedure TAIFFCommonChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommonChunk then
  with TAIFFCommonChunk(Dest) do
   begin
    AIFFCommonRecord := Self.AIFFCommonRecord;
    FCompressionType := Self.FCompressionType;
  end;
end;

procedure TAIFFCommonChunk.CalculateChunkSize;
begin
 FChunkSize := SizeOf(AIFFCommonRecord);
 if FCompressionType <> ctNotAvailable
  then FChunkSize := FChunkSize + SizeOf(TChunkName) + Length(FCompressionName);
end;

procedure TAIFFCommonChunk.LoadFromStream(Stream: TStream);
var
  CompTypeID :TChunkName;
begin
 inherited;
 with Stream do
  begin
   // load basic header first
   Read(AIFFCommonRecord, SizeOf(TAIFFCommonRecord));

   // flip header
   with AIFFCommonRecord do
    begin
     FlipWord(Channels);
     FlipLong(SampleFrames);
     FlipWord(SampleSize);
     FlipExtended(SampleRate);
    end;

   // exit if no addition information are available
   if FChunkSize = SizeOf(TAIFFCommonRecord) then
    begin
     Compression := ctNotAvailable;
     exit;
    end;

   // read additional compression information
   Read(CompTypeID, SizeOf(TChunkName));
   FlipLong(CompTypeID);
   if CompTypeID = 'NONE' then FCompressionType := ctNone else
   if CompTypeID = 'ACE2' then FCompressionType := ctACE2 else
   if CompTypeID = 'ACE8' then FCompressionType := ctACE8 else
   if CompTypeID = 'MAC3' then FCompressionType := ctMACE3 else
   if CompTypeID = 'MAC8' then FCompressionType := ctMACE6
    else FCompressionType := ctUnknown;

   // set position to end of chunk
   Position := Position + FChunkSize - SizeOf(TAIFFCommonRecord) - SizeOf(TChunkName);
  end;
end;

procedure TAIFFCommonChunk.SaveToStream(Stream: TStream);
var
  FlippedAIFFCommonRecord : TAIFFCommonRecord;
  CompressionTypeID       : TChunkName;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // save basic header first (need to be flipped first)
   FlippedAIFFCommonRecord := AIFFCommonRecord;
   with FlippedAIFFCommonRecord do
    begin
     FlipWord(Channels);
     FlipLong(SampleFrames);
     FlipWord(SampleSize);
     FlipExtended(SampleRate);
    end;
   Write(FlippedAIFFCommonRecord, SizeOf(TAIFFCommonRecord));

   // write compression info if necessary
   if FCompressionType <> ctNotAvailable then
    begin
     case FCompressionType of
         ctNone : CompressionTypeID := 'None';
         ctACE2 : CompressionTypeID := 'ACE2';
         ctACE8 : CompressionTypeID := 'ACE8';
        ctMACE3 : CompressionTypeID := 'MAC3';
        ctMACE6 : CompressionTypeID := 'MAC6';
      ctUnknown : raise Exception.Create('Not supported');
     end;
     Write(CompressionTypeID, SizeOf(TChunkName));
     Write(FCompressionName, Length(FCompressionName));
    end;
  end;
end;

procedure TAIFFCommonChunk.SetChannels(const Value: SmallInt);
begin
 if AIFFCommonRecord.Channels <> Value then
  begin
   if Value <= 0
    then raise Exception.Create('Channel count must be > 0');
   AIFFCommonRecord.Channels := Value;
  end;
end;

procedure TAIFFCommonChunk.SetCompressionType(const Value: TAIFFCompressionType);
begin
 if FCompressionType <> Value then
  begin
   FCompressionType := Value;
   case FCompressionType of
    ctNotAvailable : FCompressionName := 'not available';
            ctNone : FCompressionName := 'not compressed';
            ctACE2 : FCompressionName := 'ACE 2-to-1';
            ctACE8 : FCompressionName := 'ACE 8-to-1';
           ctMACE3 : FCompressionName := 'MACE 3-to-1';
           ctMACE6 : FCompressionName := 'MACE 6-to-1';
   end;
  end;
end;

procedure TAIFFCommonChunk.SetSampleRate(const Value: Extended);
begin
 if AIFFCommonRecord.SampleRate <> Value then
  begin
   if Value <= 0
    then raise Exception.Create('Sample rate must be > 0');
   AIFFCommonRecord.SampleRate := Value;
  end;
end;

procedure TAIFFCommonChunk.SetSampleSize(const Value: SmallInt);
begin
 if AIFFCommonRecord.SampleSize <> Value then
  begin
   if Value <= 0
    then raise Exception.Create('Sample rate must be > 0');
   AIFFCommonRecord.SampleSize := Value;
  end;
end;

{ TAIFFFormChunk }

constructor TAIFFFormChunk.Create;
begin
 inherited;
 with AIFFFormRecord do
  begin
   FormType := 'AIFF';
  end;
end;

procedure TAIFFFormChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFFormChunk then
  begin
   TAIFFFormChunk(Dest).AIFFFormRecord := AIFFFormRecord;
  end;
end;

class function TAIFFFormChunk.GetClassChunkName: TChunkName;
begin
 result := 'FORM';
end;

class function TAIFFFormChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TAIFFFormRecord);
end;

function TAIFFFormChunk.GetFormType: string;
begin
 result := AIFFFormRecord.FormType;
end;

procedure TAIFFFormChunk.SetFormType(Value: string);
begin
 with AIFFFormRecord do
  if Value = 'AIFF' then FormType := 'AIFF' else
  if Value = 'AIFC' then FormType := 'AIFC'
   else raise Exception.Create('Unknown form type');
end;

{ TAIFFFormatVersionChunk }

constructor TAIFFFormatVersionChunk.Create;
begin
 inherited;
 with AIFFFormatVersionRecord do
  begin
   TimeStamp := AIFCVersion1;
  end;
end;

procedure TAIFFFormatVersionChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFFormatVersionChunk then
  begin
   TAIFFFormatVersionChunk(Dest).AIFFFormatVersionRecord := AIFFFormatVersionRecord;
  end;
end;

class function TAIFFFormatVersionChunk.GetClassChunkName: TChunkName;
begin
 result := 'FVER';
end;

class function TAIFFFormatVersionChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TAIFFFormatVersionRecord);
end;

procedure TAIFFFormatVersionChunk.SetTimeStamp(const Value: Cardinal);
begin
 if Value <> AIFCVersion1
  then raise Exception.Create('Only one version from May 23, 1990, 2:40pm is supported yet');
end;

{ TAIFFSoundDataChunk }

constructor TAIFFSoundDataChunk.Create;
begin
 inherited;
 with AIFFSoundDataRecord do
  begin
   Offset    := 0;
   BlockSize := 0;
  end;
end;

procedure TAIFFSoundDataChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // load basic header first
   Read(AIFFSoundDataRecord, SizeOf(TAIFFSoundDataRecord));

   // flip header
   with AIFFSoundDataRecord do
    begin
     FlipLong(Offset);
     FlipLong(BlockSize);
    end;

   // set position to end of chunk
   Position := Position + FChunkSize - SizeOf(TAIFFSoundDataRecord);
  end;
end;

procedure TAIFFSoundDataChunk.SaveToStream(Stream: TStream);
var
  FlippedAIFFSoundDataRecord : TAIFFSoundDataRecord;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // save basic header first (need to be flipped first)
   FlippedAIFFSoundDataRecord := AIFFSoundDataRecord;
   with FlippedAIFFSoundDataRecord do
    begin
     FlipLong(Offset);
     FlipLong(BlockSize);
    end;
   Write(FlippedAIFFSoundDataRecord, SizeOf(TAIFFSoundDataRecord));

  end;
end;

procedure TAIFFSoundDataChunk.CalculateChunkSize;
begin
 FChunkSize := SizeOf(AIFFSoundDataRecord);
end;

procedure TAIFFSoundDataChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFSoundDataChunk then
  begin
   TAIFFSoundDataChunk(Dest).AIFFSoundDataRecord := AIFFSoundDataRecord;
  end;
end;

{ TAIFFMarkerItem }

procedure TAIFFMarkerItem.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMarkerItem
  then TAIFFMarkerItem(Dest).MarkerRecord := MarkerRecord;
end;

function TAIFFMarkerItem.GetDisplayName: string;
begin
 result := FMarkerName;
end;

function TAIFFMarkerItem.GetSize: Cardinal;
begin
 result := SizeOf(TAIFFMarkerRecord) + Length(FMarkerName) + 1;
end;

procedure TAIFFMarkerItem.LoadFromStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // read marker header
   Read(MarkerRecord, SizeOf(TAIFFMarkerRecord));

   // now read the marker string
   Read(StringSize, SizeOf(Byte));
   SetLength(FMarkerName, StringSize);
   Read(FMarkerName[1], StringSize);
  end;
end;

procedure TAIFFMarkerItem.SaveToStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // write marker header
   Write(MarkerRecord, SizeOf(TAIFFMarkerRecord));

   // now write the marker string
   StringSize := Length(FMarkerName);
   Write(StringSize, SizeOf(Byte));
   Write(FMarkerName[1], StringSize);
  end;
end;

procedure TAIFFMarkerItem.SetDisplayName(const Value: string);
begin
 FMarkerName := Value;
 inherited;
end;

procedure TAIFFMarkerItem.SetMarkerID(const Value: TMarkerID);
begin
 with MarkerRecord do
  if Value <> MarkerID then
   begin
    if Value = 0 then raise Exception.Create('MarkerID must be > 0');
    MarkerID := Value;
   end;
end;

{ TAIFFMarkerChunk }

constructor TAIFFMarkerChunk.Create;
begin
 inherited;
 FMarkers := TOwnedCollection.Create(Self, TAIFFMarkerItem);
 CalculateChunkSize;
end;

destructor TAIFFMarkerChunk.Destroy;
begin
 FreeAndNil(FMarkers);
 inherited;
end;

class function TAIFFMarkerChunk.GetClassChunkName: TChunkName;
begin
 result := 'MARK';
end;

function TAIFFMarkerChunk.GetMarkerCount: Byte;
begin
 result := FMarkers.Count;
end;

procedure TAIFFMarkerChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMarkerChunk
  then TAIFFMarkerChunk(Dest).FMarkers.Assign(FMarkers);
end;

procedure TAIFFMarkerChunk.CalculateChunkSize;
var
  i : Integer;
begin
 FChunkSize := SizeOf(Byte);
 for i := 0 to FMarkers.Count - 1
  do FChunkSize := FChunkSize + TAIFFMarkerItem(FMarkers.Items[i]).GetSize;
end;

procedure TAIFFMarkerChunk.LoadFromStream(Stream: TStream);
var
  MarkerCount, i : Integer;
begin
 inherited;
 with Stream do
  begin
   // load number of markers
   Read(MarkerCount, SizeOf(Byte));

   // clear existing markers
   FMarkers.Clear;

   // load every single marker
   for i := 0 to MarkerCount - 1 do
    with TAIFFMarkerItem(FMarkers.Add)
     do LoadFromStream(Stream);

   // set position to end of chunk
   Position := Position + FChunkSize - SizeOf(Byte);
  end;
end;

procedure TAIFFMarkerChunk.SaveToStream(Stream: TStream);
var
  MarkerCount, i : Integer;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // store number of markers
   MarkerCount := FMarkers.Count;
   Write(MarkerCount, SizeOf(Byte));

   for i := 0 to FMarkers.Count - 1
    do TAIFFMarkerItem(FMarkers.Items[i]).SaveToStream(Stream);
  end;
end;

{ TAIFFCommentItem }

procedure TAIFFCommentItem.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommentItem
  then TAIFFCommentItem(Dest).CommentRecord := CommentRecord;
end;

function TAIFFCommentItem.GetDisplayName: string;
begin
 result := FComment;
end;

function TAIFFCommentItem.GetSize: Cardinal;
begin
 result := SizeOf(TAIFFCommentRecord) + Length(FComment) + 1;
end;

procedure TAIFFCommentItem.LoadFromStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // read comment header
   Read(CommentRecord, SizeOf(TAIFFCommentRecord));

   // now read the comment string
   Read(StringSize, SizeOf(Byte));
   SetLength(FComment, StringSize);
   Read(FComment[1], StringSize);
  end;
end;

procedure TAIFFCommentItem.SaveToStream(Stream: TStream);
var
  StringSize : Integer;
begin
 with Stream do
  begin
   // write comment header
   Write(CommentRecord, SizeOf(TAIFFCommentRecord));

   // now write the comment string
   StringSize := Length(FComment);
   Write(StringSize, SizeOf(Byte));
   Write(FComment[1], StringSize);
  end;
end;

procedure TAIFFCommentItem.SetDisplayName(const Value: string);
begin
 FComment := Value;
 inherited;
end;

procedure TAIFFCommentItem.SetMarkerID(const Value: TMarkerID);
begin
 with CommentRecord do
  if Value <> MarkerID then
   begin
    if Value = 0 then raise Exception.Create('MarkerID must be > 0');
    MarkerID := Value;
   end;
end;

{ TAIFFCommentChunk }

constructor TAIFFCommentChunk.Create;
begin
 inherited;
 FComments := TOwnedCollection.Create(Self, TAIFFCommentItem);
 CalculateChunkSize;
end;

destructor TAIFFCommentChunk.Destroy;
begin
 FreeAndNil(FComments);
 inherited;
end;

procedure TAIFFCommentChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommentChunk
  then TAIFFCommentChunk(Dest).FComments.Assign(FComments);
end;

procedure TAIFFCommentChunk.CalculateChunkSize;
var
  i : Integer;
begin
 FChunkSize := SizeOf(Byte);
 for i := 0 to FComments.Count - 1
  do FChunkSize := FChunkSize + TAIFFCommentItem(FComments.Items[i]).GetSize;
end;

class function TAIFFCommentChunk.GetClassChunkName: TChunkName;
begin
 result := 'COMT';
end;

function TAIFFCommentChunk.GetCommentCount: Byte;
begin
 result := FComments.Count;
end;

procedure TAIFFCommentChunk.LoadFromStream(Stream: TStream);
var
  CommentCount, i : Integer;
begin
 inherited;
 with Stream do
  begin
   // load number of comments
   Read(CommentCount, SizeOf(Byte));

   // clear existing comments
   FComments.Clear;

   // load every single comment
   for i := 0 to CommentCount - 1 do
    with TAIFFCommentItem(FComments.Add)
     do LoadFromStream(Stream);

   // set position to end of chunk
   Position := Position + FChunkSize - SizeOf(Byte);
  end;
end;

procedure TAIFFCommentChunk.SaveToStream(Stream: TStream);
var
  CommentCount, i : Integer;
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // store number of comments
   CommentCount := FComments.Count;
   Write(CommentCount, SizeOf(Byte));

   for i := 0 to FComments.Count - 1
    do TAIFFCommentItem(FComments.Items[i]).SaveToStream(Stream);
  end;
end;

{ TAIFFInstrumentChunk }

constructor TAIFFInstrumentChunk.Create;
begin
 inherited;
 with AIFFInstrumentRecord do
  begin
   BaseNote     := 60;
   Detune       := 0;
   LowNote      := 0;
   HighNote     := 127;
   LowVelocity  := 1;
   HighVelocity := 127;
   Gain         := 0;
  end;
end;

procedure TAIFFInstrumentChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFInstrumentChunk
  then TAIFFInstrumentChunk(Dest).AIFFInstrumentRecord := AIFFInstrumentRecord; 
end;

class function TAIFFInstrumentChunk.GetClassChunkName: TChunkName;
begin
 result := 'INST';
end;

class function TAIFFInstrumentChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TAIFFInstrumentRecord); 
end;

procedure TAIFFInstrumentChunk.SetBaseNote(const Value: Byte);
begin
 if Value > 127 then raise Exception.Create('Note must be smaller then 127');
 with AIFFInstrumentRecord do
  if Value <> BaseNote then
   begin
    BaseNote := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetDetune(Value: ShortInt);
begin
 // range check (-50..50 cents)
 if Value >  50 then Value :=  50 else
 if Value < -50 then Value := -50;

 with AIFFInstrumentRecord do
  if Value <> Detune then
   begin
    Detune := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetHighNote(const Value: Byte);
begin
 if Value > 127 then raise Exception.Create('Note must be smaller then 127');
 with AIFFInstrumentRecord do
  if Value <> HighNote then
   begin
    HighNote := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetHighVelocity(const Value: Byte);
begin
 if Value = 0   then raise Exception.Create('Velocity must be larger than 0');
 if Value > 127 then raise Exception.Create('Velocity must be smaller than 127');
 with AIFFInstrumentRecord do
  if Value <> Detune then
   begin
    HighVelocity := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetLowNote(const Value: Byte);
begin
 if Value > 127 then raise Exception.Create('Note must be smaller then 127');
 with AIFFInstrumentRecord do
  if Value <> LowNote then
   begin
    LowNote := Value;
   end;
end;

procedure TAIFFInstrumentChunk.SetLowVelocity(const Value: Byte);
begin
 if Value = 0   then raise Exception.Create('Velocity must be larger than 0');
 if Value > 127 then raise Exception.Create('Velocity must be smaller than 127');
 with AIFFInstrumentRecord do
  if Value <> Detune then
   begin
    LowVelocity := Value;
   end;
end;

{ TAIFFMIDIChunk }

procedure TAIFFMIDIChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMIDIChunk then
  begin
   SetLength(TAIFFMIDIChunk(Dest).FMIDIData, Length(FMIDIData));
   Move(FMIDIData[0], TAIFFMIDIChunk(Dest).FMIDIData[0], Length(FMIDIData));
  end;
end;

class function TAIFFMIDIChunk.GetClassChunkName: TChunkName;
begin
 result := 'MIDI';
end;

function TAIFFMIDIChunk.GetMIDIData(index: Integer): Byte;
begin
 if (index >= 0) and (index < Length(FMIDIData))
  then result := FMIDIData[index]
  else result := 0;
end;

procedure TAIFFMIDIChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   SetLength(FMIDIData, FChunkSize);
   Read(FMIDIData[0], FChunkSize);
  end;
end;

procedure TAIFFMIDIChunk.SaveToStream(Stream: TStream);
begin
 FChunkSize := Length(FMIDIData);
 inherited;
 with Stream do
  begin
   Write(FMIDIData[0], FChunkSize);
  end;
end;

procedure TAIFFMIDIChunk.SetMIDIData(index: Integer; const Value: Byte);
begin
 if (index >= 0) and (index < Length(FMIDIData))
  then FMIDIData[index] := Value;
end;

{ TAIFFAudioRecordingChunk }

constructor TAIFFAudioRecordingChunk.Create;
begin
 inherited;
 StartAddress := @AudioRecordingRecord;
 AudioRecordingRecord.AESChannelStatusData  := '';
end;

procedure TAIFFAudioRecordingChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFAudioRecordingChunk then
  begin
   TAIFFAudioRecordingChunk(Dest).AESChannelStatusData := AESChannelStatusData;
  end;
end;

function TAIFFAudioRecordingChunk.GetAESChannelStatusData: string;
begin
 SetLength(result, 24);
 Move(AudioRecordingRecord.AESChannelStatusData[0], result[1], 24);
end;

class function TAIFFAudioRecordingChunk.GetClassChunkName: TChunkName;
begin
 result := 'AESD';
end;

class function TAIFFAudioRecordingChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TAIFFAudioRecordingRecord); 
end;

procedure TAIFFAudioRecordingChunk.SetAESChannelStatusData(const Value: string);
begin
 Move(Value[1], AudioRecordingRecord.AESChannelStatusData[0], Length(Value));
end;

{ TAIFFApplicationSpecificChunk }

constructor TAIFFApplicationSpecificChunk.Create;
begin
 inherited;
 // default the signature assotiated with this project
 FApplicationSignature := 'DAVD';
end;

procedure TAIFFApplicationSpecificChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFApplicationSpecificChunk then
  begin
   TAIFFApplicationSpecificChunk(Dest).FApplicationSignature := FApplicationSignature;
   SetLength(TAIFFApplicationSpecificChunk(Dest).FApplicationData, Length(FApplicationData));
   Move(FApplicationData[1], TAIFFApplicationSpecificChunk(Dest).FApplicationData[1], Length(FApplicationData));
  end;
end;

procedure TAIFFApplicationSpecificChunk.CalculateChunkSize;
begin
 FChunkSize := Length(FApplicationData) + SizeOf(TChunkName);
end;

function TAIFFApplicationSpecificChunk.GetApplicationSignature: string;
begin
 result := FApplicationSignature;
end;

class function TAIFFApplicationSpecificChunk.GetClassChunkName: TChunkName;
begin
 result := 'APPL';
end;

function TAIFFApplicationSpecificChunk.GetData(index: Integer): Byte;
begin
 if (index >= 0) and (index < Length(FApplicationData))
  then result := Byte(FApplicationData[index + 1])
  else result := 0;
end;

procedure TAIFFApplicationSpecificChunk.SetApplicationSignature(const Value: string);
var
  ApplicationSignatureSize : Integer;
begin
 ApplicationSignatureSize := Length(Value);
 if ApplicationSignatureSize > 3 then ApplicationSignatureSize := 4;
 Move(Value[1], FApplicationSignature[0], ApplicationSignatureSize);
end;

procedure TAIFFApplicationSpecificChunk.SetData(index: Integer;
  const Value: Byte);
begin
 if (index >= 0) and (index < Length(FApplicationSignature))
  then FApplicationSignature[index + 1] := Char(Value);
end;

procedure TAIFFApplicationSpecificChunk.LoadFromStream(Stream: TStream);
begin
 inherited;
 with Stream do
  begin
   // read application signature
   Read(FApplicationSignature, SizeOf(TChunkName));

   // read application data
   SetLength(FApplicationData, FChunkSize - SizeOf(TChunkName));
   Read(FApplicationData[1], FChunkSize);
  end;
end;

procedure TAIFFApplicationSpecificChunk.SaveToStream(Stream: TStream);
begin
 CalculateChunkSize;
 inherited;
 with Stream do
  begin
   // write application signature
   Write(FApplicationSignature, SizeOf(TChunkName));

   // write application data
   Write(FApplicationData[1], FChunkSize - SizeOf(TChunkName));
  end;
end;

{ TAIFFNameChunk }

class function TAIFFNameChunk.GetClassChunkName: TChunkName;
begin
  result := 'NAME';
end;

{ TAIFFAuthorChunk }

class function TAIFFAuthorChunk.GetClassChunkName: TChunkName;
begin
 result := 'AUTH';
end;

{ TAIFFCopyrightChunk }

class function TAIFFCopyrightChunk.GetClassChunkName: TChunkName;
begin
 result := '[c] ';
end;

{ TAIFFAnnotationChunk }

class function TAIFFAnnotationChunk.GetClassChunkName: TChunkName;
begin
 result := 'ANNO';
end;

end.
