unit DChunkAIFFFile;

interface

uses
  Classes, SysUtils, DChunkClasses;

const
  AIFCVersion1 = $A2805140;

type
   TAIFFCompressionType = (ctNotAvailable, ctNone, ctACE2, ctACE8, ctMACE3,
                           ctMACE6, ctUnknown);

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Common Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommonRecord = packed record // 'COMM'
    Channels        : SmallInt;
    SampleFrames    : Cardinal;
    SampleSize      : SmallInt;
    SampleRate      : Extended;
  end;

  TAIFFCommonChunk = class(TDefinedChunk)
  private
    fCompressionType: TAIFFCompressionType;
    fCompressionName: string;
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
  published
    property Channels: SmallInt read AIFFCommonRecord.Channels write SetChannels;
    property SampleFrames: Cardinal read AIFFCommonRecord.SampleFrames write AIFFCommonRecord.SampleFrames;
    property SampleSize: SmallInt read AIFFCommonRecord.SampleSize write SetSampleSize;
    property SampleRate: Extended read AIFFCommonRecord.SampleRate write SetSampleRate;
    property Compression: TAIFFCompressionType read fCompressionType write SetCompressionType;
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

  TAIFFSoundDataRecord = packed record // 'SSND'
    Offset    : Cardinal;
    BlockSize : Cardinal;
  end;

  TAIFFSoundDataChunk = class(TDefinedChunk)
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

  TAIFFMarkerRecord = packed record // 'MARK'
    MarkerID   : Byte;
    Position   : Cardinal;
  end;

  TAIFFMarkerItem = class(TCollectionItem)
  private
    fMarkerName : string;
    procedure SetMarkerID(const Value: Byte);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    MarkerRecord : TAIFFMarkerRecord;
    function GetSize: Cardinal;
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property MarkerID: Byte read MarkerRecord.MarkerID write SetMarkerID;
    property Position: Cardinal read MarkerRecord.Position write MarkerRecord.Position;
    property MarkerName: string read fMarkerName write fMarkerName;
  end;

  TAIFFMarkerChunk = class(TDefinedChunk)
  private
    fMarkers : TOwnedCollection;
    procedure CalculateChunkSize;
    function GetMarkerCount: Byte;
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
  published
    property MarkerCount: Byte read GetMarkerCount;
  end;

  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// MIDI Chunk /////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFMIDIRecord = packed record // 'MIDI'
    // DATA...
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Instrument Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFInstrumentRecord = packed record // 'INST'
    BaseNote : char;
    Detune : char;
    LowNote : char;
    HighNote : char;
    LowVelocity : char;
    HighVelocity : char;
    Gain : Byte;
    // Loopdata...
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////// Audio Recording Chunk ////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFAudioRecordingRecord = packed record // 'AESD'
    AESChannelStatusData : Array [0..23] of char;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////// Application Specific Chunk //////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFApplicationSpecificIDRecord = packed record // 'APPL'
    ApplicationSignature : Array [0..3] of char;
    ApplicationText : Array [0..255] of char;
    // comming soon..
  end;

  ////////////////////////////////////////////////////////////////////////////
  ///////////////////////////// Comments Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommentsRecord = packed record // 'COMT'
    numComments : Byte;
    // Comments...
  end;

implementation

uses
  DAVDCommon;

{ TAIFFCommonChunk }

constructor TAIFFCommonChunk.Create;
begin
 inherited;

 // set defaults
 fChunkName       := 'COMM';
 fCompressionType := ctNotAvailable;

 with AIFFCommonRecord do
  begin
   Channels       := 1;       // one channel
   SampleRate     := 44100;   // 44.1 kHz (CD quality)
   SampleSize     := 16;      // 16bit
   SampleFrames   := 0;       // no data yet
  end;

 CalculateChunkSize; 
end;

procedure TAIFFCommonChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommonChunk then
  begin
   TAIFFCommonChunk(Dest).AIFFCommonRecord := AIFFCommonRecord;
   TAIFFCommonChunk(Dest).fCompressionType := fCompressionType;
  end;
end;

procedure TAIFFCommonChunk.CalculateChunkSize;
begin
 fChunkSize := SizeOf(AIFFCommonRecord);
 if fCompressionType <> ctNotAvailable
  then fChunkSize := fChunkSize + SizeOf(TChunkName) + Length(fCompressionName);
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
   if fChunkSize = SizeOf(TAIFFCommonRecord) then
    begin
     Compression := ctNotAvailable;
     exit;
    end;

   // read additional compression information
   Read(CompTypeID, SizeOf(TChunkName));
   FlipLong(CompTypeID);
   if CompTypeID = 'NONE' then fCompressionType := ctNone else
   if CompTypeID = 'ACE2' then fCompressionType := ctACE2 else
   if CompTypeID = 'ACE8' then fCompressionType := ctACE8 else
   if CompTypeID = 'MAC3' then fCompressionType := ctMACE3 else
   if CompTypeID = 'MAC8' then fCompressionType := ctMACE6
    else fCompressionType := ctUnknown;

   // set position to end of chunk
   Position := Position + fChunkSize - SizeOf(TAIFFCommonRecord) - SizeOf(TChunkName);
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
   if fCompressionType <> ctNotAvailable then
    begin
     case fCompressionType of
         ctNone : CompressionTypeID := 'None';
         ctACE2 : CompressionTypeID := 'ACE2';
         ctACE8 : CompressionTypeID := 'ACE8';
        ctMACE3 : CompressionTypeID := 'MAC3';
        ctMACE6 : CompressionTypeID := 'MAC6';
      ctUnknown : raise Exception.Create('Not supported');
     end;
     Write(CompressionTypeID, SizeOf(TChunkName));
     Write(fCompressionName, Length(fCompressionName));
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
 if fCompressionType <> Value then
  begin
   fCompressionType := Value;
   case fCompressionType of
    ctNotAvailable : fCompressionName := 'not available';
            ctNone : fCompressionName := 'not compressed';
            ctACE2 : fCompressionName := 'ACE 2-to-1';
            ctACE8 : fCompressionName := 'ACE 8-to-1';
           ctMACE3 : fCompressionName := 'MACE 3-to-1';
           ctMACE6 : fCompressionName := 'MACE 6-to-1';
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
   Position := Position + fChunkSize - SizeOf(TAIFFSoundDataRecord);
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
 fChunkSize := SizeOf(AIFFSoundDataRecord);
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

function TAIFFMarkerItem.GetSize: Cardinal;
begin
 result := SizeOf(TAIFFMarkerRecord) + Length(fMarkerName) + 1;
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
   SetLength(fMarkerName, StringSize);
   Read(fMarkerName[1], StringSize);
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
   StringSize := Length(fMarkerName);
   Write(StringSize, SizeOf(Byte));
   Write(fMarkerName[1], StringSize);
  end;
end;

procedure TAIFFMarkerItem.SetMarkerID(const Value: Byte);
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
 fMarkers := TOwnedCollection.Create(Self, TAIFFMarkerItem);
 CalculateChunkSize;
end;

destructor TAIFFMarkerChunk.Destroy;
begin
 FreeAndNil(fMarkers);
 inherited;
end;

function TAIFFMarkerChunk.GetMarkerCount: Byte;
begin
 result := fMarkers.Count;
end;

procedure TAIFFMarkerChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFMarkerChunk
  then TAIFFMarkerChunk(Dest).fMarkers.Assign(fMarkers);
end;

procedure TAIFFMarkerChunk.CalculateChunkSize;
var
  i : Integer;
begin
 fChunkSize := SizeOf(Byte);
 for i := 0 to fMarkers.Count - 1
  do fChunkSize := fChunkSize + TAIFFMarkerItem(fMarkers.Items[i]).GetSize;
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
   fMarkers.Clear;

   // load every single marker
   for i := 0 to MarkerCount - 1 do
    with TAIFFMarkerItem(fMarkers.Add)
     do LoadFromStream(Stream);

   // set position to end of chunk
   Position := Position + fChunkSize - SizeOf(Byte);
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
   MarkerCount := fMarkers.Count;
   Write(MarkerCount, SizeOf(Byte));

   for i := 0 to fMarkers.Count - 1
    do TAIFFMarkerItem(fMarkers.Items[i]).SaveToStream(Stream);
  end;
end;

end.
