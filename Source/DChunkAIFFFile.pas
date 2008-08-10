unit DChunkAIFFFile;

interface

uses
  Classes, SysUtils, DChunkClasses;

type
  ////////////////////////////////////////////////////////////////////////////
  /////////////////////////////// Common Chunk ///////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFCommonRecord = packed record // 'COMM'
    Channels     : SmallInt;
    SampleFrames : Cardinal;
    SampleSize   : SmallInt;
    SampleRate   : Extended;
  end;

  TAIFFCommonChunk = class(TFixedDefinedChunk)
  private
    procedure SetChannels(const Value: SmallInt);
    procedure SetSampleRate(const Value: Extended);
    procedure SetSampleSize(const Value: SmallInt);
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFCommonRecord : TAIFFCommonRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property Channels: SmallInt read AIFFCommonRecord.Channels write SetChannels;
    property SampleFrames: Cardinal read AIFFCommonRecord.SampleFrames write AIFFCommonRecord.SampleFrames;
    property SampleSize: SmallInt read AIFFCommonRecord.SampleSize write SetSampleSize;
    property SampleRate: Extended read AIFFCommonRecord.SampleRate write SetSampleRate;
  end;

  ////////////////////////////////////////////////////////////////////////////
  //////////////////////////// Sound Data Chunk //////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFSoundDataRecord = packed record // 'SSND'
    StreamPos : Cardinal;
    Offset : Cardinal;
    BlockSize : Cardinal;
    // DATA...
  end;

  TAIFFSoundDataChunk = class(TFixedDefinedChunk)
  protected
    procedure AssignTo(Dest: TPersistent); override;
  public
    AIFFSoundDataRecord : TAIFFSoundDataRecord;
    constructor Create; override;
    class function GetClassChunkSize: Integer; override;
    class function GetClassChunkName : TChunkName; override;
  published
    property StreamPos: Cardinal read AIFFSoundDataRecord.StreamPos write AIFFSoundDataRecord.StreamPos;
    property Offset: Cardinal read AIFFSoundDataRecord.Offset write AIFFSoundDataRecord.Offset;
    property BlockSize: Cardinal read AIFFSoundDataRecord.BlockSize write AIFFSoundDataRecord.BlockSize;
  end;

  ////////////////////////////////////////////////////////////////////////////
  ////////////////////////////// Marker Chunk ////////////////////////////////
  ////////////////////////////////////////////////////////////////////////////

  TAIFFMarkerRecord = packed record // 'MARK'
    numMarkers : Byte;
    // DATA...
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

{ TAIFFCommonChunk }

constructor TAIFFCommonChunk.Create;
begin
 inherited;
 StartAddress := @AIFFCommonRecord;

 // set defaults
 with AIFFCommonRecord do
  begin
   Channels     := 1;       // one channel
   SampleRate   := 44100;   // 44.1 kHz (CD quality)
   SampleSize   := 16;      // 16bit
   SampleFrames := 0;       // no data yet
  end;
end;

procedure TAIFFCommonChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFCommonChunk then
  begin
   TAIFFCommonChunk(Dest).AIFFCommonRecord := AIFFCommonRecord;
  end;
end;

class function TAIFFCommonChunk.GetClassChunkName: TChunkName;
begin
 result := 'COMM';
end;

class function TAIFFCommonChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TAIFFCommonRecord);
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

{ TAIFFSoundDataChunk }

constructor TAIFFSoundDataChunk.Create;
begin
 inherited;
 StartAddress := @AIFFSoundDataRecord;
 with AIFFSoundDataRecord do
  begin
  
  end;
end;

procedure TAIFFSoundDataChunk.AssignTo(Dest: TPersistent);
begin
 inherited;
 if Dest is TAIFFSoundDataChunk then
  begin
   TAIFFSoundDataChunk(Dest).AIFFSoundDataRecord := AIFFSoundDataRecord;
  end;
end;

class function TAIFFSoundDataChunk.GetClassChunkName: TChunkName;
begin
 result := 'SSND';
end;

class function TAIFFSoundDataChunk.GetClassChunkSize: Integer;
begin
 result := SizeOf(TAIFFSoundDataRecord); 
end;

end.
