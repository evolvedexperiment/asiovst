unit DAV_AudioFile;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_ChannelDataCoder;

type
  TCodingEvent = procedure(Sender: TObject;
    const Coder: TCustomChannelDataCoder; var Position: Cardinal) of object;

  {$IFDEF Delphi5}
  TAudioEncoding = (aeInteger, aeFloat, aeMP3, aeACM, aeADPCM,
    aeMSADPCM, aeDVIADPCM, aeMuLaw, aeALaw, aeOther);
  {$ELSE}
  TAudioEncoding = (aeUndefined = -1, aeInteger = 0, aeFloat = 1, aeMP3 = 2,
                    aeACM = 3, aeADPCM = 4, aeMSADPCM = 5, aeDVIADPCM = 6,
                    aeMuLaw = 7, aeALaw = 8, aeOther = 9);
  {$ENDIF}

  TCustomAudioFile = class(TComponent{$IFDEF Delphi6_Up}, IStreamPersist{$ENDIF})
  private
    FReadHeaderOnly : Boolean;
  protected
    FOnEncode     : TCodingEvent;
    FOnDecode     : TCodingEvent;
    FOnBeginRead  : TNotifyEvent;
    FOnBeginWrite : TNotifyEvent;
    function GetChannels: Cardinal; virtual; abstract;
    function GetSampleFrames: Cardinal; virtual; abstract;
    function GetSampleRate: Double; virtual; abstract;
    function GetTotalTime: Double; virtual;
    procedure SetChannels(const Value: Cardinal); virtual; abstract;
    procedure SetSampleFrames(const Value: Cardinal); virtual; abstract;
    procedure SetSampleRate(const Value: Double); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(const FileName: TFileName); virtual;
    procedure SaveToFile(const FileName: TFileName); virtual;

    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    // file format identifier
    class function DefaultExtension: string; virtual; abstract;
    class function Description: string; virtual; abstract;
    class function FileFormatFilter: string; virtual; abstract;
    class function CanLoad(const FileName: TFileName): Boolean; overload; virtual;
    class function CanLoad(const Stream: TStream): Boolean; overload; virtual; abstract;

    property ReadHeaderOnly: Boolean read FReadHeaderOnly write FReadHeaderOnly;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ChannelCount: Cardinal read GetChannels write SetChannels;
    property SampleFrames: Cardinal read GetSampleFrames write SetSampleFrames;
    property TotalTime: Double read GetTotalTime; // = SampleFrames / SampleRate

    property OnEncode: TCodingEvent read FOnEncode write FOnEncode;
    property OnDecode: TCodingEvent read FOnDecode write FOnDecode;
    property OnBeginReadAudioData: TNotifyEvent read FOnBeginRead write FOnBeginRead;
    property OnBeginWriteAudioData: TNotifyEvent read FOnBeginWrite write FOnBeginWrite;
  end;
  TAudioFileClass = class of TCustomAudioFile;

var
  AudioFileFormats: array of TAudioFileClass;

procedure RegisterFileFormat(AClass: TAudioFileClass);

implementation

procedure RegisterFileFormat(AClass: TAudioFileClass);
var
  i : Integer;
begin
 // check if file format is already registered
 for i := 0 to Length(AudioFileFormats) - 1 do
  if AudioFileFormats[i] = AClass then exit;

 // add file format to list
 SetLength(AudioFileFormats, Length(AudioFileFormats) + 1);
 AudioFileFormats[Length(AudioFileFormats) - 1] := AClass;
end;

{ TCustomAudioFile }

constructor TCustomAudioFile.Create(AOwner: TComponent);
begin
 inherited;
 // yet empty
end;

destructor TCustomAudioFile.Destroy;
begin
 // yet empty
 inherited;
end;

class function TCustomAudioFile.CanLoad(const FileName: TFileName): Boolean;
var
  FS : TFileStream;
begin
 FS := TFileStream.Create(FileName, fmOpenRead);
 try
  result := CanLoad(FS);
 finally
  FreeAndNil(FS);
 end;
end;

function TCustomAudioFile.GetTotalTime: Double;
begin
 result := SampleFrames / SampleRate;
end;

procedure TCustomAudioFile.LoadFromFile(const FileName: TFileName);
var
  FileStream : TFileStream;
begin
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   LoadFromStream(FileStream);
  finally
   FreeAndNil(FileStream);
  end;
end;

procedure TCustomAudioFile.SaveToFile(const FileName: TFileName);
var
  FileStream : TFileStream;
begin
 if FileExists(FileName)
  then FileStream := TFileStream.Create(FileName, fmOpenWrite)
  else FileStream := TFileStream.Create(FileName, fmCreate);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   FreeAndNil(FileStream);
  end;
end;

end.
