unit DAV_AudioFile;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common, DAV_ChannelDataCoder;

type
  TChannelData32CodingEvent = procedure(Self: TObject; const Coder: TCustomChannel32DataCoder) of object;

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
    FOnEncode        : TChannelData32CodingEvent;
    FOnDecode        : TChannelData32CodingEvent;

    FReadHeaderOnly  : Boolean;
  protected
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

    property ReadHeaderOnly: Boolean read FReadHeaderOnly write FReadHeaderOnly;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ChannelCount: Cardinal read GetChannels write SetChannels;
    property SampleFrames: Cardinal read GetSampleFrames write SetSampleFrames;
    property TotalTime: Double read GetTotalTime; // = SampleFrames / SampleRate

    property OnEncode: TChannelData32CodingEvent read FOnEncode write FOnEncode;
    property OnDecode: TChannelData32CodingEvent read FOnDecode write FOnDecode;
  end;

implementation

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
 FileStream := TFileStream.Create(FileName, fmOpenRead);
 with FileStream do
  try
   SaveToStream(FileStream);
  finally
   FreeAndNil(FileStream);
  end;
end;

end.
