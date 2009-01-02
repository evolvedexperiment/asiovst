unit DAV_AudioFile;

interface

{$I ..\DAV_Compiler.inc}

uses
  Classes, SysUtils, DAV_Common;

type
  TOnLoadSaveData32 = procedure(const Buffer: array of PDAVSingleDynArray; const BufferSize: Cardinal) of object;
  TOnLoadSaveData64 = procedure(const Buffer: array of PDAVDoubleDynArray; const BufferSize: Cardinal) of object;

  {$IFDEF Delphi5}
  TAudioEncoding = (aeUndefined, aeInteger, aeFloat, aeMP3, aeACM, aeADPCM,
    aeMSADPCM, aeDVIADPCM, aeMuLaw, aeALaw, aeOther);
  {$ELSE}
  TAudioEncoding = (aeUndefined = -1, aeInteger = 0, aeFloat = 1, aeMP3 = 2,
                    aeACM = 3, aeADPCM = 4, aeMSADPCM = 5, aeDVIADPCM = 6,
                    aeMuLaw = 7, aeALaw = 8, aeOther = 9);
  {$ENDIF}

  TCustomAudioFile = class(TComponent{$IFDEF Delphi6_Up}, IStreamPersist{$ENDIF})
  private
    FOnSaveData64    : TOnLoadSaveData64;
    FOnLoadData32    : TOnLoadSaveData32;
    FOnLoadData64    : TOnLoadSaveData64;
    FOnSaveData32    : TOnLoadSaveData32;
    FReadHeaderOnly  : Boolean;
    FRWBufferSize    : Cardinal;
    FRWBuffer        : PByteArray;
    procedure SetRWBufferSize(const Value: Cardinal);
  protected
    function GetChannels: Cardinal; virtual; abstract;
    function GetSampleCount: Cardinal; virtual; abstract;
    function GetSampleRate: Double; virtual; abstract;
    function GetTotalTime: Double; virtual;
    procedure RWBufferSizeChanged; virtual;
    procedure SetChannels(const Value: Cardinal); virtual; abstract;
    procedure SetSampleCount(const Value: Cardinal); virtual; abstract;
    procedure SetSampleRate(const Value: Double); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    procedure LoadFromFile(FileName: TFileName); virtual;
    procedure SaveToFile(FileName: TFileName); virtual;

    procedure LoadFromStream(Stream: TStream); virtual; abstract;
    procedure SaveToStream(Stream: TStream); virtual; abstract;

    property ReadHeaderOnly: Boolean read FReadHeaderOnly write FReadHeaderOnly;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ChannelCount: Cardinal read GetChannels write SetChannels;
    property SampleCount: Cardinal read GetSampleCount write SetSampleCount;
    property ReadWriteBufferSize: Cardinal read FRWBufferSize write SetRWBufferSize default 16384;
    property TotalTime: Double read GetTotalTime; // = SampleCount / SampleRate
    property OnLoadData32: TOnLoadSaveData32 read FOnLoadData32 write FOnLoadData32;
    property OnLoadData64: TOnLoadSaveData64 read FOnLoadData64 write FOnLoadData64;
    property OnSaveData32: TOnLoadSaveData32 read FOnSaveData32 write FOnSaveData32;
    property OnSaveData64: TOnLoadSaveData64 read FOnSaveData64 write FOnSaveData64;
  end;

implementation

{ TCustomAudioFile }

constructor TCustomAudioFile.Create(AOwner: TComponent);
begin
 inherited;
 // default read/write buffer size: 16 kB
 FRWBufferSize := 16384;
 GetMem(FRWBuffer, FRWBufferSize);
end;

destructor TCustomAudioFile.Destroy;
begin
 Dispose(FRWBuffer);
 inherited;
end;

procedure TCustomAudioFile.SetRWBufferSize(const Value: Cardinal);
begin
 if FRWBufferSize <> Value then
  begin
   FRWBufferSize := Value;
   RWBufferSizeChanged;
  end;
end;

procedure TCustomAudioFile.RWBufferSizeChanged;
begin
 ReallocMem(FRWBuffer, FRWBufferSize);
end;

function TCustomAudioFile.GetTotalTime: Double;
begin
 result := SampleCount / SampleRate;
end;

procedure TCustomAudioFile.LoadFromFile(FileName: TFileName);
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

procedure TCustomAudioFile.SaveToFile(FileName: TFileName);
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
