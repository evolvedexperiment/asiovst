unit DAudioFile;

interface

uses
  Classes, SysUtils, DAVDCommon;

type
  TOnLoadSaveData32 = procedure(const Buffer: array of PAVDSingleDynArray; const BufferSize: Cardinal) of object;
  TOnLoadSaveData64 = procedure(const Buffer: array of PAVDDoubleDynArray; const BufferSize: Cardinal) of object;

  TAudioEncoding = (aeUndefined = -1, aeInteger = 0, aeFloat = 1, aeMP3 = 2,
                    aeACM = 3, aeADPCM = 4, aeMSADPCM = 5, aeDVIADPCM = 6,
                    aeMuLaw = 7, aeALaw = 8, aeOther = 9);

  TCustomAudioFile = class(TComponent, IStreamPersist)
  private
    fOnSaveData64    : TOnLoadSaveData64;
    fOnLoadData32    : TOnLoadSaveData32;
    fOnLoadData64    : TOnLoadSaveData64;
    fOnSaveData32    : TOnLoadSaveData32;
    fReadHeaderOnly  : Boolean;
    fRWBufferSize    : Cardinal;
    fRWBuffer        : PByteArray;
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

    property ReadHeaderOnly: Boolean read fReadHeaderOnly write fReadHeaderOnly;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ChannelCount: Cardinal read GetChannels write SetChannels;
    property SampleCount: Cardinal read GetSampleCount write SetSampleCount;
    property ReadWriteBufferSize: Cardinal read fRWBufferSize write SetRWBufferSize default 16384;
    property TotalTime: Double read GetTotalTime; // = SampleCount / SampleRate
    property OnLoadData32: TOnLoadSaveData32 read fOnLoadData32 write fOnLoadData32;
    property OnLoadData64: TOnLoadSaveData64 read fOnLoadData64 write fOnLoadData64;
    property OnSaveData32: TOnLoadSaveData32 read fOnSaveData32 write fOnSaveData32;
    property OnSaveData64: TOnLoadSaveData64 read fOnSaveData64 write fOnSaveData64;
  end;

procedure FlipWord(var Value); overload;
procedure FlipLong(var Value); overload;
procedure FlipExtended(var Value : Extended); overload;

implementation

procedure FlipLong(var Value); overload;
var
  VA   : array [0..3] of Byte absolute Value;
  temp : Byte;
begin
  temp  := VA[0];
  VA[0] := VA[3];
  VA[3] := temp;
  temp  := VA[1];
  VA[1] := VA[2];
  VA[2] := temp;
end;

procedure FlipWord(var Value);
var
  VA   : array [0..1] of Byte absolute Value;
  temp : Byte;
begin
  temp  := VA[0];
  VA[0] := VA[1];
  VA[1] := temp;
end;

procedure FlipExtended(var Value : Extended); overload;
var
  VA   : array [0..9] of Byte absolute Value;
  temp : Byte;
begin
 temp := VA[0]; VA[0] := VA[9]; VA[9] := temp;
 temp := VA[1]; VA[1] := VA[8]; VA[8] := temp;
 temp := VA[2]; VA[2] := VA[7]; VA[7] := temp;
 temp := VA[3]; VA[3] := VA[6]; VA[6] := temp;
 temp := VA[4]; VA[4] := VA[5]; VA[5] := temp;
end;

{ TCustomAudioFile }

constructor TCustomAudioFile.Create(AOwner: TComponent);
begin
 inherited;
 // default read/write buffer size: 16 kB
 fRWBufferSize := 16384;
 GetMem(fRWBuffer, fRWBufferSize);
end;

destructor TCustomAudioFile.Destroy;
begin
 Dispose(fRWBuffer);
 inherited;
end;

procedure TCustomAudioFile.SetRWBufferSize(const Value: Cardinal);
begin
 if fRWBufferSize <> Value then
  begin
   fRWBufferSize := Value;
   RWBufferSizeChanged;
  end;
end;

procedure TCustomAudioFile.RWBufferSizeChanged;
begin
 ReallocMem(fRWBuffer, fRWBufferSize);
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
