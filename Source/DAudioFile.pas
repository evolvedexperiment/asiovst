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

  TMFCustomAudioFile = class(TComponent, IStreamPersist)
  private
    fOnSaveData64    : TOnLoadSaveData64;
    fOnLoadData32    : TOnLoadSaveData32;
    fOnLoadData64    : TOnLoadSaveData64;
    fOnSaveData32    : TOnLoadSaveData32;
    fReadHeaderOnly  : Boolean;
    fRWBufferSize    : Integer;
    fRWBuffer        : PByteArray;
    procedure SetRWBufferSize(const Value: Integer);
  protected
    function GetChannels: Integer; virtual; abstract;
    function GetSampleCount: Integer; virtual; abstract;
    function GetSampleRate: Double; virtual; abstract;
    function GetTotalTime: Double; virtual;
    procedure RWBufferSizeChanged; virtual;
    procedure SetChannels(const Value: Integer); virtual; abstract;
    procedure SetSampleCount(const Value: Integer); virtual; abstract;
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
    property ChannelCount: Integer read GetChannels write SetChannels;
    property SampleCount: Integer read GetSampleCount write SetSampleCount;
    property ReadWriteBufferSize: Integer read fRWBufferSize write SetRWBufferSize default 16384;
    property TotalTime: Double read GetTotalTime; // = SampleCount / SampleRate
    property OnLoadData32: TOnLoadSaveData32 read fOnLoadData32 write fOnLoadData32;
    property OnLoadData64: TOnLoadSaveData64 read fOnLoadData64 write fOnLoadData64;
    property OnSaveData32: TOnLoadSaveData32 read fOnSaveData32 write fOnSaveData32;
    property OnSaveData64: TOnLoadSaveData64 read fOnSaveData64 write fOnSaveData64;
  end;

implementation

{ TMFCustomAudioFile }

constructor TMFCustomAudioFile.Create(AOwner: TComponent);
begin
 inherited;
 // default read/write buffer size: 16 kB
 fRWBufferSize := 16384;
 GetMem(fRWBuffer, fRWBufferSize);
end;

destructor TMFCustomAudioFile.Destroy;
begin
 Dispose(fRWBuffer);
 inherited;
end;

procedure TMFCustomAudioFile.SetRWBufferSize(const Value: Integer);
begin
 if fRWBufferSize <> Value then
  begin
   fRWBufferSize := Value;
   RWBufferSizeChanged;
  end;
end;

procedure TMFCustomAudioFile.RWBufferSizeChanged;
begin
 ReallocMem(fRWBuffer, fRWBufferSize);
end;

function TMFCustomAudioFile.GetTotalTime: Double;
begin
 result := SampleCount / SampleRate;
end;

procedure TMFCustomAudioFile.LoadFromFile(FileName: TFileName);
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

procedure TMFCustomAudioFile.SaveToFile(FileName: TFileName);
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
