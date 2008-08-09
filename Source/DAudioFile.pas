unit DAudioFile;

interface

uses
  Classes, SysUtils, DAVDCommon;

type
  TOnLoadSaveData32 = procedure(const Buffer: PAVDSingleDynArray; const BufferSize: Cardinal) of object;
  TOnLoadSaveData64 = procedure(const Buffer: PAVDDoubleDynArray; const BufferSize: Cardinal) of object;

  TAudioEncoding = (aeUndefined = -1, aeInteger = 0, aeFloat = 1, aeMP3 = 2,
                    aeACM = 3, aeADPCM = 4, aeMSADPCM = 5, aeDVIADPCM = 6,
                    aeMuLaw = 7, aeALaw = 8, aeOther = 9);

  TMFCustomAudioFile = class(TComponent)
  private
    fOnSaveData64 : TOnLoadSaveData64;
    fOnLoadData32 : TOnLoadSaveData32;
    fOnLoadData64 : TOnLoadSaveData64;
    fOnSaveData32 : TOnLoadSaveData32;
  protected
    function GetBufferSize: Integer; virtual; abstract;
    function GetChannels: Integer; virtual; abstract;
    function GetSampleCount: Integer; virtual; abstract;
    function GetSampleRate: Double; virtual; abstract;
    function GetTotalTime: Double; virtual;
    procedure SetBufferSize(const Value: Integer); virtual; abstract;
    procedure SetChannels(const Value: Integer); virtual; abstract;
    procedure SetSampleCount(const Value: Integer); virtual; abstract;
    procedure SetSampleRate(const Value: Double); virtual; abstract;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: TFileName; HeaderOnly: Boolean = False); virtual; abstract;
    procedure SaveToFile(FileName: TFileName); virtual; abstract;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ChannelCount: Integer read GetChannels write SetChannels;
    property SampleCount: Integer read GetSampleCount write SetSampleCount;
    property BufferSize: Integer read GetBufferSize write SetBufferSize;
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
end;

destructor TMFCustomAudioFile.Destroy;
begin
 inherited;
end;

function TMFCustomAudioFile.GetTotalTime: Double;
begin
 result := SampleCount / SampleRate;
end;

end.
