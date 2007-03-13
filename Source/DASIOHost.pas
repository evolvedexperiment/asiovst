unit DASIOHost;
// This unit allows you to open an ASIO audio driver and access
// its inputs and outputs. The component was written by
// Christian Budde and Tobias Fleischer, with an extension by
// Benjamin Rosseaux. Please give credit if you use this component in your
// programs. Thanks to Martin Fay (original Delphi ASIO interface)

{$I JEDI.INC}
{$R DASIOHost.res}
{.$DEFINE OpenASIO}
// define OpenASIO to compile using old OpenASIO interface (needs OpenASIO.dll)

{.$DEFINE ASIOMixer}
// define ASIOMixer to compile with the ASIO mixer
{-$DEFINE D5CP}
// define D5CP to compile with the ASIO control panel design time for Delphi 5

interface

uses {$IFDEF FPC} LCLIntf, LclType, LMessages, {$ELSE} Windows, Messages, {$ENDIF}
     SysUtils, Classes, ASIO, Types, DASIOConvert, DASIOGenerator,
     {$IFDEF OpenASIO} OpenAsio {$ELSE} BeroASIO {$ENDIF},
     {$IFDEF ASIOMixer} Forms, ComCtrls, Graphics, StdCtrls, Controls,
     ASIOMixer, {$ENDIF} {$IFDEF D5CP} dsgnintf, {$ENDIF} DDSPBase;

const
  // private message
  PM_ASIO = WM_User + 1652;   // unique we hope
  // ASIO message(s), as wParam for PM_ASIO
  AM_ResetRequest         = 0;
  AM_BufferSwitch         = 1;     // new buffer index in lParam
  AM_BufferSwitchTimeInfo = 2;     // new buffer index in lParam
                                   // time passed in MainForm.BufferTime
  AM_LatencyChanged       = 3;
  PM_UpdateSamplePos      = PM_ASIO + 1;  // sample pos in wParam (hi) and lParam (lo)

  PM_BufferSwitch         = PM_ASIO + 2;
  PM_BufferSwitchTimeInfo = PM_ASIO + 3;
  PM_Reset                = PM_ASIO + 4;

type
  TAsioDriverDesc = packed record
    id   : TGUID; //TCLSID;
    name : array[0..511] of char;
    path : array[0..511] of char;
  end;
  PAsioDriverDesc = ^TAsioDriverDesc;
  TASIOBufferList = array [0..0] of TASIOBufferInfo;
  PASIOBufferList = ^TASIOBufferList;
  TDoubleDynArray = Types.TDoubleDynArray;
  TSingleDynArray = Types.TSingleDynArray;
  TArrayOfDoubleDynArray = DDSPBase.TArrayOfDoubleDynArray;
  TArrayOfSingleDynArray = DDSPBase.TArrayOfSingleDynArray;
  PArrayOfDoubleDynArray = DDSPBase.PArrayOfDoubleDynArray;
  PArrayOfSingleDynArray = DDSPBase.PArrayOfSingleDynArray;

  TAsioDriverList = array of TAsioDriverDesc;
  TASIOCanDo = (acdInputMonitor, acdTimeInfo, acdTimeCode, acdTransport,
                acdInputGain, acdInputMeter, acdOutputGain, acdOutputMeter);
  TASIOCanDos = set of TASIOCanDo;
  TASIOOutputDither = (odNone, odUDF, odTDF);

  TConvertMethod = (cmNone, cm32, cm64);
  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TSamplePositionUpdateEvent = procedure(Sender: TObject; SamplePosition: Int64) of object;
  TSample2Event = procedure(Sender: TObject; Sample: array of Single) of object;
  TBufferSwitchEvent32 = procedure(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray) of object;
  TBufferSwitchEvent64 = procedure(Sender: TObject; const InBuffer, OutBuffer: TArrayOfDoubleDynArray) of object;
  TBufferSwitchEventNative = procedure(Sender: TObject; const BufferInfo: PASIOBufferList; const BufferIndex : Integer) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfCustom);

  TPreventClipping = (pcNone, pcDigital, pcAnalog);

  TInputMonitor = (imDisabled, imMono, imStereo, imAll);

  TATFlag = (atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
             atSpeedValid, atSampleRateChanged, atClockSourceChanged);
  TATFlags = set of TATFlag;

  TASIOTimeSub = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    function GetATInt64(Index: Integer): Int64;
    function GetATdouble(Index: Integer): Double;
    function GetATflags: TATFlags;
    procedure SetATInt64(Index: Integer; Value: Int64);
    procedure SetATdouble(Index: Integer; Value: Double);
    procedure SetATflags(Flags: TATFlags);
  protected
    FBufferTime: TASIOTime;
    procedure Change; dynamic;
    procedure AssignTo(Dest: TPersistent); override;
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;
  published
    property SamplePos: Int64 index 0 read GetATInt64 write SetATInt64;
    property Speed : Double index 0 read  GetATdouble write SetATdouble; //absolute speed (1. = nominal)
    property SampleRate: Double Index 1 read GetATdouble write SetATdouble;
    property Flags : TATFlags read GetATflags Write SetATflags;
  end;

{$IFDEF D5CP}
  TASIOControlPanel = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
{$ENDIF}

  TCustomASIOHostBasic = class(TComponent)
  private
    Fmin, Fmax,
    Fpref, Fgran          : Integer;
    FBuffersCreated       : Boolean;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
  protected
    FHandle               : THandle;
    FASIOTime             : TASIOTimeSub;
    FOnCreate             : TNotifyEvent;
    FOnDestroy            : TNotifyEvent;
    FOnReset              : TNotifyEvent;
    FOnDriverChanged      : TNotifyEvent;
    FOnLatencyChanged     : TNotifyEvent;
    FOnSampleRateChanged  : TNotifyEvent;
    FOnBuffersCreate      : TNotifyEvent;
    FOnBuffersDestroy     : TNotifyEvent;
    FOnUpdateSamplePos    : TSamplePositionUpdateEvent;
    FOnBufferSwitch       : TBufferSwitchEventNative;
    FASIOCanDos           : TASIOCanDos;
    FASIOdriverlist       : TASIODriverList;
    FCallbacks            : TASIOCallbacks;
    FUnAlignedBuffer      : PASIOBufferInfo;
    FSampleRate           : Double;
    FInputBuffer          : PASIOBufferInfo;
    FOutputBuffer         : PASIOBufferInfo;
    FActive               : Boolean;
    FDriverIndex          : Integer;
    FDriverList           : TStrings;
    FDriverName           : String;
    FDriverVersion        : integer;
    FInputLatency         : Integer;
    FOutputLatency        : Integer;
    FInputChannelCount    : Integer;
    FOutputChannelCount   : Integer;
    FBufferSize           : Cardinal;
    FInConvertors         : array of TInConvertor;
    FOutConvertors        : array of TOutConvertor;
    {$IFDEF OpenASIO}
    FDriver               : IOpenAsio;
    {$ELSE}
    FDriver               : IBeroASIO;
    {$ENDIF}
    procedure SetActive(Value: Boolean); virtual;
    procedure SetDriverIndex(Value: Integer); virtual;
    procedure SetDriverName(const s: String); virtual;
    {$IFDEF FPC}
    procedure WndProc(var Msg: TLMessage);
    procedure PMASIO(var Message: TLMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TLMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TLMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TLMessage); message PM_BufferSwitchTimeInfo;
    {$ELSE}
    procedure WndProc(var Msg: TMessage);
    procedure PMASIO(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage); message PM_BufferSwitchTimeInfo;
    {$ENDIF}
    function GetInputMeter(Channel:Integer): Integer; virtual;
    function GetOutputMeter(Channel:Integer): Integer; virtual;
    function CanInputGain: Boolean; virtual;
    function CanInputMeter: Boolean; virtual;
    function CanOutputGain: Boolean; virtual;
    function CanOutputMeter: Boolean; virtual;
    procedure SetASIOCanDos(const Value: TASIOCanDos); virtual;
    function CanTimeCode: Boolean; virtual;
    function CanTimeInfo: Boolean; virtual;
    function CanTransport: Boolean; virtual;
    function CreateBuffers: Boolean; virtual;
    procedure DestroyBuffers; virtual;
    procedure SetSampleRate(const Value: Double); virtual;
    procedure BufferSwitch(index: integer); virtual;
    procedure BufferSwitchTimeInfo(index: integer; const params: TASIOTime); virtual;
    function GetDriverList: TStrings;
    procedure ReadState(Reader: TReader); override;
  public
    InputChannelInfos   : array of TASIOChannelInfo;
    OutputChannelInfos  : array of TASIOChannelInfo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlPanel; virtual;
    procedure Reset; virtual;
    function GetNumDrivers: integer; virtual;
    procedure OpenDriver; virtual;
    procedure CloseDriver; virtual;
    function CanSampleRate(sampleRate: TASIOSampleRate): TASIOError; virtual;
    procedure GetOutputGain(Channel, Gain: Integer); virtual;
    procedure SetInputGain(Channel, Gain: Integer); virtual;

    property InputMeter[Channel:Integer]: Integer read GetInputMeter;
    property OutputMeter[Channel:Integer]: Integer read GetOutputMeter;
    property Active: Boolean read FActive write SetActive default false;
    property CanDos : TASIOCanDos read fASIOCanDos write SetASIOCanDos;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: integer read FDriverVersion;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex default -1;
    property BufferSize: Cardinal read fBufferSize stored false default 1;
    property BufferMinimum: Integer read Fmin stored false;
    property BufferMaximum: Integer read Fmax stored false;
    property BufferPreferredSize: Integer read Fpref stored false;
    property BufferGranularity: Integer read Fgran stored false;
    property InputLatency: Integer read FInputLatency stored false default 0;
    property InputChannelCount: Integer read FInputChannelCount stored false default 0;
    property OutputLatency: Integer read FOutputLatency stored false default 0;
    property OutputChannelCount: Integer read FOutputChannelCount stored false default 0;
    property SampleRate: Double read fSampleRate write SetSampleRate;
    property ASIOTime: TASIOTimeSub read FASIOTime Write FASIOTime;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnBufferSwitch: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property OnBuffersCreate: TNotifyEvent read FOnBuffersCreate write FOnBuffersCreate;
    property OnBuffersDestroy: TNotifyEvent read FOnBuffersDestroy write FOnBuffersDestroy;
    property DriverList: TStrings read FDriverList;
  end;

  TASIOHostBasic = class(TCustomASIOHostBasic)
  published
    property Active;
    property CanDos;
    property DriverName;
    property DriverVersion;
    property DriverIndex;
    property BufferSize;
    property BufferMinimum;
    property BufferMaximum;
    property BufferPreferredSize;
    property BufferGranularity;
    property InputLatency;
    property InputChannelCount;
    property OutputLatency;
    property OutputChannelCount;
    property SampleRate;
    property ASIOTime;
    property OnCreate;
    property OnDestroy;
    property OnUpdateSamplePos;
    property OnReset;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnSampleRateChanged;
    property OnBufferSwitch;
    property OnBuffersCreate;
    property OnBuffersDestroy;
    property DriverList;
  end;

  TCustomASIOHost = class(TCustomASIOHostBasic)
  private
    FPreventClipping      : TPreventClipping;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;
    FOnSample2Output      : TSample2Event;
    FOnInput2Sample       : TSample2Event;
    FOnBufferSwitch32     : TBufferSwitchEvent32;
    FOnBufferSwitch64     : TBufferSwitchEvent64;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
    FInputChannelOffset   : Word;
    FOutputChannelOffset  : Word;
    FASIOGenerator        : TASIOGenerator;
    FSingleInBuffer       : TArrayOfSingleDynArray;
    FSingleOutBuffer      : TArrayOfSingleDynArray;
    FDoubleInBuffer       : TArrayOfDoubleDynArray;
    FDoubleOutBuffer      : TArrayOfDoubleDynArray;
    FUnAlignedBuffer      : PASIOBufferInfo;
    FInputMonitor         : TInputMonitor;
    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    FOutputDither         : TASIOOutputDither;
    {$IFDEF ASIOMixer}
    FASIOMixer            : TFmASIOMixer;
    {$ENDIF}
    procedure SetInputChannelOffset(const w: Word);
    procedure SetOutputChannelOffset(const w: Word);
    procedure SetConvertOptimizations(const co: TConvertOptimizations);
    procedure SetASIOGenerator(const v: TASIOGenerator);
    procedure SetPreventClipping(v: TPreventClipping);
    {$IFDEF ASIOMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
    {$ENDIF}
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
    procedure SetOutputDither(const Value: TASIOOutputDither);
  protected
    FInputLatency         : Integer;
    FOutputLatency        : Integer;
    FInConvertors         : array of TInConvertor;
    FOutConvertors        : array of TOutConvertor;
    function CreateBuffers: Boolean; override;
    procedure DestroyBuffers; override;
    procedure BufferSwitchTimeInfo(index: integer; const params: TASIOTime); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF ASIOMixer}
    procedure Mixer;
    {$ENDIF}
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping default pcNone;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property CustomGenerator: TASIOGenerator read FASIOGenerator Write SetASIOGenerator;
    property InputChannelOffset : Word read FInputChannelOffset write SetInputChannelOffset default 0;
    property OutputDither: TASIOOutputDither read FOutputDither write SetOutputDither default odNone;
    property OutputChannelOffset: Word read FOutputChannelOffset write SetOutputChannelOffset default 0;
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;
    property OnInput2Sample: TSample2Event read FOnInput2Sample write FOnInput2Sample;
    property OnSample2Output: TSample2Event read FOnSample2Output write FOnSample2Output;
    property OnBufferSwitch32: TBufferSwitchEvent32 read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchEvent64 read FOnBufferSwitch64 write SetOnBufferSwitch64;
    property OnBufferSwitchNative: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property InputMonitor: TInputMonitor read FInputMonitor write FInputMonitor default imDisabled;
  end;

  TASIOHost = class(TCustomASIOHost)
  published
    property Active;
    property CanDos;
    property PreventClipping;
    property PreFillInBuffer;
    property PreFillOutBuffer;
    property DriverName;
    property DriverVersion;
    property DriverIndex;
    property BufferSize;
    property BufferMinimum;
    property BufferMaximum;
    property BufferPreferredSize;
    property BufferGranularity;
    property CustomGenerator;
    property InputLatency;
    property InputChannelCount;
    property InputChannelOffset;
    property OutputLatency;
    property OutputDither;
    property OutputChannelCount;
    property OutputChannelOffset;
    property ConvertOptimizations;
    property SampleRate;
    property ASIOTime;
    property OnCreate;
    property OnDestroy;
    property OnUpdateSamplePos;
    property OnReset;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnInput2Sample;
    property OnSample2Output;
    property OnSampleRateChanged;
    property OnBufferSwitch32;
    property OnBufferSwitch64;
    property OnBufferSwitchNative;
    property OnBuffersCreate;
    property OnBuffersDestroy;
    property InputMonitor;
    property DriverList;
  end;

var theHost             : TCustomASIOHostBasic;
    {$IFDEF FPC}
    PMUpdSamplePos      : TLMessage;
    PMBufSwitch         : TLMessage;
    PMBufSwitchTimeInfo : TLMessage;
    PMReset             : TLMessage;
    {$ELSE}
    PMUpdSamplePos      : TMessage;
    PMBufSwitch         : TMessage;
    PMBufSwitchTimeInfo : TMessage;
    PMReset             : TMessage;
    {$ENDIF}

procedure Register;
function ChannelTypeToString(vType: TASIOSampleType): string;
procedure ListAsioDrivers(var List: TAsioDriverList);

implementation

uses Registry, ComObj, Math {$IFDEF ASIOMixer}, ASIOChannelStrip {$ENDIF};

const ASIODRV_DESC  = 'description';
      INPROC_SERVER = 'InprocServer32';
      ASIO_PATH     = 'software\asio';
      COM_CLSID     = 'clsid';

function findDrvPath(const clsidstr: string; var dllpath: string): longint;
var
   reg     : TRegistry;
   success : boolean;
   buf     : array[0..1024] of char;
   s       : string;
   temps   : string;
begin
  Result := -1;

  //CharLowerBuff(clsidstr,strlen(clsidstr));
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    success := reg.OpenKeyReadOnly(COM_CLSID + '\' + clsidstr + '\' + INPROC_SERVER);
    if success then
    begin
      dllpath := reg.ReadString('');
      if (ExtractFilePath(dllpath) = '') and (dllpath <> '') then
      begin
        {$IFNDEF FPC}
        buf[0] := #0;
        temps := dllpath;   // backup the value
        if GetSystemDirectory(buf, 1023) <> 0 then   // try the system directory first
        begin
          s := buf;
          dllpath := s + '\' + temps;
        end;

        if not FileExists(dllpath) then              // try the windows dir if necessary
        begin
          buf[0] := #0;
          if GetWindowsDirectory(buf, 1023) <> 0 then   // try the system directory first
          begin
            s := buf;
            dllpath := s + '\' + temps;
          end;
        end;
       {$ENDIF}
      end;

      if FileExists(dllpath) then
        Result := 0;
      reg.CloseKey;
    end;
  finally
    reg.Free;
  end;
end;

procedure ListAsioDrivers(var List: TAsioDriverList);
var
   r       : TRegistry;
   keys    : TStringList;
   success : boolean;
   i       : integer;
   id      : string;
   dllpath : string;
   count   : integer;
begin
  SetLength(List, 0);

  keys := TStringList.Create;
  r := TRegistry.Create;
  try
    r.RootKey := HKEY_LOCAL_MACHINE;
    success := r.OpenKeyReadOnly(ASIO_PATH);
    if success then
    begin
     r.GetKeyNames(keys);
     r.CloseKey;
    end;
    count := 0;
    for i := 0 to keys.Count - 1 do
    begin
      success := r.OpenKeyReadOnly(ASIO_PATH + '\' + keys[i]);
      if success then
      begin
        id := r.ReadString(COM_CLSID);
        if findDrvPath(id, dllpath) = 0 then  // check if the dll exists
        begin
          SetLength(List, count+1);
          List[count].id := StringToGUID(id);
          StrPLCopy(List[count].name, keys[i], 512);
          StrPLCopy(List[count].path, dllpath, 512);
          inc(count);
        end;
        r.CloseKey;
      end;
    end;
  finally
    keys.Free;
    r.Free;
  end;
end;

{$IFDEF DELPHI5}
{$IFDEF D5CP}
procedure TASIOControlPanel.Edit;
begin
 ExecuteVerb(0);
end;

function TASIOControlPanel.GetVerb(Index: Integer): string;
begin
 case Index of
 0: Result := 'Control Panel';
 end;
end;

function TASIOControlPanel.GetVerbCount: Integer;
begin
 if (Component as TCustomASIOHost).DriverIndex >= 0 then
  Result := 1 else Result := 0;
end;

procedure TASIOControlPanel.ExecuteVerb(Index: Integer);
begin
 case Index of
 0: if (Component as TCustomASIOHost).DriverIndex >= 0 then
  (Component as TCustomASIOHost).ControlPanel;
 end;
end;
{$ENDIF}
{$ENDIF}

constructor TASIOTimeSub.Create;
begin
 FBufferTime.timeInfo.speed := 1;
 FBufferTime.timeInfo.sampleRate := 44100;
 FBufferTime.timeInfo.samplePosition := Int64ToASIOSamples(0);
 Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid];
end;

procedure TASIOTimeSub.Change;
begin
 if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TASIOTimeSub.AssignTo(Dest: TPersistent);
begin
 if Dest is TASIOTimeSub then
 with TASIOTimeSub(Dest) do
 begin
  FBufferTime := Self.FBufferTime;
  Change;
 end else inherited AssignTo(Dest);
end;

function TASIOTimeSub.GetATflags: TATFlags;
begin
 result := [];
 if (FBufferTime.timeInfo.flags and kSystemTimeValid) <> 0 then
  result := result + [atSystemTimeValid] else
  result := result - [atSystemTimeValid];
 if (FBufferTime.timeInfo.flags and kSamplePositionValid) <> 0 then
  result := result + [atSamplePositionValid] else
  result := result - [atSamplePositionValid];
 if (FBufferTime.timeInfo.flags and kSampleRateValid) <> 0 then
  result := result + [atSampleRateValid] else
  result := result - [atSampleRateValid];
 if (FBufferTime.timeInfo.flags and kSpeedValid) <> 0 then
  result := result + [atSpeedValid] else
  result := result - [atSpeedValid];
 if (FBufferTime.timeInfo.flags and kSampleRateChanged) <> 0 then
  result := result + [atSampleRateChanged] else
  result := result - [atSampleRateChanged];
 if (FBufferTime.timeInfo.flags and kClockSourceChanged) <> 0 then
  result := result + [atClockSourceChanged] else
  result := result - [atClockSourceChanged];
end;

procedure TASIOTimeSub.SetATflags(Flags: TATFlags);
var temp: Integer;
begin
 temp := 0;
 if (atSystemTimeValid in Flags) then temp := temp + kSystemTimeValid;
 if (atSamplePositionValid in Flags) then temp := temp + kSamplePositionValid;
 if (atSampleRateValid in Flags) then temp := temp + kSampleRateValid;
 if (atSpeedValid in Flags) then temp := temp + kSpeedValid;
 if (atSampleRateChanged in Flags) then temp := temp + kSampleRateChanged;
 if (atClockSourceChanged in Flags) then temp := temp + kClockSourceChanged;
 FBufferTime.timeInfo.flags := temp;
end;

function TASIOTimeSub.GetATdouble(Index :Integer): Double;
begin
 Result := 0;
 case Index of
  0: Result := FBufferTime.timeInfo.speed;
  1: Result := FBufferTime.timeInfo.sampleRate;
 end;
end;

procedure TASIOTimeSub.SetATdouble(Index :Integer; Value: Double);
begin
 case Index of
  0: if Value <> FBufferTime.timeInfo.speed then
  begin
   FBufferTime.timeInfo.speed:=Value;
   Change;
  end;
  1: if Value <> FBufferTime.timeInfo.sampleRate then
  begin
   FBufferTime.timeInfo.sampleRate:=Value;
   Change;
  end;
 end;
end;

function TASIOTimeSub.GetATInt64(Index :Integer): Int64;
begin
 Result := 0;
 case Index of
  0: Result := ASIOSamplesToInt64(FBufferTime.timeInfo.samplePosition);
 end;
end;

procedure TASIOTimeSub.SetATInt64(Index :Integer; Value: Int64);
begin
 case Index of
  0: if Value <> ASIOSamplesToInt64(FBufferTime.timeInfo.samplePosition) then
       begin
        FBufferTime.timeInfo.SamplePosition:=Int64ToASIOSamples(Value);
        Change;
       end;
 end;
end;

function ChannelTypeToString(vType: TASIOSampleType): string;
begin
 Result := '';
 case vType of
  ASIOSTInt16MSB   :  Result := 'Int16MSB';
  ASIOSTInt24MSB   :  Result := 'Int24MSB';
  ASIOSTInt32MSB   :  Result := 'Int32MSB';
  ASIOSTFloat32MSB :  Result := 'Float32MSB';
  ASIOSTFloat64MSB :  Result := 'Float64MSB';

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can be more easily used with these
  ASIOSTInt32MSB16 :  Result := 'Int32MSB16';
  ASIOSTInt32MSB18 :  Result := 'Int32MSB18';
  ASIOSTInt32MSB20 :  Result := 'Int32MSB20';
  ASIOSTInt32MSB24 :  Result := 'Int32MSB24';

  ASIOSTInt16LSB   :  Result := 'Int16LSB';
  ASIOSTInt24LSB   :  Result := 'Int24LSB';
  ASIOSTInt32LSB   :  Result := 'Int32LSB';
  ASIOSTFloat32LSB :  Result := 'Float32LSB';
  ASIOSTFloat64LSB :  Result := 'Float64LSB';

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can more easily used with these
  ASIOSTInt32LSB16 :  Result := 'Int32LSB16';
  ASIOSTInt32LSB18 :  Result := 'Int32LSB18';
  ASIOSTInt32LSB20 :  Result := 'Int32LSB20';
  ASIOSTInt32LSB24 :  Result := 'Int32LSB24';
 end;
end;

procedure ASIOBufferSwitch(doubleBufferIndex: longint;
 directProcess: TASIOBool); cdecl;
begin
  directProcess := ASIOFalse;
  case directProcess of
    ASIOFalse:
    begin
     PMBufSwitch.WParam := AM_BufferSwitch;
     PMBufSwitch.LParam := doublebufferindex;
     theHost.Dispatch(PMBufSwitch);
    end;
    ASIOTrue:  theHost.BufferSwitch(doubleBufferIndex);
  end;
end;

function ASIOBufferSwitchTimeInfo(var params: TASIOTime;
 doubleBufferIndex: longint; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  directProcess := ASIOFalse; 
  case directProcess of
    ASIOFalse:
    begin
     theHost.ASIOTime.FBufferTime := params;
     PMBufSwitchTimeInfo.WParam := AM_BufferSwitchTimeInfo;
     PMBufSwitchTimeInfo.LParam := doublebufferindex;
     theHost.Dispatch(PMBufSwitchTimeInfo);
      end;
    ASIOTrue:  theHost.BufferSwitchTimeInfo(doubleBufferIndex, params);
  end;
  Result := nil;
end;

procedure ASIOSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
begin
 if Assigned(theHost.FOnSampleRateChanged) then
  theHost.FOnSampleRateChanged(theHost);
end;

function ASIOMessage(selector, value: longint;
 message: pointer; opt: pdouble): longint; cdecl;
begin
 Result := 0;
 case selector of
  kASIOSelectorSupported    :   // return 1 if a selector is supported
   begin
    case value of
     kASIOEngineVersion        :  Result := 1;
     kASIOResetRequest         :  Result := 1;
     kASIOBufferSizeChange     :  Result := 0;
     kASIOResyncRequest        :  Result := 1;
     kASIOLatenciesChanged     :  Result := 1;
     kASIOSupportsTimeInfo     :  Result := 1;
     kASIOSupportsTimeCode     :  Result := 1;
     kASIOSupportsInputMonitor :  Result := 0;
    end;
   end;
  kASIOEngineVersion        :  Result := 2;   // ASIO 2 is supported
  kASIOResetRequest         :
   begin
    PMReset.Msg := PM_ASIO;
    PMReset.WParam := AM_ResetRequest;
    PMReset.LParam := 0;
    theHost.Dispatch(PMReset);
    Result := 1;
   end;
  kASIOBufferSizeChange     :
   begin
    PMReset.Msg := PM_ASIO;
    PMReset.WParam := AM_ResetRequest;
    PMReset.LParam := 0;
    theHost.Dispatch(PMReset);
    Result := 1;
   end;
  kASIOResyncRequest        :  ;
  kASIOLatenciesChanged     :
   begin
    PMReset.Msg := PM_ASIO;
    PMReset.WParam := AM_LatencyChanged;
    PMReset.LParam := 0;
    theHost.Dispatch(PMReset);
    Result := 1;
   end;
  kASIOSupportsTimeInfo     :  Result := 1;
  kASIOSupportsTimeCode     :  Result := 0;
  kASIOSupportsInputMonitor :  Result := 0;
 end;
end;

{ TCustomASIOHostBasic }

constructor TCustomASIOHostBasic.Create(AOwner: TComponent);
begin
//  if AOwner is TForm then Handy := TForm(AOwner).Handle else Handy := Application.Handle;
  {$IFNDEF FPC}
  {$ENDIF}
  fHandle:=AllocateHWnd(WndProc);
  //if theHost<>nil then
  theHost := Self;
  FUnAlignedBuffer:=nil;
  FInputBuffer := nil;
  FOutputBuffer := nil;
  FASIOTime := TASIOTimeSub.Create;
  {$IFNDEF FPC}
  {$ENDIF}
  FDriverList := GetDriverList;

  // set the callbacks record fields
  FCallbacks.bufferSwitch := ASIOBufferSwitch;
  FCallbacks.sampleRateDidChange := ASIOSampleRateDidChange;
  FCallbacks.ASIOMessage := ASIOMessage;
  FCallbacks.bufferSwitchTimeInfo := ASIOBufferSwitchTimeInfo;

  // set the driver itself to nil for now
  FDriver := nil;
  FBuffersCreated := FALSE;

  // and make sure all controls are enabled or disabled
  FDriverIndex := -1;
  inherited;
end;

destructor TCustomASIOHostBasic.Destroy;
begin
 try
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  FCallbacks.bufferSwitchTimeInfo := nil;
  if Active then Active := False;
  CloseDriver;
  DeallocateHWnd(fHandle);
  SetLength(FASIOdriverlist, 0);
  SetLength(FASIOdriverlist, 0);
  FDriverList.Free;
  FreeAndNil(FASIOTime);
 finally
  inherited;
 end;
end;

{$IFNDEF FPC}
procedure TCustomASIOHostBasic.WndProc(var Msg: TMessage);
begin
 with Msg do Result := DefWindowProc(fHandle, Msg, wParam, lParam);
end;
{$ELSE}
function DefWindowProc(hWnd:THandle; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LRESULT; external 'user32' name 'DefWindowProcA';

procedure TCustomASIOHostBasic.WndProc(var Msg: TLMessage);
begin
 with Msg do Result := DefWindowProc(fHandle, Msg, wParam, lParam);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

function TCustomASIOHostBasic.GetDriverList: TStrings;
var i : Integer;
begin
 Result := TStringList.Create;
 SetLength(FASIOdriverlist, 0);
 ListASIODrivers(FASIOdriverlist);
 for i := Low(FASIOdriverlist) to High(FASIOdriverlist) do
  Result.Add(FASIOdriverlist[i].name);
end;

procedure TCustomASIOHostBasic.SetDriverName(const s:String);
begin
 if FDriverList.IndexOf(s) > -1
  then DriverIndex := FDriverList.IndexOf(s);
end;

procedure TCustomASIOHostBasic.SetDriverIndex(Value: Integer);
var DrName: array[0..255] of Char;
    tmpActive : Boolean;
begin
 if (Value <> FDriverIndex) then
  begin
   tmpActive := Active;
   Active := False;
   if Value < -1 then FDriverIndex := -1 else
    if Value >= FDriverList.Count then FDriverIndex := FDriverList.Count - 1
     else FDriverIndex := Value;
   if FDriverIndex = -1 then
   begin
    FDriverName := '';
    FInputLatency := 0;
    FOutputLatency := 0;
    FInputChannelCount := 0;
    FOutputChannelCount := 0;
    FBufferSize := 0;
    CloseDriver;
   end else
   begin
    try
     CloseDriver;
     FDriverName := FDriverList[FDriverIndex];
     OpenDriver;
    except 
     exit;
    end;
    if assigned(FDriver) then
    begin
     FDriver.GetDriverName(DrName);
     FDriverVersion := FDriver.GetDriverVersion;
     CanTimeCode;   CanTimeInfo;    CanTransport;
     CanInputGain;  CanInputMeter;
     CanOutputGain; CanOutputMeter;
    end;
   end;
   if assigned(fOnDriverChanged) then OnDriverChanged(self);
   Active := tmpActive;
  end;
end;

function TCustomASIOHostBasic.CreateBuffers: Boolean;
var i             : integer;
    currentbuffer : PASIOBufferInfo;
begin
 if FDriver = nil then
 begin
  result := false;
  Exit;
 end;
 if FBuffersCreated then DestroyBuffers;
 FDriver.GetBufferSize(Fmin, Fmax, Fpref, Fgran);
 if Fmin = Fmax then Fpref := Fmin;
 FBufferSize := Fpref;
 if (FBufferSize < 1) or (FBufferSize > 65530)
  then FBufferSize := 4096;
 FDriver.GetSampleRate(FSampleRate);
 SetSampleRate(FSampleRate);
 FDriver.GetChannels(FInputChannelCount, FOutputChannelCount);

 GetMem(FUnAlignedBuffer, SizeOf(TAsioBufferInfo) * (FInputChannelCount + FOutputChannelCount) + 16);
 FInputBuffer := PASIOBufferInfo(Integer(FUnAlignedBuffer)+16-(Integer(FUnAlignedBuffer) mod 16));

 SetLength(InputChannelInfos, FInputChannelCount);
 SetLength(FInConvertors, FInputChannelCount);
 currentbuffer := FInputBuffer;
 for i := 0 to FInputChannelCount - 1 do
  begin
   InputChannelInfos[i].channel := i;
   InputChannelInfos[i].isInput := ASIOTrue;
   FDriver.GetChannelInfo(InputChannelInfos[i]);
    case InputChannelInfos[i].vType of
     ASIOSTInt16MSB:   FInConvertors[i] := FromInt16MSB;
     ASIOSTInt24MSB:   FInConvertors[i] := FromInt24MSB;
     ASIOSTInt32MSB:   FInConvertors[i] := FromInt32MSB;
     ASIOSTFloat32MSB: FInConvertors[i] := FromSingleMSB;
     ASIOSTFloat64MSB: FInConvertors[i] := FromDoubleMSB;
     ASIOSTInt32MSB16: FInConvertors[i] := FromInt32MSB16;
     ASIOSTInt32MSB18: FInConvertors[i] := FromInt32MSB18;
     ASIOSTInt32MSB20: FInConvertors[i] := FromInt32MSB20;
     ASIOSTInt32MSB24: FInConvertors[i] := FromInt32MSB24;
     ASIOSTInt16LSB:   FInConvertors[i] := FromInt16LSB;
     ASIOSTInt24LSB:   FInConvertors[i] := FromInt24LSB;
     ASIOSTInt32LSB:   FInConvertors[i] := FromInt32LSB;
     ASIOSTFloat32LSB: FInConvertors[i] := FromSingleLSB;
     ASIOSTFloat64LSB: FInConvertors[i] := FromDoubleLSB;
     ASIOSTInt32LSB16: FInConvertors[i] := FromInt32LSB16;
     ASIOSTInt32LSB18: FInConvertors[i] := FromInt32LSB18;
     ASIOSTInt32LSB20: FInConvertors[i] := FromInt32LSB20;
     ASIOSTInt32LSB24: FInConvertors[i] := FromInt32LSB24;
    end;

   currentbuffer^.isInput := ASIOTrue;
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 FOutputBuffer := currentbuffer;
 SetLength(OutputChannelInfos, FOutputChannelCount);
 SetLength(FOutConvertors, FOutputChannelCount);
 for i := 0 to FOutputChannelCount - 1 do
  begin
   OutputChannelInfos[i].channel := i;
   OutputChannelInfos[i].isInput := ASIOFalse;   //  output
   FDriver.GetChannelInfo(OutputChannelInfos[i]);
   case OutputChannelInfos[i].vType of
    ASIOSTInt16MSB:   FOutConvertors[i] := ToInt16MSB;
    ASIOSTInt24MSB:   FOutConvertors[i] := ToInt24MSB;
    ASIOSTInt32MSB:   FOutConvertors[i] := ToInt32MSB;
    ASIOSTFloat32MSB: FOutConvertors[i] := ToSingleMSB;
    ASIOSTFloat64MSB: FOutConvertors[i] := ToDoubleMSB;
    ASIOSTInt32MSB16: FOutConvertors[i] := ToInt32MSB16;
    ASIOSTInt32MSB18: FOutConvertors[i] := ToInt32MSB18;
    ASIOSTInt32MSB20: FOutConvertors[i] := ToInt32MSB20;
    ASIOSTInt32MSB24: FOutConvertors[i] := ToInt32MSB24;
    ASIOSTInt16LSB:   FOutConvertors[i] := ToInt16LSB;
    ASIOSTInt24LSB:   FOutConvertors[i] := ToInt24LSB;
    ASIOSTInt32LSB:   FOutConvertors[i] := ToInt32LSB;
    ASIOSTFloat32LSB: FOutConvertors[i] := ToSingleLSB;
    ASIOSTFloat64LSB: FOutConvertors[i] := ToDoubleLSB;
    ASIOSTInt32LSB16: FOutConvertors[i] := ToInt32LSB16;
    ASIOSTInt32LSB18: FOutConvertors[i] := ToInt32LSB18;
    ASIOSTInt32LSB20: FOutConvertors[i] := ToInt32LSB20;
    ASIOSTInt32LSB24: FOutConvertors[i] := ToInt32LSB24;
   end;
   currentbuffer^.isInput := ASIOfalse;  // create an output buffer
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 result := (FDriver.CreateBuffers(FInputBuffer,
  (FInputChannelCount + FOutputChannelCount), Fpref, FCallbacks) = ASE_OK);
 FDriver.GetLatencies(FInputLatency, FOutputLatency);
 if Assigned (FOnBuffersCreate) then FOnBuffersCreate(Self);
 if Assigned (FOnLatencyChanged) then FOnLatencyChanged(Self);
 Randomize;
end;

procedure TCustomASIOHostBasic.DestroyBuffers;
begin
 if (FDriver = nil) then Exit;
 if FBuffersCreated then
 begin
  if Assigned (FOnBuffersDestroy)
   then FOnBuffersDestroy(Self);
  FreeMem(FUnAlignedBuffer);
  FUnAlignedBuffer := nil;
  FInputBuffer := nil;
  FOutputBuffer := nil;
  try
   FDriver.DisposeBuffers;
  except
  end;
  FBuffersCreated := false;
  SetLength(InputChannelInfos, 0);
  SetLength(OutputChannelInfos, 0);
 end;
end;

procedure TCustomASIOHostBasic.OpenDriver;
var tmpActive: Boolean;

  function Succeeded(Res: HResult): Boolean;
  begin Result := Res and $80000000 = 0; end;

begin
 tmpActive := false;
 if assigned(FDriver) then
 begin
  try
   tmpActive := Active;
   Active := False;
   CloseDriver;
  except
  end;
 end;
 if FDriverIndex >= 0 then
 begin
 {$IFDEF OpenASIO}
 try
  if OpenASIOCreate(FASIOdriverlist[FDriverIndex].id, FDriver) then
  begin
   if assigned(FDriver) then
    if not Succeeded(FDriver.Init(fHandle)) then
     FDriver := nil;
  end;
 except
 end;
{$ELSE}
  try
  if CreateBeRoASIO(FASIOdriverlist[FDriverIndex].id, FDriver) then
  begin
   {$IFNDEF FPC}
   {$ENDIF}
   if assigned(FDriver) then
    if not Succeeded(FDriver.Init(fHandle)) then FDriver := nil;
  end;
  except
   FDriver := nil;
  end;
{$ENDIF}
 end;
 if fDriver = nil then raise Exception.Create('ASIO Driver Failed!');
 FBuffersCreated := CreateBuffers;
 if tmpActive then Active := True;
end;

procedure TCustomASIOHostBasic.CloseDriver;
begin
 if assigned(FDriver) then
 begin
  try
   if FBuffersCreated then DestroyBuffers;
  except
  end;
  FDriver := nil;  // RELEASE;
 end;
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannelCount := 0;
 FOutputChannelCount := 0;
 FSampleRate := 44100;
end;

procedure TCustomASIOHostBasic.ControlPanel;
begin
 if assigned(FDriver)
  then FDriver.ControlPanel;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHostBasic.Mixer;
begin
 FASIOMixer.Show;
end;
{$ENDIF}

procedure TCustomASIOHostBasic.ReadState(Reader: TReader);
begin
  inherited;
  if Assigned(FOnCreate) then FOnCreate(Self);
end;

procedure TCustomASIOHostBasic.Reset;
begin
 OpenDriver; // restart the driver
 if Assigned (FOnReset) then FOnReset(Self);
end;

{$IFDEF FPC}
procedure TCustomASIOHostBasic.PMASIO(var Message: TLMessage);
{$ELSE}
procedure TCustomASIOHostBasic.PMASIO(var Message: TMessage);
{$ENDIF}
var inp, outp: integer;
begin
 if FDriver = nil then exit;
 case Message.WParam of
  AM_ResetRequest:
   begin
    OpenDriver; // restart the driver
    if Assigned (FOnReset) then FOnReset(Self);
   end;
  AM_BufferSwitch: BufferSwitch(Message.LParam); // process a buffer
  AM_BufferSwitchTimeInfo: BufferSwitchTimeInfo(Message.LParam,
   ASIOTime.FBufferTime);  // process a buffer with time
  AM_LatencyChanged:
   begin
    if assigned(FDriver) then FDriver.GetLatencies(inp, outp);
    if assigned(FOnLatencyChanged) then FOnLatencyChanged(Self);
   end;
 end;
end;

{$IFDEF FPC}
procedure TCustomASIOHostBasic.PMUpdateSamplePos(var Message: TLMessage);
{$ELSE}
procedure TCustomASIOHostBasic.PMUpdateSamplePos(var Message: TMessage);
{$ENDIF}
var Samples: TASIOSamples;
begin
 Samples.hi := Message.wParam;
 Samples.lo := Message.LParam;
 if Assigned(FOnUpdateSamplePos)
  then FOnUpdateSamplePos(Self,ASIOSamplesToInt64(Samples));
end;

procedure TCustomASIOHostBasic.BufferSwitch(index: integer);
begin
 FillChar(ASIOTime.FBufferTime, SizeOf(TASIOTime), 0);
 // get the time stamp of the buffer, not necessary if no
 // synchronization to other media is required
 if FDriver.GetSamplePosition(ASIOTime.FBufferTime.timeInfo.samplePosition,
  ASIOTime.FBufferTime.timeInfo.systemTime) = ASE_OK then
   ASIOTime.Flags := ASIOTime.Flags + [atSystemTimeValid,atSamplePositionValid];
 BufferSwitchTimeInfo(index, ASIOTime.FBufferTime);
end;

procedure TCustomASIOHostBasic.BufferSwitchTimeInfo(index: integer;
 const params: TASIOTime);
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := params.timeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.timeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);
 if assigned(FOnBufferSwitch) then FOnBufferSwitch(Self,@(FInputBuffer^),index);
 FDriver.OutputReady;
end;

procedure TCustomASIOHostBasic.SetSampleRate(const Value: Double);
begin
 FSampleRate := Value;
 if (FSampleRate <= 0) or (FSampleRate > 1048575)
  then FSampleRate := 44100;
 ASIOTime.SampleRate := Value;
 if assigned(FDriver) then FDriver.SetSampleRate(Value);
end;

procedure TCustomASIOHostBasic.SetActive(Value: Boolean);
var currentbuffer : PASIOBufferInfo;
    i             : Integer;
    sz            : Word;
begin
 if FDriver = nil then exit;
 if FActive = Value then exit;
 if Value = True then
 begin
  try
   FActive := (FDriver.Start = ASE_OK);
  except
   FBufferSize := 2048;
   FSampleRate := 44100;
  end;
  if FActive = False then FDriver.Stop;
 end else
 begin
  FActive := False;
  try
   FDriver.Stop;
  except
  end;
  if FBuffersCreated then
  begin
   currentbuffer := FOutputBuffer;
   for i := 0 to FOutputChannelCount - 1 do
    begin
     if OutputChannelInfos[i].vType in [ASIOSTInt16MSB,ASIOSTInt16LSB] then sz:=SizeOf(Word) else
     if OutputChannelInfos[i].vType in [ASIOSTInt24MSB,ASIOSTInt24LSB] then sz:=3 else
     if OutputChannelInfos[i].vType in [ASIOSTFloat32LSB,ASIOSTFloat32MSB] then sz:=SizeOf(Single) else
     if OutputChannelInfos[i].vType in [ASIOSTFloat64LSB,ASIOSTFloat64MSB] then sz:=SizeOf(Double)
      else sz:=SizeOf(Integer);
     FillChar(currentbuffer^.buffers[0]^, FBufferSize * sz, 0);
     FillChar(currentbuffer^.buffers[1]^, FBufferSize * sz, 0);
     inc(currentbuffer);
    end;
   currentbuffer := FInputBuffer;
   for i := 0 to FInputChannelCount - 1 do
    begin
     if InputChannelInfos[i].vType in [ASIOSTInt16MSB,ASIOSTInt16LSB] then sz:=SizeOf(Word) else
     if InputChannelInfos[i].vType in [ASIOSTInt24MSB,ASIOSTInt24LSB] then sz:=3 else
     if InputChannelInfos[i].vType in [ASIOSTFloat32LSB,ASIOSTFloat32MSB] then sz:=SizeOf(Single) else
     if InputChannelInfos[i].vType in [ASIOSTFloat64LSB,ASIOSTFloat64MSB] then sz:=SizeOf(Double)
      else sz:=SizeOf(Integer);
     FillChar(currentbuffer^.buffers[0]^, FBufferSize * sz, 0);
     FillChar(currentbuffer^.buffers[1]^, FBufferSize * sz, 0);
     inc(currentbuffer);
    end;
  end;
 end;
end;

function TCustomASIOHostBasic.GetNumDrivers: integer;
begin
 result := length(FASIOdriverlist);
end;

function TCustomASIOHostBasic.CanSampleRate(sampleRate: TASIOSampleRate): TASIOError;
begin
 if assigned(FDriver)
  then result := FDriver.CanSampleRate(SampleRate)
  else result := ASE_NotPresent;
end;

{$IFDEF FPC}
procedure TCustomASIOHostBasic.PMBufferSwitch(var Message: TLMessage);
{$ELSE}
procedure TCustomASIOHostBasic.PMBufferSwitch(var Message: TMessage);
{$ENDIF}
begin
 BufferSwitch(Message.LParam);
end;

{$IFDEF FPC}
procedure TCustomASIOHostBasic.PMBufferSwitchTimeInfo(var Message: TLMessage);
{$ELSE}
procedure TCustomASIOHostBasic.PMBufferSwitchTimeInfo(var Message: TMessage);
{$ENDIF}
begin
 BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);
end;

function TCustomASIOHostBasic.GetInputMeter(Channel:Integer): Integer;
var ACC : TASIOChannelControls;
begin
 if FDriver = nil then begin result := -1; Exit; end;
 ACC.isInput:=1; ACC.Channel:=Channel;
 FDriver.Future(kAsioGetInputMeter,@ACC);
 Result:=ACC.meter;
end;

function TCustomASIOHostBasic.GetOutputMeter(Channel:Integer): Integer;
var ACC : TASIOChannelControls;
begin
 if FDriver = nil then begin result := -1; Exit; end;
 ACC.isInput:=0; ACC.Channel:=Channel;
 FDriver.Future(kAsioGetOutputMeter,@ACC);
 Result:=ACC.meter;
end;

procedure TCustomASIOHostBasic.SetInputGain(Channel:Integer; Gain: Integer);
var ACC : TASIOChannelControls;
begin
 if FDriver = nil then Exit;
 ACC.isInput:=1; ACC.Channel:=Channel; ACC.Gain:=Gain;
 FDriver.Future(kAsioSetInputGain,@ACC);
end;

procedure TCustomASIOHostBasic.GetOutputGain(Channel:Integer; Gain: Integer);
var ACC : TASIOChannelControls;
begin
 if FDriver = nil then Exit;
 ACC.isInput:=0; ACC.Channel:=Channel; ACC.Gain:=Gain;
 FDriver.Future(kAsioSetOutputGain,@ACC);
end;

function TCustomASIOHostBasic.CanTimeInfo: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanTimeInfo,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdTimeInfo];
end;

function TCustomASIOHostBasic.CanTimeCode: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanTimeCode,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdTimeCode];
end;

function TCustomASIOHostBasic.CanTransport: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanTransport,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdTransport];
end;

function TCustomASIOHostBasic.CanInputGain: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanInputGain,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdInputGain];
end;

function TCustomASIOHostBasic.CanInputMeter: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanInputMeter,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdInputMeter];
end;

function TCustomASIOHostBasic.CanOutputGain: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanOutputGain,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdOutputGain];
end;

function TCustomASIOHostBasic.CanOutputMeter: Boolean;
begin
 if FDriver = nil
  then Result:=False
  else Result:=FDriver.Future(kAsioCanOutputMeter,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdOutputMeter];
end;

procedure TCustomASIOHostBasic.SetASIOCanDos(const Value: TASIOCanDos); begin end;

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// TCustomASIOHost ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

constructor TCustomASIOHost.Create(AOwner: TComponent);
begin
  fClipPrevent:=ClipDigital;
  FConvertOptimizations := [coSSE, co3DNow];
  FOutputDither:=odNone;
  FInputMonitor := imDisabled;

  {$IFDEF ASIOMixer} FASIOMixer := TFmASIOMixer.Create(nil); {$ENDIF}
  inherited;
end;

destructor TCustomASIOHost.Destroy;
begin
 SetLength(FInConvertors, 0);
 SetLength(FOutConvertors, 0);
 SetLength(FOutputVolume, 0);
 SetLength(FInConvertors, 0);
 SetLength(FOutConvertors, 0);
 SetLength(FOutputVolume, 0);
 {$IFDEF ASIOMixer} FASIOMixer.Free; {$ENDIF}
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TCustomASIOHost.SetInputChannelOffset(const w: Word);
begin
 if (w <> FInputChannelOffset) and (w < FInputChannelCount)
  then FInputChannelOffset := w;
end;

procedure TCustomASIOHost.SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
begin
 FOnBufferSwitch32 := Value;
 if assigned(FOnBufferSwitch64) then FConvertMethod:=cm64 else
 if assigned(FOnBufferSwitch32) then FConvertMethod:=cm32
  else FConvertMethod:=cmNone;
end;

procedure TCustomASIOHost.SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
begin
 FOnBufferSwitch64 := Value;
 if assigned(FOnBufferSwitch64) then FConvertMethod:=cm64 else
 if assigned(FOnBufferSwitch32) then FConvertMethod:=cm32
  else FConvertMethod:=cmNone;
end;

procedure TCustomASIOHost.SetOutputChannelOffset(const w: Word);
begin
 if (w <> FOutputChannelOffset) and (w < FOutputChannelCount)
  then FOutputChannelOffset := w;
end;

procedure TCustomASIOHost.SetConvertOptimizations(const co: TConvertOptimizations);
begin
 Use_x87;
 case FPUType of
  fpuSSE: if coSSE in co then Use_SSE;
  fpu3DNow: if co3DNow in co then Use_3DNow;
 end;
 FConvertOptimizations := co;
end;

procedure TCustomASIOHost.SetASIOGenerator(const v: TASIOGenerator);
begin
 if v <> FASIOGenerator then
  begin
   FASIOGenerator := v;
   if Assigned(FASIOGenerator) then
    begin
     FASIOGenerator.BlockSize := FBufferSize;
     FASIOGenerator.SampleRate := FSampleRate;
    end;
  end;
end;

procedure TCustomASIOHost.SetPreventClipping(v : TPreventClipping);
begin
 fPreventClipping:=v;
 case fPreventClipping of
  pcDigital: fClipPrevent:=ClipDigital;
  pcAnalog: fClipPrevent:=ClipAnalog;
 end;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHost.VolumeChange(Sender: TObject);
begin
 FOutputVolume[(Sender As TFrChannelStrip).Channel] := (Sender As TFrChannelStrip).Volume;
 if (Sender As TFrChannelStrip).Mute then
  FOutputVolume[(Sender As TFrChannelStrip).Channel] := 0;
end;

procedure TCustomASIOHost.SetupMixer;
var i: Integer;
begin
 for i := 0 to Length(FASIOMixer.ChannelsStrips) - 1 do
  FASIOMixer.ChannelsStrips[i].Free;
 SetLength(FASIOMixer.ChannelsStrips, FOutputChannels);
 for i := FOutputChannels - 1 downto 0 do
  begin
   FASIOMixer.ChannelsStrips[i] := TFrChannelStrip.Create(FASIOMixer);
   FASIOMixer.ChannelsStrips[i].Width := 44;
   FASIOMixer.ChannelsStrips[i].Name := 'ChannelStrip' + IntToStr(i);
   FASIOMixer.ChannelsStrips[i].Parent := FASIOMixer.MixerPanel;
   FASIOMixer.ChannelsStrips[i].Align := alLeft;
   FASIOMixer.ChannelsStrips[i].OnVolumeChange := VolumeChange;
   FASIOMixer.ChannelsStrips[i].OnMuteChange := VolumeChange;
   FASIOMixer.ChannelsStrips[i].Channel := FOutputChannels - 1 - i;
  end;
 if FOutputChannels > 0 then
  begin
   FASIOMixer.ClientHeight := 20 + FASIOMixer.ChannelsStrips[0].Height;
   FASIOMixer.ClientWidth := 20 + FOutputChannels * FASIOMixer.ChannelsStrips[0].Width;
  end;
end;
{$ENDIF ASIOMixer}

function TCustomASIOHost.CreateBuffers: Boolean;
var i             : integer;
    currentbuffer : PASIOBufferInfo;
begin
 if FDriver = nil then
 begin
  result := false;
  Exit;
 end;
 if FBuffersCreated then DestroyBuffers;
 FDriver.GetBufferSize(Fmin, Fmax, Fpref, Fgran);
 if Fmin = Fmax then Fpref := Fmin;
 FBufferSize := Fpref;
 if (FBufferSize < 1) or (FBufferSize > 65530)
  then FBufferSize := 4096;
 if Assigned(FASIOGenerator) then FASIOGenerator.BlockSize := FBufferSize;
 FDriver.GetSampleRate(FSampleRate);
 SetSampleRate(FSampleRate);
 FDriver.GetChannels(FInputChannelCount, FOutputChannelCount);
 SetLength(FOutputVolume, FOutputChannelCount);
 for i := 0 to FOutputChannelCount - 1 do FOutputVolume[i] := 1;
 {$IFDEF ASIOMixer} SetupMixer; {$ENDIF}

 GetMem(FUnAlignedBuffer, SizeOf(TAsioBufferInfo) * (FInputChannelCount + FOutputChannelCount) + 16);
 FInputBuffer := PASIOBufferInfo(Integer(FUnAlignedBuffer)+16-(Integer(FUnAlignedBuffer) mod 16));

 SetLength(InputChannelInfos, FInputChannelCount);
 SetLength(FSingleInBuffer, FInputChannelCount);
 SetLength(FDoubleInBuffer, FInputChannelCount);
 SetLength(FInConvertors, FInputChannelCount);
 currentbuffer := FInputBuffer;
 for i := 0 to FInputChannelCount - 1 do
  begin
   InputChannelInfos[i].channel := i;
   InputChannelInfos[i].isInput := ASIOTrue;
   FDriver.GetChannelInfo(InputChannelInfos[i]);
    case InputChannelInfos[i].vType of
     ASIOSTInt16MSB:   FInConvertors[i] := FromInt16MSB;
     ASIOSTInt24MSB:   FInConvertors[i] := FromInt24MSB;
     ASIOSTInt32MSB:   FInConvertors[i] := FromInt32MSB;
     ASIOSTFloat32MSB: FInConvertors[i] := FromSingleMSB;
     ASIOSTFloat64MSB: FInConvertors[i] := FromDoubleMSB;
     ASIOSTInt32MSB16: FInConvertors[i] := FromInt32MSB16;
     ASIOSTInt32MSB18: FInConvertors[i] := FromInt32MSB18;
     ASIOSTInt32MSB20: FInConvertors[i] := FromInt32MSB20;
     ASIOSTInt32MSB24: FInConvertors[i] := FromInt32MSB24;
     ASIOSTInt16LSB:   FInConvertors[i] := FromInt16LSB;
     ASIOSTInt24LSB:   FInConvertors[i] := FromInt24LSB;
     ASIOSTInt32LSB:   FInConvertors[i] := FromInt32LSB;
     ASIOSTFloat32LSB: FInConvertors[i] := FromSingleLSB;
     ASIOSTFloat64LSB: FInConvertors[i] := FromDoubleLSB;
     ASIOSTInt32LSB16: FInConvertors[i] := FromInt32LSB16;
     ASIOSTInt32LSB18: FInConvertors[i] := FromInt32LSB18;
     ASIOSTInt32LSB20: FInConvertors[i] := FromInt32LSB20;
     ASIOSTInt32LSB24: FInConvertors[i] := FromInt32LSB24;
    end;

   SetLength(FSingleInBuffer[i], FBufferSize);
   SetLength(FDoubleInBuffer[i], FBufferSize);
   FillChar(FSingleInBuffer[i,0], FBufferSize * SizeOf(Single), 0);
   FillChar(FDoubleInBuffer[i,0], FBufferSize * SizeOf(Double), 0);
   currentbuffer^.isInput := ASIOTrue;
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 FOutputBuffer := currentbuffer;
 SetLength(OutputChannelInfos, FOutputChannelCount);
 SetLength(FSingleOutBuffer, FOutputChannelCount);
 SetLength(FDoubleOutBuffer, FOutputChannelCount);
 SetLength(FOutConvertors, FOutputChannelCount);
 for i := 0 to FOutputChannelCount - 1 do
  begin
   OutputChannelInfos[i].channel := i;
   OutputChannelInfos[i].isInput := ASIOFalse;   //  output
   FDriver.GetChannelInfo(OutputChannelInfos[i]);
   case OutputChannelInfos[i].vType of
    ASIOSTInt16MSB:   FOutConvertors[i] := ToInt16MSB;
    ASIOSTInt24MSB:   FOutConvertors[i] := ToInt24MSB;
    ASIOSTInt32MSB:   FOutConvertors[i] := ToInt32MSB;
    ASIOSTFloat32MSB: FOutConvertors[i] := ToSingleMSB;
    ASIOSTFloat64MSB: FOutConvertors[i] := ToDoubleMSB;
    ASIOSTInt32MSB16: FOutConvertors[i] := ToInt32MSB16;
    ASIOSTInt32MSB18: FOutConvertors[i] := ToInt32MSB18;
    ASIOSTInt32MSB20: FOutConvertors[i] := ToInt32MSB20;
    ASIOSTInt32MSB24: FOutConvertors[i] := ToInt32MSB24;
    ASIOSTInt16LSB:   FOutConvertors[i] := ToInt16LSB;
    ASIOSTInt24LSB:   FOutConvertors[i] := ToInt24LSB;
    ASIOSTInt32LSB:   FOutConvertors[i] := ToInt32LSB;
    ASIOSTFloat32LSB: FOutConvertors[i] := ToSingleLSB;
    ASIOSTFloat64LSB: FOutConvertors[i] := ToDoubleLSB;
    ASIOSTInt32LSB16: FOutConvertors[i] := ToInt32LSB16;
    ASIOSTInt32LSB18: FOutConvertors[i] := ToInt32LSB18;
    ASIOSTInt32LSB20: FOutConvertors[i] := ToInt32LSB20;
    ASIOSTInt32LSB24: FOutConvertors[i] := ToInt32LSB24;
   end;
   SetLength(FSingleOutBuffer[i], FBufferSize);
   SetLength(FDoubleOutBuffer[i], FBufferSize);
   FillChar(FSingleOutBuffer[i,0], FBufferSize * SizeOf(Single), 0);
   FillChar(FDoubleOutBuffer[i,0], FBufferSize * SizeOf(Double), 0);
   currentbuffer^.isInput := ASIOfalse;  // create an output buffer
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 result := (FDriver.CreateBuffers(FInputBuffer,
  (FInputChannelCount + FOutputChannelCount), Fpref, FCallbacks) = ASE_OK);
 FDriver.GetLatencies(FInputLatency, FOutputLatency);
 if Assigned (FOnBuffersCreate) then FOnBuffersCreate(Self);
 if Assigned (FOnLatencyChanged) then FOnLatencyChanged(Self);
 Randomize;
end;

procedure TCustomASIOHost.DestroyBuffers;
begin
 if (FDriver = nil) then Exit;
 if FBuffersCreated then
 begin
  if Assigned (FOnBuffersDestroy)
   then FOnBuffersDestroy(Self);
  FreeMem(FUnAlignedBuffer);
  FUnAlignedBuffer := nil;
  FInputBuffer := nil;
  FOutputBuffer := nil;
  try
   FDriver.DisposeBuffers;
  except
  end;
  FBuffersCreated := false;
  FSingleInBuffer := nil;
  FSingleOutBuffer := nil;
  SetLength(InputChannelInfos, 0);
  SetLength(OutputChannelInfos, 0);
 end;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHost.Mixer;
begin
 FASIOMixer.Show;
end;
{$ENDIF}

procedure TCustomASIOHost.BufferSwitchTimeInfo(index: integer;
 const params: TASIOTime);
var i, j                : Integer;
    currentbuffer       : PASIOBufferInfo;
    PChannelArray       : Pointer;
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := params.timeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.timeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);
 currentbuffer := FInputBuffer;

 if assigned(FOnBufferSwitchNative) then FOnBufferSwitchNative(Self,@(FInputBuffer^),index) else
 if FConvertMethod=cm64 then
  begin
   // 64Bit float processing
   case FInBufferPreFill of
    bpfZero: for j := 0 to FInputChannelCount - 1
              do FillChar(FDoubleInBuffer[j,0], FBufferSize * SizeOf(Double), 0);
    bpfNoise: for j := 0 to FInputChannelCount - 1 do
               for i := 0 to FBufferSize - 1 do FDoubleInBuffer[j,i] := 2 * Random - 1;
    else
     begin
      for j := 0 to FInputChannelCount - 1 do
       begin
        PChannelArray := currentbuffer^.buffers[Index];
        if Assigned(PChannelArray) then
          FInConvertors[j].ic64(PChannelArray, PDouble(FDoubleInBuffer[j]), FBufferSize);
        inc(currentbuffer);
       end;
     end;
   end;

   if fPreventClipping<>pcNone
    then for j := 0 to FInputChannelCount - 1 do fClipPrevent.cb64(@FDoubleInBuffer[j,0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for j := 0 to FOutputChannelCount - 1
               do FillChar(FDoubleOutBuffer[j,0], FBufferSize * SizeOf(Double), 0);
    bpfNoise: for j := 0 to FOutputChannelCount - 1 do
               for i := 0 to FBufferSize - 1
                do FDoubleOutBuffer[j,i] := 2 * Random - 1;
   end;

   case FInputMonitor of
    imMono: Move(FDoubleInBuffer[FInputChannelOffset,0], FDoubleOutBuffer[FOutputChannelOffset,0], FBufferSize * SizeOf(Double));
    imStereo:
     begin
      Move(FDoubleInBuffer[FInputChannelOffset  ,0], FDoubleOutBuffer[FOutputChannelOffset  ,0], FBufferSize * SizeOf(Double));
      Move(FDoubleInBuffer[FInputChannelOffset+1,0], FDoubleOutBuffer[FOutputChannelOffset+1,0], FBufferSize * SizeOf(Double));
     end;
    imAll:
     for j := 0 to min(FInputChannelCount, FOutputChannelCount) - 1 do
      Move(FDoubleInBuffer[j,0], FDoubleOutBuffer[j,0], FBufferSize * SizeOf(Double));
   end;

   FOnBufferSwitch64(Self, FDoubleInBuffer, FDoubleOutBuffer);

   if fPreventClipping<>pcNone then
    for j := 0 to FOutputChannelCount - 1 do fClipPrevent.cb64(@FDoubleOutBuffer[j,0] ,FBufferSize);

   currentbuffer := FOutputBuffer;
   for j := 0 to FOutputChannelCount - 1 do
    begin
     PChannelArray := currentbuffer^.buffers[Index];
     if assigned(PChannelArray)
      then FOutConvertors[j].oc64(PDouble(FDoubleOutBuffer[j]),PChannelArray, FBufferSize);
     inc(currentbuffer);
    end;
  end
 else
  begin
   case FInBufferPreFill of
    bpfZero: for j := 0 to FInputChannelCount - 1
              do FillChar(FSingleInBuffer[j,0], FBufferSize * SizeOf(Single), 0);
    bpfNoise: for j := 0 to FInputChannelCount - 1 do
               for i := 0 to FBufferSize - 1 do FSingleInBuffer[j,i] := 2 * Random - 1;
    bpfCustom: if Assigned(FASIOGenerator) then FASIOGenerator.ProcessBlock(FSingleInBuffer, false);
    else
     begin
      for j := 0 to FInputChannelCount - 1 do
       begin
        PChannelArray := currentbuffer^.buffers[Index];
        if Assigned(PChannelArray) then
          FInConvertors[j].ic32(PChannelArray, PSingle(FSingleInBuffer[j]), FBufferSize);
        inc(currentbuffer);
       end;
     end;
   end;

   if fPreventClipping<>pcNone
    then for j := 0 to FInputChannelCount - 1 do fClipPrevent.cb32(@FSingleInBuffer[j,0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for j := 0 to FOutputChannelCount - 1
               do FillChar(FSingleOutBuffer[j,0], FBufferSize * SizeOf(Single), 0);
    bpfNoise: for j := 0 to FOutputChannelCount - 1 do
               for i := 0 to FBufferSize - 1
                do FSingleOutBuffer[j,i] := 2 * Random - 1;
    bpfCustom: if Assigned(FASIOGenerator) then FASIOGenerator.ProcessBlock(FSingleOutBuffer, true);
   end;

   case FInputMonitor of
    imMono: Move(FSingleInBuffer[FInputChannelOffset,0], FSingleOutBuffer[FOutputChannelOffset,0], FBufferSize * SizeOf(Single));
    imStereo:
     begin
      Move(FSingleInBuffer[FInputChannelOffset  ,0], FSingleOutBuffer[FOutputChannelOffset  ,0], FBufferSize * SizeOf(Single));
      Move(FSingleInBuffer[FInputChannelOffset+1,0], FSingleOutBuffer[FOutputChannelOffset+1,0], FBufferSize * SizeOf(Single));
     end;
    imAll:
     for j := 0 to min(FInputChannelCount, FOutputChannelCount) - 1 do
      Move(FSingleInBuffer[j,0], FSingleOutBuffer[j,0], FBufferSize * SizeOf(Single));
   end;

   if Assigned(FOnBufferSwitch32)
    then FOnBufferSwitch32(Self, FSingleInBuffer, FSingleOutBuffer);

   if fPreventClipping<>pcNone then
    for j := 0 to FOutputChannelCount - 1 do fClipPrevent.cb32(@FSingleOutBuffer[j,0] ,FBufferSize);

   currentbuffer := FOutputBuffer;
   for j := 0 to FOutputChannelCount - 1 do
    begin
     PChannelArray := currentbuffer^.buffers[Index];
     if assigned(PChannelArray)
      then FOutConvertors[j].oc32(PSingle(FSingleOutBuffer[j]),PChannelArray, FBufferSize);
     inc(currentbuffer);
    end;
  end;
 FDriver.OutputReady;
end;

procedure TCustomASIOHost.SetOutputDither(const Value: TASIOOutputDither);
begin
 if FOutputDither <> Value then
  begin
   FOutputDither := Value;
   case FOutputDither of
    odNone :
      begin
       Use_x87;
       case FPUType of
        fpuSSE: if coSSE in FConvertOptimizations then Use_SSE;
        fpu3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
       end;
      end;
    odUDF  : Use_x87_UDF;
    odTDF  : Use_x87_TDF;
   end;
  end;
end;

procedure Register;
begin
 RegisterComponents('Audio', [TASIOHost]);
 RegisterComponents('Audio', [TASIOHostBasic]);
 {$IFDEF DELPHI5}
 {$IFDEF D5CP}
 RegisterComponentEditor(TASIOHost, TASIOControlPanel);
 {$ENDIF}
 {$ENDIF}
end;

initialization

 PMUpdSamplePos.Msg := PM_UpdateSamplePos;
 PMBufSwitch.Msg := PM_BufferSwitch;
 PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
 PMReset.Msg := PM_Reset;

end.
