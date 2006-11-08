unit DASIOHost;
// This unit allows you to open an ASIO audio driver and access
// its inputs and outputs. The component was written by
// Christian Budde and Tobias Fleischer, with an extension by
// Benjamin Rosseaux. Please give credit if you use this component in your
// programs. Thanks to Martin Fay (original Delphi ASIO interface)

{$R DASIOHost.res}
{$I JEDI.INC}
{.$DEFINE OpenASIO}
// define OpenASIO to compile using old OpenASIO interface (needs OpenASIO.dll)

{.$DEFINE ASIOMixer}
// define ASIOMixer to compile with the ASIO mixer
{-$DEFINE D5CP}
// define D5CP to compile with the ASIO control panel design time for Delphi 5

interface

uses Windows, Messages, SysUtils, Classes, ActiveX, ASIO, DASIOConvert, Types,
     DASIOGenerator, {$IFDEF OpenASIO} OpenAsio {$ELSE} BeroASIO {$ENDIF},
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
    id   : TCLSID;
    name : array[0..511] of char;
    path : array[0..511] of char;
  end;
  PAsioDriverDesc = ^TAsioDriverDesc;

  TAsioDriverList = array of TAsioDriverDesc;
  TASIOCanDo = (acdInputMonitor, acdTimeInfo, acdTimeCode, acdTransport,
                acdInputGain, acdInputMeter, acdOutputGain, acdOutputMeter);
  TASIOCanDos = set of TASIOCanDo;

  TConvertMethod = (cmNone, cm32, cm64);
  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TSamplePositionUpdateEvent = procedure(Sender: TObject; SamplePosition: Int64) of object;
  TSample2Event = procedure(Sender: TObject; Sample: array of Single) of object;
  TBufferSwitchEvent32 = procedure(Sender: TObject; const InBuffer, OutBuffer: TArrayOfSingleDynArray) of object;
  TBufferSwitchEvent64 = procedure(Sender: TObject; const InBuffer, OutBuffer: TArrayOfDoubleDynArray) of object;
  TBufferSwitchEventNative = procedure(Sender: TObject; const InputBuffer, OutputBuffer: PASIOBufferInfo; const BufferIndex : Integer) of object;

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

  TASIOHost = class(TComponent)
  private
    FActive               : Boolean;
    fHandle               : HWND;
    FPreventClipping      : TPreventClipping;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;
    FDriverIndex          : Integer;
    FDriverList           : TStrings;
    FDriverName           : String;
    FDriverVersion        : integer;
    FInputLatency         : Integer;
    FOutputLatency        : Integer;
    FInputChannels        : Integer;
    FOutputChannels       : Integer;
    FSampleRate           : Double;
    FBufferSize           : Cardinal;
    FASIOTime             : TASIOTimeSub;
    FOnCreate             : TNotifyEvent;
    FOnDestroy            : TNotifyEvent;
    FOnReset              : TNotifyEvent;
    FOnDriverChanged      : TNotifyEvent;
    FOnLatencyChanged     : TNotifyEvent;
    FOnBuffersCreate      : TNotifyEvent;
    FOnBuffersDestroy     : TNotifyEvent;
    FOnSampleRateChanged  : TNotifyEvent;
    FOnSample2Output      : TSample2Event;
    FOnInput2Sample       : TSample2Event;
    FOnUpdateSamplePos    : TSamplePositionUpdateEvent;
    FOnBufferSwitch32     : TBufferSwitchEvent32;
    FOnBufferSwitch64     : TBufferSwitchEvent64;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
    FInputChannelOffset   : Word;
    FOutputChannelOffset  : Word;
    fASIOCanDos           : TASIOCanDos;
    Fmin, Fmax,
    Fpref, Fgran          : Integer;
    FInConvertors         : array of TInConvertor;
    FOutConvertors        : array of TOutConvertor;
    FASIOGenerator        : TASIOGenerator;
    ASIOdriverlist        : TASIODriverList;
    {$IFDEF OpenASIO}
    Driver                : IOpenAsio;
    {$ELSE}
    Driver                : IBeroASIO;
    {$ENDIF}
    BuffersCreated        : boolean;
    callbacks             : TASIOCallbacks;
    SingleInBuffer        : TArrayOfSingleDynArray;
    SingleOutBuffer       : TArrayOfSingleDynArray;
    DoubleInBuffer        : TArrayOfDoubleDynArray;
    DoubleOutBuffer       : TArrayOfDoubleDynArray;
    UnAlignedBuffer       : PASIOBufferInfo;
    InputBuffer           : PASIOBufferInfo;
    OutputBuffer          : PASIOBufferInfo;
    FInputMonitor         : TInputMonitor;
    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    {$IFDEF ASIOMixer}
    FASIOMixer            : TFmASIOMixer;
    {$ENDIF}
    procedure SetActive(Value: Boolean);
    procedure SetDriverIndex(Value: Integer);
    function CreateBuffers: Boolean;
    procedure DestroyBuffers;
    procedure BufferSwitch(index: integer);
    procedure BufferSwitchTimeInfo(index: integer; const params: TASIOTime);
    procedure SetSampleRate(const Value: Double);
    procedure SetDriverName(const s: String);
    procedure SetInputChannelOffset(const w: Word);
    procedure SetOutputChannelOffset(const w: Word);
    procedure SetConvertOptimizations(const co: TConvertOptimizations);
    procedure SetASIOGenerator(const v: TASIOGenerator);
    procedure SetPreventClipping(v: TPreventClipping);
    function GetBufferSize: Cardinal;
    function GetSampleRate: Double;
    procedure WndProc(var Msg: TMessage);
    {$IFDEF ASIOMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
    {$ENDIF}
    function GetInputMeter(Channel:Integer): Integer;
    function GetOutputMeter(Channel:Integer): Integer;
    function CanInputGain: Boolean;
    function CanInputMeter: Boolean;
    function CanOutputGain: Boolean;
    function CanOutputMeter: Boolean;
    procedure SetASIOCanDos(const Value: TASIOCanDos);
    function CanTimeCode: Boolean;
    function CanTimeInfo: Boolean;
    function CanTransport: Boolean;
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
  protected
    procedure PMASIO(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage); message PM_BufferSwitchTimeInfo;
    function GetDriverList: TStrings;
    procedure ReadState(Reader: TReader); override;
  public
    InputChannelInfos   : array of TASIOChannelInfo;
    OutputChannelInfos  : array of TASIOChannelInfo;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ControlPanel;
    {$IFDEF ASIOMixer}
    procedure Mixer;
    {$ENDIF}
    procedure Reset;
    function GetNumDrivers: integer;
    procedure OpenDriver;
    procedure CloseDriver;
    function CanSampleRate(sampleRate: TASIOSampleRate): TASIOError;
    procedure GetOutputGain(Channel, Gain: Integer);
    procedure SetInputGain(Channel, Gain: Integer);
    property InputMeter[Channel:Integer]: Integer read GetInputMeter;
    property OutputMeter[Channel:Integer]: Integer read GetOutputMeter;
  published
    property Active: Boolean read FActive write SetActive default false;
    property CanDos : TASIOCanDos read fASIOCanDos write SetASIOCanDos;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping default pcNone;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: integer read FDriverVersion;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex default -1;
    property BufferSize: Cardinal read GetBufferSize stored false default 1;
    property BufferMinimum: Integer read Fmin stored false;
    property BufferMaximum: Integer read Fmax stored false;
    property BufferPreferredSize: Integer read Fpref stored false;
    property BufferGranularity: Integer read Fgran stored false;
    property CustomGenerator: TASIOGenerator read FASIOGenerator Write SetASIOGenerator;
    property InputLatency: Integer read FInputLatency stored false default 0;
    property InputChannels: Integer read FInputChannels stored false default 0;
    property InputChannelOffset : Word read FInputChannelOffset write SetInputChannelOffset default 0;
    property OutputLatency: Integer read FOutputLatency stored false default 0;
    property OutputChannels: Integer read FOutputChannels stored false default 0;
    property OutputChannelOffset: Word read FOutputChannelOffset write SetOutputChannelOffset default 0;
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property ASIOTime: TASIOTimeSub read FASIOTime Write FASIOTime;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnBuffersCreate: TNotifyEvent read FOnBuffersCreate write FOnBuffersCreate;
    property OnBuffersDestroy: TNotifyEvent read FOnBuffersDestroy write FOnBuffersDestroy;
    property OnInput2Sample: TSample2Event read FOnInput2Sample write FOnInput2Sample;
    property OnSample2Output: TSample2Event read FOnSample2Output write FOnSample2Output;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnBufferSwitch32: TBufferSwitchEvent32 read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchEvent64 read FOnBufferSwitch64 write SetOnBufferSwitch64;
    property OnBufferSwitchNative: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property InputMonitor: TInputMonitor read FInputMonitor write FInputMonitor default imDisabled;
    property DriverList: TStrings read FDriverList;
  end;

var theHost             : TASIOHost;
    PMUpdSamplePos      : TMessage;
    PMBufSwitch         : TMessage;
    PMBufSwitchTimeInfo : TMessage;
    PMReset             : TMessage;

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
 if (Component as TASIOHost).DriverIndex >= 0 then
  Result := 1 else Result := 0;
end;

procedure TASIOControlPanel.ExecuteVerb(Index: Integer);
begin
 case Index of
 0: if (Component as TASIOHost).DriverIndex >= 0 then
  (Component as TASIOHost).ControlPanel;
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

procedure TASIOHost.WndProc(var Msg: TMessage);
begin
 with Msg do Result := DefWindowProc(fHandle, Msg, wParam, lParam);
end;

constructor TASIOHost.Create(AOwner: TComponent);
begin
  fClipPrevent:=ClipDigital;
//  if AOwner is TForm then Handy := TForm(AOwner).Handle else Handy := Application.Handle;
  {$IFNDEF FPC}
  fHandle:=AllocateHWnd(WndProc);
  {$ENDIF}
  theHost := Self;
  UnAlignedBuffer:=nil;
  InputBuffer := nil;
  OutputBuffer := nil;
  ASIOTime := TASIOTimeSub.Create;
  {$IFNDEF FPC}
  FDriverList := GetDriverList;
  {$ENDIF}
  FConvertOptimizations := [coSSE, co3DNow];

  // set the callbacks record fields
  callbacks.bufferSwitch := ASIOBufferSwitch;
  callbacks.sampleRateDidChange := ASIOSampleRateDidChange;
  callbacks.ASIOMessage := ASIOMessage;
  callbacks.bufferSwitchTimeInfo := ASIOBufferSwitchTimeInfo;

  // set the driver itself to nil for now
  Driver := nil;
  BuffersCreated := FALSE;

  // and make sure all controls are enabled or disabled
  FDriverIndex := -1;
  FInputMonitor := imDisabled;

  {$IFDEF ASIOMixer} FASIOMixer := TFmASIOMixer.Create(nil); {$ENDIF}
  inherited;
end;

destructor TASIOHost.Destroy;
begin
 if Assigned(FOnDestroy) then FOnDestroy(Self);
 callbacks.bufferSwitchTimeInfo := nil;
 if Active then Active := False;
 CloseDriver;
 {$IFNDEF FPC}
 DeallocateHWnd(fHandle);
 {$ENDIF}
 SetLength(ASIOdriverlist, 0);
 SetLength(FInConvertors, 0);
 SetLength(FOutConvertors, 0);
 SetLength(FOutputVolume, 0);
 SetLength(ASIOdriverlist, 0);
 SetLength(FInConvertors, 0);
 SetLength(FOutConvertors, 0);
 SetLength(FOutputVolume, 0);
 FDriverList.Free;
 ASIOTime.Free;
 inherited;
 {$IFDEF ASIOMixer} FASIOMixer.Free; {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////

function TASIOHost.GetDriverList: TStrings;
var i : Integer;
begin
 Result := TStringList.Create;
 SetLength(ASIOdriverlist, 0);
 ListASIODrivers(ASIOdriverlist);
 for i := Low(ASIOdriverlist) to High(ASIOdriverlist) do
  Result.Add(ASIOdriverlist[i].name);
end;

procedure TASIOHost.SetDriverName(const s:String);
begin
 if FDriverList.IndexOf(s) > -1
  then DriverIndex := FDriverList.IndexOf(s);
end;

procedure TASIOHost.SetInputChannelOffset(const w: Word);
begin
 if (w <> FInputChannelOffset) and (w < FInputChannels)
  then FInputChannelOffset := w;
end;

procedure TASIOHost.SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
begin
 FOnBufferSwitch32 := Value;
 if assigned(FOnBufferSwitch64) then FConvertMethod:=cm64 else
 if assigned(FOnBufferSwitch32) then FConvertMethod:=cm32
  else FConvertMethod:=cmNone;
end;

procedure TASIOHost.SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
begin
 FOnBufferSwitch64 := Value;
 if assigned(FOnBufferSwitch64) then FConvertMethod:=cm64 else
 if assigned(FOnBufferSwitch32) then FConvertMethod:=cm32
  else FConvertMethod:=cmNone;
end;

procedure TASIOHost.SetOutputChannelOffset(const w: Word);
begin
 if (w <> FOutputChannelOffset) and (w < FOutputChannels)
  then FOutputChannelOffset := w;
end;

procedure TASIOHost.SetConvertOptimizations(const co: TConvertOptimizations);
begin
 Use_x87;
 case FPUType of
 fpuSSE: if coSSE in co then Use_SSE;
 fpu3DNow: if co3DNow in co then Use_3DNow;
 end;
 FConvertOptimizations := co;
end;

procedure TASIOHost.SetASIOGenerator(const v: TASIOGenerator);
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

procedure TASIOHost.SetDriverIndex(Value: Integer);
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
    FInputChannels := 0;
    FOutputChannels := 0;
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
    if assigned(Driver) then
    begin
     Driver.GetDriverName(DrName);
     FDriverVersion := Driver.GetDriverVersion;
     CanTimeCode;   CanTimeInfo;    CanTransport;
     CanInputGain;  CanInputMeter;
     CanOutputGain; CanOutputMeter;
    end;
   end;
   if assigned(fOnDriverChanged) then OnDriverChanged(self);
   Active := tmpActive;
  end;
end;

procedure TASIOHost.SetPreventClipping(v : TPreventClipping);
begin
 fPreventClipping:=v;
 case fPreventClipping of
  pcDigital: fClipPrevent:=ClipDigital;
  pcAnalog: fClipPrevent:=ClipAnalog;
 end;
end;

{$IFDEF ASIOMixer}
procedure TASIOHost.VolumeChange(Sender: TObject);
begin
 FOutputVolume[(Sender As TFrChannelStrip).Channel] := (Sender As TFrChannelStrip).Volume;
 if (Sender As TFrChannelStrip).Mute then
  FOutputVolume[(Sender As TFrChannelStrip).Channel] := 0;
end;

procedure TASIOHost.SetupMixer;
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

function TASIOHost.CreateBuffers: Boolean;
var i             : integer;
    currentbuffer : PASIOBufferInfo;
begin
 if Driver = nil then
 begin
  result := false;
  Exit;
 end;
 if BuffersCreated then DestroyBuffers;
 Driver.GetBufferSize(Fmin, Fmax, Fpref, Fgran);
 if Fmin = Fmax then Fpref := Fmin;
 FBufferSize := Fpref;
 if Assigned(FASIOGenerator) then FASIOGenerator.BlockSize := FBufferSize;
 Driver.GetSampleRate(FSampleRate);
 if FSampleRate < 0 then FSampleRate := 44100;
 SetSampleRate(FSampleRate);
 Driver.GetChannels(FInputChannels, FOutputChannels);
 SetLength(FOutputVolume, FOutputChannels);
 for i := 0 to FOutputChannels - 1 do FOutputVolume[i] := 1;
 {$IFDEF ASIOMixer} SetupMixer; {$ENDIF}

 {$IFNDEF FPC}
 GetMem(UnAlignedBuffer, SizeOf(TAsioBufferInfo) * (FInputChannels + FOutputChannels) + 16);
 InputBuffer := Ptr(Integer(UnAlignedBuffer)+16-(Integer(UnAlignedBuffer) mod 16));
 {$ENDIF}

 SetLength(InputChannelInfos, FInputChannels);
 SetLength(SingleInBuffer, FInputChannels);
 SetLength(DoubleInBuffer, FInputChannels);
 SetLength(FInConvertors, FInputChannels);
 currentbuffer := InputBuffer;
 for i := 0 to FInputChannels - 1 do
  begin
   InputChannelInfos[i].channel := i;
   InputChannelInfos[i].isInput := ASIOTrue;
   Driver.GetChannelInfo(InputChannelInfos[i]);
    case InputChannelInfos[i].vType of
     ASIOSTInt16MSB:   FInConvertors[i] := ToInt16MSB;
     ASIOSTInt24MSB:   FInConvertors[i] := ToInt24MSB;
     ASIOSTInt32MSB:   FInConvertors[i] := ToInt32MSB;
     ASIOSTFloat32MSB: FInConvertors[i] := ToFloat32MSB;
     ASIOSTFloat64MSB: FInConvertors[i] := ToFloat64MSB;
     ASIOSTInt32MSB16: FInConvertors[i] := ToInt32MSB16;
     ASIOSTInt32MSB18: FInConvertors[i] := ToInt32MSB18;
     ASIOSTInt32MSB20: FInConvertors[i] := ToInt32MSB20;
     ASIOSTInt32MSB24: FInConvertors[i] := ToInt32MSB24;
     ASIOSTInt16LSB:   FInConvertors[i] := ToInt16LSB;
     ASIOSTInt24LSB:   FInConvertors[i] := ToInt24LSB;
     ASIOSTInt32LSB:   FInConvertors[i] := ToInt32LSB;
     ASIOSTFloat32LSB: FInConvertors[i] := ToFloat32LSB;
     ASIOSTFloat64LSB: FInConvertors[i] := ToFloat64LSB;
     ASIOSTInt32LSB16: FInConvertors[i] := ToInt32LSB16;
     ASIOSTInt32LSB18: FInConvertors[i] := ToInt32LSB18;
     ASIOSTInt32LSB20: FInConvertors[i] := ToInt32LSB20;
     ASIOSTInt32LSB24: FInConvertors[i] := ToInt32LSB24;
    end;

   SetLength(SingleInBuffer[i], BufferSize * SizeOf(Single));
   SetLength(DoubleInBuffer[i], BufferSize * SizeOf(Single));
   FillChar(SingleInBuffer[i,0], BufferSize * SizeOf(Single), 0);
   currentbuffer^.isInput := ASIOTrue;
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 OutputBuffer := currentbuffer;
 SetLength(OutputChannelInfos, FOutputChannels);
 SetLength(SingleOutBuffer, FOutputChannels);
 SetLength(DoubleOutBuffer, FOutputChannels);
 SetLength(FOutConvertors, FOutputChannels);
 for i := 0 to FOutputChannels - 1 do
  begin
   OutputChannelInfos[i].channel := i;
   OutputChannelInfos[i].isInput := ASIOFalse;   //  output
   Driver.GetChannelInfo(OutputChannelInfos[i]);
   case OutputChannelInfos[i].vType of
    ASIOSTInt16MSB:   FOutConvertors[i] := FromInt16MSB;
    ASIOSTInt24MSB:   FOutConvertors[i] := FromInt24MSB;
    ASIOSTInt32MSB:   FOutConvertors[i] := FromInt32MSB;
    ASIOSTFloat32MSB: FOutConvertors[i] := FromFloat32MSB;
    ASIOSTFloat64MSB: FOutConvertors[i] := FromFloat64MSB;
    ASIOSTInt32MSB16: FOutConvertors[i] := FromInt32MSB16;
    ASIOSTInt32MSB18: FOutConvertors[i] := FromInt32MSB18;
    ASIOSTInt32MSB20: FOutConvertors[i] := FromInt32MSB20;
    ASIOSTInt32MSB24: FOutConvertors[i] := FromInt32MSB24;
    ASIOSTInt16LSB:   FOutConvertors[i] := FromInt16LSB;
    ASIOSTInt24LSB:   FOutConvertors[i] := FromInt24LSB;
    ASIOSTInt32LSB:   FOutConvertors[i] := FromInt32LSB;
    ASIOSTFloat32LSB: FOutConvertors[i] := FromFloat32LSB;
    ASIOSTFloat64LSB: FOutConvertors[i] := FromFloat64LSB;
    ASIOSTInt32LSB16: FOutConvertors[i] := FromInt32LSB16;
    ASIOSTInt32LSB18: FOutConvertors[i] := FromInt32LSB18;
    ASIOSTInt32LSB20: FOutConvertors[i] := FromInt32LSB20;
    ASIOSTInt32LSB24: FOutConvertors[i] := FromInt32LSB24;
   end;
   SetLength(SingleOutBuffer[i], BufferSize * SizeOf(Single));
   SetLength(DoubleOutBuffer[i], BufferSize * SizeOf(Single));
   FillChar(SingleOutBuffer[i,0], BufferSize * SizeOf(Single), 0);
   currentbuffer^.isInput := ASIOfalse;  // create an output buffer
   currentbuffer^.channelNum := i;
   currentbuffer^.buffers[0] := nil;
   currentbuffer^.buffers[1] := nil;
   inc(currentbuffer);
  end;

 result := (Driver.CreateBuffers(InputBuffer,
  (FInputChannels + FOutputChannels), Fpref, callbacks) = ASE_OK);
 Driver.GetLatencies(FInputLatency, FOutputLatency);
 if Assigned (FOnLatencyChanged) then FOnLatencyChanged(Self);
 if Assigned (FOnBuffersCreate) then FOnBuffersCreate(Self);
 Randomize;
end;

procedure TASIOHost.DestroyBuffers;
begin
 if (Driver = nil) or not BuffersCreated then Exit;
 if Assigned (FOnBuffersDestroy) then FOnBuffersDestroy(Self);

 FreeMem(UnAlignedBuffer);
 UnAlignedBuffer := nil;
 InputBuffer := nil;
 OutputBuffer := nil;
 try
  Driver.DisposeBuffers;
 except
 end;
 BuffersCreated := false;
 SingleInBuffer := nil;
 SingleOutBuffer := nil;
 SetLength(InputChannelInfos, 0);
 SetLength(OutputChannelInfos, 0);
end;

procedure TASIOHost.OpenDriver;
var tmpActive: Boolean;
begin
 tmpActive := false;
 if assigned(Driver) then
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
  if OpenASIOCreate(ASIOdriverlist[FDriverIndex].id, Driver) then
  begin
   if assigned(Driver) then
    if not Succeeded(Driver.Init(Handy)) then
     Driver := nil;
  end;
 except
 end;
{$ELSE}
  try
  if CreateBeRoASIO(ASIOdriverlist[FDriverIndex].id, Driver) then
  begin
   {$IFNDEF FPC}
   if assigned(Driver) then
    if not Succeeded(Driver.Init(fHandle)) then Driver := nil;
   {$ENDIF}
  end;
  except
   Driver := nil;
  end;
{$ENDIF}
 end;
// if Driver = nil then raise Exception.Create('ASIO Driver Failed!');
 BuffersCreated := CreateBuffers;
 if tmpActive then Active := True;
end;

procedure TASIOHost.CloseDriver;
begin
 if assigned(Driver) then
 begin
  try
   if BuffersCreated then DestroyBuffers;
  except
  end;
  Driver := nil;  // RELEASE;
 end;
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannels := 0;
 FOutputChannels := 0;
 FSampleRate := 44100;
end;

procedure TASIOHost.ControlPanel;
begin
 if assigned(Driver) then Driver.ControlPanel;
end;

{$IFDEF ASIOMixer}
procedure TASIOHost.Mixer;
begin
 FASIOMixer.Show;
end;
{$ENDIF}

procedure TASIOHost.ReadState(Reader: TReader);
begin
  inherited;
  if Assigned(FOnCreate) then FOnCreate(Self);
end;

procedure TASIOHost.Reset;
begin
 OpenDriver; // restart the driver
 if Assigned (FOnReset) then FOnReset(Self);
end;

procedure TASIOHost.PMASIO(var Message: TMessage);
var inp, outp: integer;
begin
 if Driver = nil then exit;
 case Message.WParam of
  AM_ResetRequest: Reset;
  AM_BufferSwitch: BufferSwitch(Message.LParam); // process a buffer
  AM_BufferSwitchTimeInfo: BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);  // process a buffer with time
  AM_LatencyChanged:
   begin
    if assigned(Driver) then Driver.GetLatencies(inp, outp);
    if assigned(FOnLatencyChanged) then FOnLatencyChanged(Self);
   end;
 end;
end;

procedure TASIOHost.PMUpdateSamplePos(var Message: TMessage);
var Samples: TASIOSamples;
begin
 Samples.hi := Message.wParam;
 Samples.lo := Message.LParam;
 if Assigned(FOnUpdateSamplePos)
  then FOnUpdateSamplePos(Self,ASIOSamplesToInt64(Samples));
end;

procedure TASIOHost.BufferSwitch(index: integer);
begin
 FillChar(ASIOTime.FBufferTime, SizeOf(TASIOTime), 0);
 // get the time stamp of the buffer, not necessary if no
 // synchronization to other media is required
 if Driver.GetSamplePosition(ASIOTime.FBufferTime.timeInfo.samplePosition,
  ASIOTime.FBufferTime.timeInfo.systemTime) = ASE_OK then
   ASIOTime.Flags := ASIOTime.Flags + [atSystemTimeValid,atSamplePositionValid];
 BufferSwitchTimeInfo(index, ASIOTime.FBufferTime);
end;

procedure TASIOHost.BufferSwitchTimeInfo(index: integer;
 const params: TASIOTime);
var i, j                : Integer;
    currentbuffer       : PASIOBufferInfo;
    PChannelArray       : Pointer;
begin
 if Driver = nil then exit;
 PMUpdSamplePos.wParam := params.timeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.timeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);
 currentbuffer := InputBuffer;

 if assigned(FOnBufferSwitchNative) then FOnBufferSwitchNative(Self,InputBuffer,OutputBuffer,index); 
 if FConvertMethod=cm64 then
  begin
   // 64Bit float processing
   case FInBufferPreFill of
    bpfZero: for j := 0 to FInputChannels - 1
              do FillChar(DoubleInBuffer[j,0], BufferSize * SizeOf(Double), 0);
    bpfNoise: for j := 0 to FInputChannels - 1 do
               for i := 0 to BufferSize - 1 do DoubleInBuffer[j,i] := 2 * Random - 1;
    else
     begin
      for j := 0 to FInputChannels - 1 do
       begin
        PChannelArray := currentbuffer^.buffers[Index];
        if Assigned(PChannelArray) then
          FInConvertors[j].ic64(PChannelArray, PDouble(DoubleInBuffer[j]), BufferSize);
        inc(currentbuffer);
       end;
     end;
   end;

   if fPreventClipping<>pcNone
    then for j := 0 to FInputChannels - 1 do fClipPrevent.cb64(@DoubleInBuffer[j,0], BufferSize);

   case FOutBufferPreFill of
    bpfZero : for j := 0 to FOutputChannels - 1
               do FillChar(DoubleOutBuffer[j,0], BufferSize * SizeOf(Double), 0);
    bpfNoise: for j := 0 to FOutputChannels - 1 do
               for i := 0 to BufferSize - 1
                do DoubleOutBuffer[j,i] := 2 * Random - 1;
   end;

   case FInputMonitor of
    imMono: Move(DoubleInBuffer[FInputChannelOffset,0], DoubleOutBuffer[FOutputChannelOffset,0], BufferSize * SizeOf(Double));
    imStereo:
     begin
      Move(DoubleInBuffer[FInputChannelOffset  ,0], DoubleOutBuffer[FOutputChannelOffset  ,0], BufferSize * SizeOf(Double));
      Move(DoubleInBuffer[FInputChannelOffset+1,0], DoubleOutBuffer[FOutputChannelOffset+1,0], BufferSize * SizeOf(Double));
     end;
    imAll:
     for j := 0 to min(FInputChannels, FOutputChannels) - 1 do
      Move(DoubleInBuffer[j,0], DoubleOutBuffer[j,0], BufferSize * SizeOf(Double));
   end;

   FOnBufferSwitch64(Self, DoubleInBuffer, DoubleOutBuffer);

   if fPreventClipping<>pcNone then
    for j := 0 to FOutputChannels - 1 do fClipPrevent.cb64(@DoubleOutBuffer[j,0] ,BufferSize);

   currentbuffer := OutputBuffer;
   for j := 0 to FOutputChannels - 1 do
    begin
     PChannelArray := currentbuffer^.buffers[Index];
     if assigned(PChannelArray)
      then FOutConvertors[j].oc64(PDouble(DoubleOutBuffer[j]),PChannelArray, BufferSize);
     inc(currentbuffer);
    end;
  end
 else
  begin
   case FInBufferPreFill of
    bpfZero: for j := 0 to FInputChannels - 1
              do FillChar(SingleInBuffer[j,0], BufferSize * SizeOf(Single), 0);
    bpfNoise: for j := 0 to FInputChannels - 1 do
               for i := 0 to BufferSize - 1 do SingleInBuffer[j,i] := 2 * Random - 1;
    bpfCustom: if Assigned(FASIOGenerator) then FASIOGenerator.ProcessBlock(SingleInBuffer, false);
    else
     begin
      for j := 0 to FInputChannels - 1 do
       begin
        PChannelArray := currentbuffer^.buffers[Index];
        if Assigned(PChannelArray) then
          FInConvertors[j].ic32(PChannelArray, PSingle(SingleInBuffer[j]), BufferSize);
        inc(currentbuffer);
       end;
     end;
   end;

   if fPreventClipping<>pcNone
    then for j := 0 to FInputChannels - 1 do fClipPrevent.cb32(@SingleInBuffer[j,0], BufferSize);

   case FOutBufferPreFill of
    bpfZero : for j := 0 to FOutputChannels - 1
               do FillChar(SingleOutBuffer[j,0], BufferSize * SizeOf(Single), 0);
    bpfNoise: for j := 0 to FOutputChannels - 1 do
               for i := 0 to BufferSize - 1
                do SingleOutBuffer[j,i] := 2 * Random - 1;
    bpfCustom: if Assigned(FASIOGenerator) then FASIOGenerator.ProcessBlock(SingleOutBuffer, true);
   end;

   case FInputMonitor of
    imMono: Move(SingleInBuffer[FInputChannelOffset,0], SingleOutBuffer[FOutputChannelOffset,0], BufferSize * SizeOf(Single));
    imStereo:
     begin
      Move(SingleInBuffer[FInputChannelOffset  ,0], SingleOutBuffer[FOutputChannelOffset  ,0], BufferSize * SizeOf(Single));
      Move(SingleInBuffer[FInputChannelOffset+1,0], SingleOutBuffer[FOutputChannelOffset+1,0], BufferSize * SizeOf(Single));
     end;
    imAll:
     for j := 0 to min(FInputChannels, FOutputChannels) - 1 do
      Move(SingleInBuffer[j,0], SingleOutBuffer[j,0], BufferSize * SizeOf(Single));
   end;

   if Assigned(FOnBufferSwitch32)
    then FOnBufferSwitch32(Self, SingleInBuffer, SingleOutBuffer);

   if fPreventClipping<>pcNone then
    for j := 0 to FOutputChannels - 1 do fClipPrevent.cb32(@SingleOutBuffer[j,0] ,BufferSize);

   currentbuffer := OutputBuffer;
   for j := 0 to FOutputChannels - 1 do
    begin
     PChannelArray := currentbuffer^.buffers[Index];
     if assigned(PChannelArray)
      then FOutConvertors[j].oc32(PSingle(SingleOutBuffer[j]),PChannelArray, BufferSize);
     inc(currentbuffer);
    end;
  end;
 Driver.OutputReady;
end;

procedure TASIOHost.SetSampleRate(const Value: Double);
begin
 FSampleRate := Value;
 ASIOTime.SampleRate := Value;
 if assigned(Driver) then Driver.SetSampleRate(Value);
 if assigned(FASIOGenerator) then FASIOGenerator.SampleRate := FSampleRate;
// if Assigned(FOnSampleRateChanged) then FOnSampleRateChanged(theHost);
end;

procedure TASIOHost.SetActive(Value: Boolean);
var currentbuffer : PASIOBufferInfo;
    i : Integer;
begin
 if Driver = nil then exit;
 if FActive = Value then exit;
 if Value = True then
 begin
  try
   FActive := (Driver.Start = ASE_OK);
  except
   FBufferSize := 2048;
   FSampleRate := 44100;
  end;
  if FActive = False then Driver.Stop;
 end else
 begin
  FActive := False;
  try
   Driver.Stop;
  except
  end; 
  if bufferscreated then
  begin
   currentbuffer := OutputBuffer;
   for i := 0 to FOutputChannels - 1 do
   begin
    FillChar(currentbuffer^.buffers[0]^, BufferSize * 4, 0);
    FillChar(currentbuffer^.buffers[1]^, BufferSize * 4, 0);
    inc(currentbuffer);
   end;
   currentbuffer := InputBuffer;
   for i := 0 to FInputChannels - 1 do
   begin
    FillChar(currentbuffer^.buffers[0]^, BufferSize * 4, 0);
    FillChar(currentbuffer^.buffers[1]^, BufferSize * 4, 0);
    inc(currentbuffer);
   end;
  end;
 end;
end;

function TASIOHost.GetNumDrivers: integer;
begin
 result := length(ASIOdriverlist);
end;

function TASIOHost.CanSampleRate(sampleRate: TASIOSampleRate): TASIOError;
begin
 if assigned(Driver)
  then result := Driver.CanSampleRate(sampleRate)
  else result := ASE_NotPresent;
end;

procedure TASIOHost.PMBufferSwitch(var Message: TMessage);
begin
 BufferSwitch(Message.LParam);
end;

procedure TASIOHost.PMBufferSwitchTimeInfo(var Message: TMessage);
begin
 BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);
end;

function TASIOHost.GetBufferSize: Cardinal;
begin
 if (FBufferSize < 1) or (FBufferSize > 65530)
  then FBufferSize := 4096;
 Result := FBufferSize;
end;

function TASIOHost.GetSampleRate: Double;
begin
 if (FSampleRate < 1) or (FSampleRate > 1048575)
  then FSampleRate := 44100;
 Result := FSampleRate;
end;

function TASIOHost.GetInputMeter(Channel:Integer): Integer;
var ACC : TASIOChannelControls;
begin
 if Driver = nil then begin result := -1; Exit; end;
 ACC.isInput:=1; ACC.Channel:=Channel;
 Driver.Future(kAsioGetInputMeter,@ACC);
 Result:=ACC.meter;
end;

function TASIOHost.GetOutputMeter(Channel:Integer): Integer;
var ACC : TASIOChannelControls;
begin
 if Driver = nil then begin result := -1; Exit; end;
 ACC.isInput:=0; ACC.Channel:=Channel;
 Driver.Future(kAsioGetOutputMeter,@ACC);
 Result:=ACC.meter;
end;

procedure TASIOHost.SetInputGain(Channel:Integer; Gain: Integer);
var ACC : TASIOChannelControls;
begin
 if Driver = nil then Exit;
 ACC.isInput:=1; ACC.Channel:=Channel; ACC.Gain:=Gain;
 Driver.Future(kAsioSetInputGain,@ACC);
end;

procedure TASIOHost.GetOutputGain(Channel:Integer; Gain: Integer);
var ACC : TASIOChannelControls;
begin
 if Driver = nil then Exit;
 ACC.isInput:=0; ACC.Channel:=Channel; ACC.Gain:=Gain;
 Driver.Future(kAsioSetOutputGain,@ACC);
end;

function TASIOHost.CanTimeInfo: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanTimeInfo,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdTimeInfo];
end;

function TASIOHost.CanTimeCode: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanTimeCode,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdTimeCode];
end;

function TASIOHost.CanTransport: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanTransport,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdTransport];
end;

function TASIOHost.CanInputGain: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanInputGain,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdInputGain];
end;

function TASIOHost.CanInputMeter: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanInputMeter,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdInputMeter];
end;

function TASIOHost.CanOutputGain: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanOutputGain,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdOutputGain];
end;

function TASIOHost.CanOutputMeter: Boolean;
begin
 if Driver = nil
  then Result:=False
  else Result:=Driver.Future(kAsioCanOutputMeter,nil)=ASE_SUCCESS;
 fASIOCanDos:=fASIOCanDos+[acdOutputMeter];
end;

procedure TASIOHost.SetASIOCanDos(const Value: TASIOCanDos); begin end;

procedure Register;
begin
 RegisterComponents('Audio', [TASIOHost]);
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
