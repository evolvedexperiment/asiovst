unit DAV_ASIOHost;
// This unit allows you to open an ASIO audio driver and access
// its inputs and outputs. The component was written by
// Christian Budde and Tobias Fleischer, with an extension by
// Benjamin Rosseaux. Please give credit if you use this component in your
// programs. Thanks to Martin Fay (original Delphi ASIO interface)

{$I ..\DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC}LCLIntf, LclType, LMessages, LResources,
  {$ELSE}Windows, Messages,{$ENDIF}
  {$IFDEF OpenASIO} DAV_OpenAsio {$ELSE} DAV_BeroASIO {$ENDIF},
  {$IFDEF ASIOMixer} Forms, ComCtrls, Graphics, StdCtrls, DAVASIOMixer,{$ENDIF}
  {$IFDEF DELPHI5} Forms, DsgnIntf, {$ENDIF}
  SysUtils, Classes, Controls,
  DAV_ASIO, DAV_ASIOConvert, DAV_ASIOGenerator, DAV_Common, DAV_AudioData;

const
  // private message
  PM_ASIO = WM_User + 1652;        // unique we hope
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
    Id   : TGUID; //TCLSID;
    Name : array[0..511] of AnsiChar;
    Path : array[0..511] of AnsiChar;
  end;
  PAsioDriverDesc = ^TAsioDriverDesc;
  TASIOBufferList = array [0..0] of TASIOBufferInfo;
  PASIOBufferList = ^TASIOBufferList;

  TASIOSelectorSupport = (assEngineVersion, assResetRequest,
                          assBufferSizeChange, assResyncRequest,
                          assLatenciesChanged, assSupportsTimeInfo,
                          assSupportsTimeCode, assSupportsInputMonitor);
  TASIOSelectorSupports = set of TASIOSelectorSupport;                        

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
  TBufferSwitchEvent32 = procedure(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray) of object;
  TBufferSwitchEvent64 = procedure(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfDoubleFixedArray) of object;
  TBufferSwitchEventNative = procedure(Sender: TObject; const BufferInfo: PASIOBufferList; const BufferIndex : Integer) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfCustom);

  TPreventClipping = (pcNone, pcDigital, pcAnalog);

  {$IFDEF DELPHI10_UP} {$region 'TASIOTimeSub'} {$ENDIF}
  TATFlag = (atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
             atSpeedValid, atSampleRateChanged, atClockSourceChanged);
  TATFlags = set of TATFlag;

  TASIOTimeSub = class(TPersistent)
  private
    FOnChange: TNotifyEvent;
    function GetATInt64(Index: Integer): Int64;
    function GetATdouble(Index: Integer): Double;
    function GetATFlags: TATFlags;
    procedure SetATInt64(Index: Integer; Value: Int64);
    procedure SetATdouble(Index: Integer; Value: Double);
    procedure SetATFlags(Flags: TATFlags);
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
    property Flags : TATFlags read GetATFlags Write SetATFlags;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TASIOTimeSub'} {$ENDIF}

  {$IFDEF D5CP}
  TASIOControlPanel = class(TComponentEditor)
  public
    procedure Edit; override;
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;
  {$ENDIF}

  TCustomAudioDevice = class(TComponent);

  {$IFDEF DELPHI10_UP} {$region 'TASIOHostBasic'} {$ENDIF}
  TCustomASIOHostBasic = class(TCustomAudioDevice)
  private
    FMin, FMax            : Integer;
    Fpref, FGran          : Integer;
    FOnBufferSwitchNative : TBufferSwitchEventNative;
    function GetInputChannelInfo(Index: Integer): TASIOChannelInfo;
    function GetOutputChannelInfo(Index: Integer): TASIOChannelInfo;
    function GetOutConverter(ConverterType: TASIOSampleType): TOutConverter;
    procedure UpdateCanDos;
    procedure ResetDriverSpecificData;
    procedure ClearBuffers;
  protected
    FHandle               : THandle;
    FHandleOwned          : Boolean;
    FAsioTime             : TASIOTimeSub;
    FBuffersCreated       : Boolean;
    FOnCreate             : TNotifyEvent;
    FOnDestroy            : TNotifyEvent;
    FOnReset              : TNotifyEvent;
    FOnDriverChanged      : TNotifyEvent;
    FOnLatencyChanged     : TNotifyEvent;
    FOnSampleRateChanged  : TNotifyEvent;
    FOnBuffersCreate      : TNotifyEvent;
    FOnBuffersDispose     : TNotifyEvent;
    FOnUpdateSamplePos    : TSamplePositionUpdateEvent;
    FOnBufferSwitch       : TBufferSwitchEventNative;
    FASIOCanDos           : TASIOCanDos;
    FAsioDriverList       : TASIODriverList;
    FCallbacks            : TASIOCallbacks;
    FUnAlignedBuffer      : PASIOBufferInfo;
    FSampleRate           : Double;
    FInputBuffer          : PASIOBufferInfo;
    FOutputBuffer         : PASIOBufferInfo;
    FActive               : Boolean;
    FDriverIndex          : Integer;
    FDriverList           : TStrings;
    FDriverName           : String;
    FDriverVersion        : Integer;
    FInputLatency         : Integer;
    FOutputLatency        : Integer;
    FInputChannelCount    : Integer;
    FOutputChannelCount   : Integer;
    FBufferSize           : Cardinal;
    FInputChannelInfos    : array of TASIOChannelInfo;
    FOutputChannelInfos   : array of TASIOChannelInfo;
    FInConverters         : array of TInConverter;
    FOutConverters        : array of TOutConverter;
    {$IFDEF OpenASIO}
    FDriver               : IOpenAsio;
    {$ELSE}
    FDriver               : IBeroASIO;
    {$ENDIF}
    FASIOSelectorSupport  : TASIOSelectorSupports;
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
    function CreateBuffers: Boolean; virtual;
    function GetDriverList: TStrings;
    function GetInputMeter(Channel: Integer): Integer; virtual;
    function GetOutputMeter(Channel: Integer): Integer; virtual;
    function GetInConverter(ConverterType: TASIOSampleType): TInConverter;
    procedure BufferSwitch(Index: Integer); virtual;
    procedure BufferSwitchTimeInfo(Index: Integer; const params: TASIOTime); virtual;
    procedure DisposeBuffers; virtual;
    procedure ReadState(Reader: TReader); override;
    procedure DetermineBuffersize; virtual;
    procedure AquireCurrentSampleRate;
    procedure SetSampleRate(Value: Double); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; virtual;
    function GetNumDrivers: Integer; virtual;
    procedure CloseDriver; virtual;
    procedure ControlPanel; virtual;
    procedure OpenDriver; virtual;
    procedure Reset; virtual;
    procedure SetInputGain(Channel, Gain: Integer); virtual;
    procedure SetOutputGain(Channel, Gain: Integer); virtual;

    property Active: Boolean read FActive write SetActive default False;
    property ASIOTime: TASIOTimeSub read FAsioTime Write FAsioTime;
    property BufferGranularity: Integer read FGran stored False;
    property BufferMaximum: Integer read FMax stored False;
    property BufferMinimum: Integer read FMin stored False;
    property BufferPreferredSize: Integer read Fpref stored False;
    property BufferSize: Cardinal read FBufferSize stored False default 1;
    property CanDos : TASIOCanDos read FASIOCanDos;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex default -1;
    property DriverList: TStrings read FDriverList;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: Integer read FDriverVersion;
    property InputChannelCount: Integer read FInputChannelCount stored False default 0;
    property InputChannelInfos[index : Integer] : TASIOChannelInfo read GetInputChannelInfo;
    property InputLatency: Integer read FInputLatency stored False default 0;
    property InputMeter[Channel:Integer]: Integer read GetInputMeter;
    property OnBuffersCreate: TNotifyEvent read FOnBuffersCreate write FOnBuffersCreate;
    property OnBuffersDispose: TNotifyEvent read FOnBuffersDispose write FOnBuffersDispose;
    property OnBufferSwitch: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OutputChannelCount: Integer read FOutputChannelCount stored False default 0;
    property OutputChannelInfos[index : Integer] : TASIOChannelInfo read GetOutputChannelInfo;
    property OutputLatency: Integer read FOutputLatency stored False default 0;
    property OutputMeter[Channel:Integer]: Integer read GetOutputMeter;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property SelectorSupport : TASIOSelectorSupports read FASIOSelectorSupport write FASIOSelectorSupport;
  end;

  TASIOHostBasic = class(TCustomASIOHostBasic)
  published
    property Active;
    property ASIOTime;
    property BufferGranularity;
    property BufferMaximum;
    property BufferMinimum;
    property BufferPreferredSize;
    property BufferSize;
    property CanDos;
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property OutputChannelCount;
    property OutputLatency;
    property SampleRate;
    property SelectorSupport;
    property OnBuffersCreate;
    property OnBuffersDispose;
    property OnBufferSwitch;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TASIOHostBasic'} {$ENDIF}

  TASIOAudioData32 = class(TAudioData32);
  TASIOAudioData64 = class(TAudioData64);

  {$IFDEF DELPHI10_UP} {$region 'TASIOHost'} {$ENDIF}
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
    FASIOGenerator        : TASIOGenerator;
    FSingleInBuffer       : TDAVArrayOfSingleFixedArray;
    FSingleOutBuffer      : TDAVArrayOfSingleFixedArray;
    FDoubleInBuffer       : TDAVArrayOfDoubleFixedArray;
    FDoubleOutBuffer      : TDAVArrayOfDoubleFixedArray;
    FInputMonitor         : Boolean;
    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TDAVSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    FOutputDither         : TASIOOutputDither;
    {$IFDEF ASIOMixer}
    FASIOMixer            : TFmASIOMixer;
    {$ENDIF}
    procedure SetConvertOptimizations(const Value: TConvertOptimizations);
    procedure SetASIOGenerator(const Value: TASIOGenerator);
    procedure SetPreventClipping(Value: TPreventClipping);
    {$IFDEF ASIOMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
    {$ENDIF}
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
    procedure SetOutputDither(const Value: TASIOOutputDither);
    procedure SetConvertMethod(const Value: TConvertMethod);
    procedure OnBufferSwitchChanged;
  protected
    function CreateBuffers: Boolean; override;
    procedure ConvertMethodChanged; virtual;
    procedure BufferSwitchTimeInfo(Index: Integer; const Params: TASIOTime); override;
    procedure DetermineBuffersize; override;
    procedure ConvertOptimizationsChanged; virtual;
    procedure ASIOGeneratorChanged; virtual;
    procedure PreventClippingChanged; virtual;
    procedure CreateFloatBuffers; virtual;

    property ConvertMethod: TConvertMethod read FConvertMethod write SetConvertMethod default cmNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF ASIOMixer}
    procedure Mixer;
    {$ENDIF}
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;
    property CustomGenerator: TASIOGenerator read FASIOGenerator Write SetASIOGenerator;
    property InputMonitor: Boolean read FInputMonitor write FInputMonitor default False;
    property OnBufferSwitch32: TBufferSwitchEvent32 read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchEvent64 read FOnBufferSwitch64 write SetOnBufferSwitch64;
    property OnBufferSwitchNative: TBufferSwitchEventNative read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property OnInput2Sample: TSample2Event read FOnInput2Sample write FOnInput2Sample;
    property OnSample2Output: TSample2Event read FOnSample2Output write FOnSample2Output;
    property OutputDither: TASIOOutputDither read FOutputDither write SetOutputDither default odNone;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping default pcNone;
  end;

  TASIOHost = class(TCustomASIOHost)
  published
    property Active;
    property ASIOTime;
    property BufferGranularity;
    property BufferMaximum;
    property BufferMinimum;
    property BufferPreferredSize;
    property BufferSize;
    property CanDos;
    property ConvertOptimizations;
    property CustomGenerator;
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property InputMonitor;
    property OutputChannelCount;
    property OutputDither;
    property OutputLatency;
    property PreFillInBuffer;
    property PreFillOutBuffer;
    property PreventClipping;
    property SampleRate;
    property SelectorSupport;
    property OnBuffersCreate;
    property OnBuffersDispose;
    property OnBufferSwitch32;
    property OnBufferSwitch64;
    property OnBufferSwitchNative;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnInput2Sample;
    property OnLatencyChanged;
    property OnReset;
    property OnSample2Output;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TASIOHost'} {$ENDIF}

  TASIOAudioChannel32 = class(TAudioChannel32);
  TASIOAudioChannel64 = class(TAudioChannel64);

  TASIOAudioDataCollection32 = class(TCustomAudioDataCollection32)
  published
    property Channels;
    property SampleRate;
  end;

  TASIOAudioDataCollection64 = class(TCustomAudioDataCollection64)
  published
    property Channels;
    property SampleRate;
  end;

  TBufferSwitchAudioData32Event = procedure(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection32) of object;
  TBufferSwitchAudioData64Event = procedure(Sender: TObject; const InBuffer, OutBuffer: TASIOAudioDataCollection64) of object;

  {$IFDEF DELPHI10_UP} {$region 'TASIOHostAudioData'} {$ENDIF}
  TCustomASIOHostAudioData = class(TCustomASIOHostBasic)
  private
    FPreventClipping      : TPreventClipping;
    FInBufferPreFill      : TBufferPreFill;
    FOutBufferPreFill     : TBufferPreFill;

    FOnBufferSwitch32     : TBufferSwitchAudioData32Event;
    FOnBufferSwitch64     : TBufferSwitchAudioData64Event;

    FConvertOptimizations : TConvertOptimizations;
    FOutputVolume         : TDAVSingleDynArray;
    FClipPrevent          : TClipBuffer;
    FConvertMethod        : TConvertMethod;
    FOutputDither         : TASIOOutputDither;

    FAudioDataInput       : TCustomAudioDataCollection;
    FAudioDataOutput      : TCustomAudioDataCollection;

    {$IFDEF ASIOMixer}
    FASIOMixer            : TFmASIOMixer;
    {$ENDIF}
    procedure SetConvertOptimizations(const Value: TConvertOptimizations);
    procedure SetConvertMethod(const Value: TConvertMethod);
    procedure SetPreventClipping(v: TPreventClipping);
    {$IFDEF ASIOMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
    {$ENDIF}
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchAudioData32Event);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchAudioData64Event);
  protected
    function CreateBuffers: Boolean; override;
    procedure BufferSwitchTimeInfo(Index: Integer; const params: TASIOTime); override;
    procedure DetermineBuffersize; override;
    procedure ConvertMethodChanged; virtual;
    procedure ConvertOptimizationsChanged; virtual;

    property ConvertMethod: TConvertMethod read FConvertMethod write SetConvertMethod default cmNone;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    {$IFDEF ASIOMixer}
    procedure Mixer;
    {$ENDIF}
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;

    property OnBufferSwitch32: TBufferSwitchAudioData32Event read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchAudioData64Event read FOnBufferSwitch64 write SetOnBufferSwitch64;

    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill default bpfNone;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping default pcNone;
  end;

  TASIOHostAudioData = class(TCustomASIOHostAudioData)
  published
    property Active;
    property ASIOTime;
    property BufferGranularity;
    property BufferMaximum;
    property BufferMinimum;
    property BufferPreferredSize;
    property BufferSize;
    property CanDos;
    property ConvertOptimizations;
    property DriverIndex;
    property DriverList;
    property DriverName;
    property DriverVersion;
    property InputChannelCount;
    property InputLatency;
    property OutputChannelCount;
    property OutputLatency;
    property PreFillInBuffer;
    property PreFillOutBuffer;
    property PreventClipping;
    property SampleRate;
    property SelectorSupport;
    property OnBuffersCreate;
    property OnBuffersDispose;
    property OnBufferSwitch32;
    property OnBufferSwitch64;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
  {$IFDEF DELPHI10_UP} {$endregion 'TASIOHostAudioData'} {$ENDIF}

var
  GAsioHost           : TCustomASIOHostBasic;
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

function ChannelTypeToString(vType: TASIOSampleType): string;
procedure ListAsioDrivers(var List: TAsioDriverList);

implementation

uses
  Registry, ComObj, Math {$IFDEF ASIOMixer}, DAVASIOChannelStrip {$ENDIF};

resourcestring
  RStrASIODriverFailed = 'ASIO driver failed!';
  RStrASIONoBuffersCreated = 'ASIO buffers could not be created!';
  RStrConverterTypeUnknown = 'Converter type unknown';
  RCStrIndexOutOfBounds = 'Index out of bounds %d';
  RCStrOnlyOneASIOHost = 'Only one ASIO host is allowed per instance';
  RCStrPreferedBufferSize = 'Prefered buffer size invalid!';
  RCStrDriverNotPresent = 'Driver not present';
  RCStrHardwareMalfunction = 'Hardware malfunctioning';
  RCStrInputParameterInvalid = 'Input parameter invalid';
  RCStrInvalidMode = 'Hardware is in a bad mode or used in a bad mode';
  RCStrSPNotAdvancing = 'Hardware is not running when sample position is inquired';
  RCStrNoClock = 'Sample clock or rate cannot be determined or is not present';
  RCStrNoMemory = 'Not enough memory for completing the request';

const
  CInprocServer = 'InprocServer32';
  CAsioPath     = 'software\asio';
  CComClsId     = 'clsid';

function FindDrivervDLL(const ClsIdStr: string; var DrivervDLL: TFileName): Integer;
{$IFNDEF FPC}
var
  CharBuffer : array[0..1024] of AnsiChar;
  DriverName : TFileName;
{$ENDIF}
begin
 Result := -1;

 with TRegistry.Create do
  try
   RootKey := HKEY_CLASSES_ROOT;
   if OpenKeyReadOnly(CComClsId + '\' + Lowercase(clsidstr) + '\' + CInprocServer) then
    begin
     DrivervDLL := ReadString('');
     {$IFNDEF FPC}
     if not FileExists(DrivervDLL) then
      begin
       CharBuffer[0] := #0;
       DriverName := ExtractFileName(DrivervDLL);   // backup the value

       // try the system directory first
       if GetSystemDirectory(CharBuffer, 1023) <> 0
        then DrivervDLL := StrPas(CharBuffer) + '\' + DriverName;

       // try the windows dir if necessary
       if not FileExists(DrivervDLL) then
        begin
         CharBuffer[0] := #0;
         if GetWindowsDirectory(CharBuffer, 1023) <> 0
          then DrivervDLL := StrPas(CharBuffer) + '\' + DriverName;
        end;
      end;
     {$ENDIF}

     // if driver found set result to zero (no error occured)
     if FileExists(DrivervDLL) then Result := 0;
     CloseKey;
    end;
  finally
   Free;
  end;
end;

procedure ListAsioDrivers(var List: TAsioDriverList);
var
  Keys      : TStringList;
  i         : Integer;
  ID        : string;
  DriverDll : TFileName;
begin
 SetLength(List, 0);

 Keys := TStringList.Create;
 try
  with TRegistry.Create do
   try
    RootKey := HKEY_LOCAL_MACHINE;
    if OpenKeyReadOnly(CAsioPath) then
     begin
      GetKeyNames(Keys);
      CloseKey;
     end;
    for i := 0 to Keys.Count - 1 do
     begin
      if OpenKeyReadOnly(CAsioPath + '\' + Keys[i]) then
       begin
        id := ReadString(CComClsId);
        if FindDrivervDLL(id, DriverDll) = 0 then  // check if the dll exists
         begin
          SetLength(List, Length(List) + 1);
          List[Length(List) - 1].id := StringToGUID(id);
          StrPLCopy(List[Length(List) - 1].Name, Keys[i], 512);
          StrPLCopy(List[Length(List) - 1].Path, DriverDll, 512);
         end;
        CloseKey;
       end;
     end;
   finally
    Free;
   end;
 finally
  FreeAndNil(Keys);
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
 Result := Integer((Component as TCustomASIOHost).DriverIndex >= 0);
end;

procedure TASIOControlPanel.ExecuteVerb(Index: Integer);
begin
 case Index of
 0: if (Component as TCustomASIOHost).DriverIndex >= 0
  then (Component as TCustomASIOHost).ControlPanel;
 end;
end;
{$ENDIF}
{$ENDIF}

{$IFDEF DELPHI10_UP} {$region 'TASIOTimeSub implementation'} {$ENDIF}
constructor TASIOTimeSub.Create;
begin
 with FBufferTime.timeInfo do
  begin
   Speed := 1;
   SampleRate := 44100;
   SamplePosition := Int64ToASIOSamples(0);
  end;
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
   end
 else inherited AssignTo(Dest);
end;

function TASIOTimeSub.GetATFlags: TATFlags;
begin
 Result := [];
 if (FBufferTime.TimeInfo.Flags and kSystemTimeValid) <> 0
  then Result := Result + [atSystemTimeValid]
  else Result := Result - [atSystemTimeValid];
 if (FBufferTime.TimeInfo.Flags and kSamplePositionValid) <> 0
  then Result := Result + [atSamplePositionValid]
  else Result := Result - [atSamplePositionValid];
 if (FBufferTime.TimeInfo.Flags and kSampleRateValid) <> 0
  then Result := Result + [atSampleRateValid]
  else Result := Result - [atSampleRateValid];
 if (FBufferTime.TimeInfo.Flags and kSpeedValid) <> 0
  then Result := Result + [atSpeedValid]
  else Result := Result - [atSpeedValid];
 if (FBufferTime.TimeInfo.Flags and kSampleRateChanged) <> 0
  then Result := Result + [atSampleRateChanged]
  else Result := Result - [atSampleRateChanged];
 if (FBufferTime.TimeInfo.Flags and kClockSourceChanged) <> 0
  then Result := Result + [atClockSourceChanged]
  else Result := Result - [atClockSourceChanged];
end;

procedure TASIOTimeSub.SetATFlags(Flags: TATFlags);
var
  temp: Integer;
begin
 temp := 0;
 if (atSystemTimeValid in Flags) then temp := temp + kSystemTimeValid;
 if (atSamplePositionValid in Flags) then temp := temp + kSamplePositionValid;
 if (atSampleRateValid in Flags) then temp := temp + kSampleRateValid;
 if (atSpeedValid in Flags) then temp := temp + kSpeedValid;
 if (atSampleRateChanged in Flags) then temp := temp + kSampleRateChanged;
 if (atClockSourceChanged in Flags) then temp := temp + kClockSourceChanged;
 FBufferTime.TimeInfo.Flags := temp;
end;

function TASIOTimeSub.GetATdouble(Index :Integer): Double;
begin
 Result := 0;
 case Index of
  0: Result := FBufferTime.TimeInfo.speed;
  1: Result := FBufferTime.TimeInfo.sampleRate;
 end;
end;

procedure TASIOTimeSub.SetATdouble(Index :Integer; Value: Double);
begin
 case Index of
  0: if Value <> FBufferTime.TimeInfo.speed then
  begin
   FBufferTime.TimeInfo.speed := Value;
   Change;
  end;
  1: if Value <> FBufferTime.TimeInfo.sampleRate then
  begin
   FBufferTime.TimeInfo.sampleRate := Value;
   Change;
  end;
 end;
end;

function TASIOTimeSub.GetATInt64(Index :Integer): Int64;
begin
 Result := 0;
 case Index of
  0: Result := ASIOSamplesToInt64(FBufferTime.TimeInfo.samplePosition);
 end;
end;

procedure TASIOTimeSub.SetATInt64(Index :Integer; Value: Int64);
begin
 case Index of
  0: if Value <> ASIOSamplesToInt64(FBufferTime.TimeInfo.samplePosition) then
       begin
        FBufferTime.TimeInfo.SamplePosition := Int64ToASIOSamples(Value);
        Change;
       end;
 end;
end;
{$IFDEF DELPHI10_UP} {$endregion 'TASIOTimeSub implementation'} {$ENDIF}

function ChannelTypeToString(vType: TASIOSampleType): string;
begin
 Result := '';
 case vType of
  ASIOSTInt16MSB   : Result := 'Int16MSB';
  ASIOSTInt24MSB   : Result := 'Int24MSB';
  ASIOSTInt32MSB   : Result := 'Int32MSB';
  ASIOSTFloat32MSB : Result := 'Float32MSB';
  ASIOSTFloat64MSB : Result := 'Float64MSB';

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can be more easily used with these
  ASIOSTInt32MSB16 : Result := 'Int32MSB16';
  ASIOSTInt32MSB18 : Result := 'Int32MSB18';
  ASIOSTInt32MSB20 : Result := 'Int32MSB20';
  ASIOSTInt32MSB24 : Result := 'Int32MSB24';

  ASIOSTInt16LSB   : Result := 'Int16LSB';
  ASIOSTInt24LSB   : Result := 'Int24LSB';
  ASIOSTInt32LSB   : Result := 'Int32LSB';
  ASIOSTFloat32LSB : Result := 'Float32LSB';
  ASIOSTFloat64LSB : Result := 'Float64LSB';

  // these are used for 32 bit data buffer, with different alignment of the data inside
  // 32 bit PCI bus systems can more easily used with these
  ASIOSTInt32LSB16 : Result := 'Int32LSB16';
  ASIOSTInt32LSB18 : Result := 'Int32LSB18';
  ASIOSTInt32LSB20 : Result := 'Int32LSB20';
  ASIOSTInt32LSB24 : Result := 'Int32LSB24';
 end;
end;

procedure ASIOBufferSwitch(DoubleBufferIndex: Integer; DirectProcess: TASIOBool); cdecl;
begin
  DirectProcess := ASIOFalse;
  if assigned(GAsioHost) then
   case DirectProcess of
    ASIOFalse:
    begin
     PMBufSwitch.WParam := AM_BufferSwitch;
     PMBufSwitch.LParam := DoubleBufferIndex;
     GAsioHost.Dispatch(PMBufSwitch);
    end;
    ASIOTrue : GAsioHost.BufferSwitch(DoubleBufferIndex);
   end;
end;

function ASIOBufferSwitchTimeInfo(var Params: TASIOTime;
  DoubleBufferIndex: Integer; DirectProcess: TASIOBool): PASIOTime; cdecl;
begin
  DirectProcess := ASIOFalse;
  if assigned(GAsioHost) then
   case DirectProcess of
    ASIOFalse :
    begin
     GAsioHost.ASIOTime.FBufferTime := Params;
     PMBufSwitchTimeInfo.WParam := AM_BufferSwitchTimeInfo;
     PMBufSwitchTimeInfo.LParam := DoubleBufferIndex;
     GAsioHost.Dispatch(PMBufSwitchTimeInfo);
    end;
    ASIOTrue : GAsioHost.BufferSwitchTimeInfo(DoubleBufferIndex, params);
   end;
  Result := nil;
end;

procedure ASIOSampleRateDidChange(SampleRate: TASIOSampleRate); cdecl;
begin
 if Assigned(GAsioHost) then
  begin
   GAsioHost.SampleRate := SampleRate;
   if Assigned(GAsioHost.FOnSampleRateChanged)
    then GAsioHost.FOnSampleRateChanged(GAsioHost);
  end;
end;

function ASIOMessage(selector, value: Integer; message: pointer; opt: pdouble): Integer; cdecl;
begin
 Result := 0;
 case selector of
  kASIOSelectorSupported :   // return 1 if a selector is supported
   begin
    case value of
     kASIOEngineVersion        : if assigned(GAsioHost) then Result := Integer(assEngineVersion in GAsioHost.FASIOSelectorSupport) else Result := 1;
     kASIOResetRequest         : if assigned(GAsioHost) then Result := Integer(assResetRequest in GAsioHost.FASIOSelectorSupport) else Result := 1;
     kASIOBufferSizeChange     : if assigned(GAsioHost) then Result := Integer(assBufferSizeChange in GAsioHost.FASIOSelectorSupport) else Result := 1;
     kASIOResyncRequest        : if assigned(GAsioHost) then Result := Integer(assResyncRequest in GAsioHost.FASIOSelectorSupport) else Result := 1;
     kASIOLatenciesChanged     : if assigned(GAsioHost) then Result := Integer(assLatenciesChanged in GAsioHost.FASIOSelectorSupport) else Result := 1;
     kASIOSupportsTimeInfo     : if assigned(GAsioHost) then Result := Integer(assSupportsTimeInfo in GAsioHost.FASIOSelectorSupport) else Result := 0;
     kASIOSupportsTimeCode     : if assigned(GAsioHost) then Result := Integer(assSupportsTimeCode in GAsioHost.FASIOSelectorSupport) else Result := 0;
     kASIOSupportsInputMonitor : if assigned(GAsioHost) then Result := Integer(assSupportsInputMonitor in GAsioHost.FASIOSelectorSupport) else Result := 0;
    end;
   end;
  kASIOEngineVersion :  Result := 2;   // ASIO 2 is supported
  kASIOResetRequest :
   if assigned(GAsioHost) then
    begin
     PMReset.Msg := PM_ASIO;
     PMReset.WParam := AM_ResetRequest;
     PMReset.LParam := 0;
     GAsioHost.Dispatch(PMReset);
     Result := 1;
    end;
  kASIOBufferSizeChange :
   if assigned(GAsioHost) then
    begin
     PMReset.Msg := PM_ASIO;
     PMReset.WParam := AM_ResetRequest;
     PMReset.LParam := 0;
     GAsioHost.Dispatch(PMReset);
     Result := 1;
    end;
  kASIOResyncRequest :  ;
  kASIOLatenciesChanged :
   if assigned(GAsioHost) then
    begin
     PMReset.Msg := PM_ASIO;
     PMReset.WParam := AM_LatencyChanged;
     PMReset.LParam := 0;
     GAsioHost.Dispatch(PMReset);
     Result := 1;
    end;
  kASIOSupportsTimeInfo :  Result := 1;
  kASIOSupportsTimeCode :
   begin
    Result := 0;
   end;
  kASIOSupportsInputMonitor :
   begin
    Result := 1;
   end;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
///////////////////////////// TCustomASIOHostBasic /////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomASIOHostBasic implementation'} {$ENDIF}

{ TCustomASIOHostBasic }

constructor TCustomASIOHostBasic.Create(AOwner: TComponent);
begin
  FHandleOwned := False;
  if AOwner is TWinControl
   then FHandle := TWinControl(AOwner).Handle
   else
    begin
     FHandle := AllocateHWnd(WndProc);
     FHandleOwned := True;
    end;

  {$IFNDEF AllowMultipleAsioHosts}
  if GAsioHost <> nil
   then raise Exception.Create(RCStrOnlyOneASIOHost) else
  {$ENDIF}
  GAsioHost            := Self;
  FUnAlignedBuffer     := nil;
  FInputBuffer         := nil;
  FOutputBuffer        := nil;
  FSampleRate          := 44100;
  FAsioTime            := TASIOTimeSub.Create;
  FDriverList          := GetDriverList;
  FASIOSelectorSupport := [assEngineVersion, assResetRequest,
    assBufferSizeChange, assResyncRequest, assLatenciesChanged];

  // set the callbacks record fields
  with FCallbacks do
   begin
    BufferSwitch := ASIOBufferSwitch;
    SampleRateDidChange := ASIOSampleRateDidChange;
    BufferSwitchTimeInfo := ASIOBufferSwitchTimeInfo;
   end;
  // do not add this to the above with statement!
  FCallbacks.ASIOMessage := ASIOMessage;

  // set the driver itself to nil for now
  FDriver := nil;
  FBuffersCreated := False;

  // and make sure all controls are enabled or disabled
  FDriverIndex := -1;
  inherited;
end;

destructor TCustomASIOHostBasic.Destroy;
begin
 try
  if GAsioHost = Self
   then GAsioHost := nil;
  if Assigned(FOnDestroy) then FOnDestroy(Self);
  FCallbacks.BufferSwitchTimeInfo := nil;
  if Active then Active := False;
  CloseDriver;
  if FHandleOwned
   then DeallocateHWnd(FHandle);
  SetLength(FAsioDriverList, 0);
  SetLength(FInConverters, 0);
  SetLength(FOutConverters, 0);
  FreeAndNil(FDriverList);
  FreeAndNil(FAsioTime);
 finally
  inherited;
  GAsioHost := nil;
 end;
end;

{$IFNDEF FPC}
procedure TCustomASIOHostBasic.WndProc(var Msg: TMessage);
begin
 with Msg do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;
{$ELSE}
function DefWindowProc(hWnd:THandle; Msg:UINT; wParam:WPARAM; lParam:LPARAM):LResult; external 'user32' name 'DefWindowProcA';

procedure TCustomASIOHostBasic.WndProc(var Msg: TLMessage);
begin
 with Msg do Result := DefWindowProc(FHandle, Msg, wParam, lParam);
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

function TCustomASIOHostBasic.GetDriverList: TStrings;
var
  i : Integer;
begin
 Result := TStringList.Create;
 SetLength(FAsioDriverList, 0);
 ListASIODrivers(FAsioDriverList);
 for i := Low(FAsioDriverList) to High(FAsioDriverList)
  do Result.Add(FAsioDriverList[i].Name);
end;

procedure TCustomASIOHostBasic.ResetDriverSpecificData;
begin
 FDriverName := '';
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannelCount := 0;
 FOutputChannelCount := 0;
 FBufferSize := 0;
end;

procedure TCustomASIOHostBasic.SetDriverName(const s: string);
begin
 if FDriverList.IndexOf(s) > -1
  then DriverIndex := FDriverList.IndexOf(s);
end;

procedure TCustomASIOHostBasic.SetDriverIndex(Value: Integer);
var
  DrName    : array[0..255] of AnsiChar;
  tmpActive : Boolean;
begin
 if (Value <> FDriverIndex) then
  begin
   tmpActive := Active;
   Active := False;
   if Value < -1 then FDriverIndex := -1 else
    if Value >= FDriverList.Count
     then FDriverIndex := -1
     else FDriverIndex := Value;

   // check if no driver has been selected and reset all driver specific data  
   if FDriverIndex = -1 then
    begin
     ResetDriverSpecificData;
     CloseDriver;
    end
   else
    begin
     try
      CloseDriver;
      FDriverName := FDriverList[FDriverIndex];
      OpenDriver;
     except
      FDriverIndex := -1;
      ResetDriverSpecificData;
      Exit;
     end;

     if assigned(FDriver) then
      begin
       FDriver.GetDriverName(DrName);
       if DrName <> ''
        then FDriverName := DrName;
       FDriverVersion := FDriver.GetDriverVersion;
       UpdateCanDos;
      end;
    end;
   if assigned(FOnDriverChanged)
    then FOnDriverChanged(self);
   Active := tmpActive;
  end;
end;

procedure TCustomASIOHostBasic.DetermineBuffersize;
begin
 FDriver.GetBufferSize(FMin, FMax, Fpref, FGran);
 if FMin = FMax then Fpref := FMin;

 // check prefered buffersize is valid
 if Fpref <= 0
  then raise Exception.Create(RCStrPreferedBufferSize);

 FBufferSize := Fpref;
end;

procedure TCustomASIOHostBasic.AquireCurrentSampleRate;
begin
 FDriver.GetSampleRate(FSampleRate);
 ASIOTime.SampleRate := FSampleRate;
end;

procedure TCustomASIOHostBasic.UpdateCanDos;
begin
 // check whether driver is has been assigned
 if FDriver = nil then
  begin
   FASIOCanDos := [];
   Exit;
  end;

 // test "Time Info"
 if FDriver.Future(kAsioCanTimeInfo, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdTimeInfo]
  else FASIOCanDos := FASIOCanDos - [acdTimeInfo];

 // test "Time Code"
 if FDriver.Future(kAsioCanTimeCode, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdTimeCode]
  else FASIOCanDos := FASIOCanDos - [acdTimeCode];

 // test "Transport"
 if FDriver.Future(kAsioCanTransport, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdTransport]
  else FASIOCanDos := FASIOCanDos - [acdTransport];

 // test "Input Gain"
 if FDriver.Future(kAsioCanInputGain, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdInputGain]
  else FASIOCanDos := FASIOCanDos - [acdInputGain];

 // test "Input Meter"
 if FDriver.Future(kAsioCanInputMeter, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdInputMeter]
  else FASIOCanDos := FASIOCanDos - [acdInputMeter];

 // test "Output Gain"
 if FDriver.Future(kAsioCanOutputGain, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdOutputGain]
  else FASIOCanDos := FASIOCanDos - [acdOutputGain];

 // test "Output Meter"
 if FDriver.Future(kAsioCanOutputMeter, nil) = ASE_SUCCESS
  then FASIOCanDos := FASIOCanDos + [acdOutputMeter]
  else FASIOCanDos := FASIOCanDos - [acdOutputMeter];
end;

function TCustomASIOHostBasic.GetInConverter(ConverterType: TASIOSampleType): TInConverter;
begin
 case ConverterType of
  ASIOSTInt16MSB   : Result := FromInt16MSB;
  ASIOSTInt24MSB   : Result := FromInt24MSB;
  ASIOSTInt32MSB   : Result := FromInt32MSB;
  ASIOSTFloat32MSB : Result := FromSingleMSB;
  ASIOSTFloat64MSB : Result := FromDoubleMSB;
  ASIOSTInt32MSB16 : Result := FromInt32MSB16;
  ASIOSTInt32MSB18 : Result := FromInt32MSB18;
  ASIOSTInt32MSB20 : Result := FromInt32MSB20;
  ASIOSTInt32MSB24 : Result := FromInt32MSB24;
  ASIOSTInt16LSB   : Result := FromInt16LSB;
  ASIOSTInt24LSB   : Result := FromInt24LSB;
  ASIOSTInt32LSB   : Result := FromInt32LSB;
  ASIOSTFloat32LSB : Result := FromSingleLSB;
  ASIOSTFloat64LSB : Result := FromDoubleLSB;
  ASIOSTInt32LSB16 : Result := FromInt32LSB16;
  ASIOSTInt32LSB18 : Result := FromInt32LSB18;
  ASIOSTInt32LSB20 : Result := FromInt32LSB20;
  ASIOSTInt32LSB24 : Result := FromInt32LSB24;
  else raise Exception.Create(RStrConverterTypeUnknown);
 end;
end;

function TCustomASIOHostBasic.GetOutConverter(ConverterType: TASIOSampleType): TOutConverter;
begin
 case ConverterType of
  ASIOSTInt16MSB   : Result := ToInt16MSB;
  ASIOSTInt24MSB   : Result := ToInt24MSB;
  ASIOSTInt32MSB   : Result := ToInt32MSB;
  ASIOSTFloat32MSB : Result := ToSingleMSB;
  ASIOSTFloat64MSB : Result := ToDoubleMSB;
  ASIOSTInt32MSB16 : Result := ToInt32MSB16;
  ASIOSTInt32MSB18 : Result := ToInt32MSB18;
  ASIOSTInt32MSB20 : Result := ToInt32MSB20;
  ASIOSTInt32MSB24 : Result := ToInt32MSB24;
  ASIOSTInt16LSB   : Result := ToInt16LSB;
  ASIOSTInt24LSB   : Result := ToInt24LSB;
  ASIOSTInt32LSB   : Result := ToInt32LSB;
  ASIOSTFloat32LSB : Result := ToSingleLSB;
  ASIOSTFloat64LSB : Result := ToDoubleLSB;
  ASIOSTInt32LSB16 : Result := ToInt32LSB16;
  ASIOSTInt32LSB18 : Result := ToInt32LSB18;
  ASIOSTInt32LSB20 : Result := ToInt32LSB20;
  ASIOSTInt32LSB24 : Result := ToInt32LSB24;
  else raise Exception.Create(RStrConverterTypeUnknown);
 end;
end;

function TCustomASIOHostBasic.CreateBuffers: Boolean;
var
  Channel : Integer;
  Buffer  : PASIOBufferInfo;
begin
 // make sure a driver has been selected
 if FDriver = nil then
  begin
   Result := False;
   Exit;
  end;

 // eventually dispose buffers
 if FBuffersCreated then DisposeBuffers;

 // get default values of the current driver
 DetermineBuffersize;
 AquireCurrentSampleRate;
 FDriver.GetChannels(FInputChannelCount, FOutputChannelCount);

 // allocate memory for input and output buffers
 GetMem(FUnAlignedBuffer, SizeOf(TAsioBufferInfo) * (FInputChannelCount + FOutputChannelCount) + 16);
 Buffer := PASIOBufferInfo(Integer(FUnAlignedBuffer) + 16 - (Integer(FUnAlignedBuffer) mod 16));

 // setup input channel info and converter
 FInputBuffer := Buffer;
 SetLength(FInputChannelInfos, FInputChannelCount);
 SetLength(FInConverters, FInputChannelCount);
 for Channel := 0 to FInputChannelCount - 1 do
  begin
   FInputChannelInfos[Channel].Channel := Channel;
   FInputChannelInfos[Channel].IsInput := ASIOTrue;
   FDriver.GetChannelInfo(FInputChannelInfos[Channel]);
   FInConverters[Channel] := GetInConverter(FInputChannelInfos[Channel].SampleType);

   Buffer^.IsInput := ASIOTrue;
   Buffer^.ChannelNum := Channel;
   Buffer^.Buffers[0] := nil;
   Buffer^.Buffers[1] := nil;
   inc(Buffer);
  end;

 // setup input channel info and converter
 FOutputBuffer := Buffer;
 SetLength(FOutputChannelInfos, FOutputChannelCount);
 SetLength(FOutConverters, FOutputChannelCount);
 for Channel := 0 to FOutputChannelCount - 1 do
  begin
   FOutputChannelInfos[Channel].Channel := Channel;
   FOutputChannelInfos[Channel].IsInput := ASIOFalse;   //  output
   FDriver.GetChannelInfo(FOutputChannelInfos[Channel]);
   FOutConverters[Channel] := GetOutConverter(FOutputChannelInfos[Channel].SampleType);

   Buffer^.IsInput := ASIOFalse;
   Buffer^.ChannelNum := Channel;
   Buffer^.Buffers[0] := nil;
   Buffer^.Buffers[1] := nil;
   inc(Buffer);
  end;

 Assert(FBufferSize > 0);

 Result := (FDriver.CreateBuffers(FInputBuffer,
   (FInputChannelCount + FOutputChannelCount), FBufferSize, FCallbacks) = ASE_OK);
 if Assigned (FOnBuffersCreate) then FOnBuffersCreate(Self);

 // get current latencies
 FDriver.GetLatencies(FInputLatency, FOutputLatency);
 if Assigned (FOnLatencyChanged) then FOnLatencyChanged(Self);

 // initialize random generator for online noise processing
 Randomize;
end;

procedure TCustomASIOHostBasic.DisposeBuffers;
begin
 if (FDriver = nil) then Exit;
 if FBuffersCreated then
  begin
   if Assigned (FOnBuffersDispose)
    then FOnBuffersDispose(Self);
   FInputBuffer := nil;
   FOutputBuffer := nil;
   Dispose(FUnAlignedBuffer);
   FUnAlignedBuffer := nil;
   try
    FDriver.DisposeBuffers;
   except
   end;
   FBuffersCreated := False;
   SetLength(FInputChannelInfos, 0);
   SetLength(FOutputChannelInfos, 0);
  end;
end;

procedure TCustomASIOHostBasic.OpenDriver;
var
  OldActive    : Boolean;
  ErrorMessage : PChar;
begin
 // store last active state and deactivate current driver
 OldActive := False;
 if assigned(FDriver) then
  try
   Active := False;
   CloseDriver;
  except
  end;

 // if a driver index has been assigned open/create ASIO interface
 if FDriverIndex >= 0 then
  try
   {$IFDEF OpenASIO}
   if OpenASIOCreate(FAsioDriverList[FDriverIndex].Id, FDriver) then
   {$ELSE}
   if CreateBeRoASIO(FAsioDriverList[FDriverIndex].Id, FDriver) then
    {$ENDIF}
    try
     if assigned(FDriver) then
      case FDriver.Init(FHandle) of
       0 : begin
            // equals false
            GetMem(ErrorMessage, 128);
            try
             FDriver.GetErrorMessage(ErrorMessage);
             raise Exception.Create(ErrorMessage);
            finally
             Dispose(ErrorMessage);
            end;
           end;

       // the below codes are here due to incompatibility of some soundcards    
       ASE_NotPresent       : raise Exception.Create(RCStrDriverNotPresent);
       ASE_HWMalfunction    : raise Exception.Create(RCStrHardwareMalfunction);
       ASE_InvalidParameter : raise Exception.Create(RCStrInputParameterInvalid);
       ASE_InvalidMode      : raise Exception.Create(RCStrInvalidMode);
       ASE_SPNotAdvancing   : raise Exception.Create(RCStrSPNotAdvancing);
       ASE_NoClock          : raise Exception.Create(RCStrNoClock);
       ASE_NoMemory         : raise Exception.Create(RCStrNoMemory);
      end;
    except
     FDriver := nil;
    end;
  except
   FDriver := nil;
  end;

 // check driver is assigned
 if FDriver = nil
  then raise Exception.Create(RStrASIODriverFailed);

 // create and check buffers
 FBuffersCreated := CreateBuffers;
 if not FBuffersCreated
  then raise Exception.Create(RStrASIONoBuffersCreated);

 // eventually reactivate
 Active := OldActive and FBuffersCreated;
end;

procedure TCustomASIOHostBasic.CloseDriver;
begin
 // release driver
 if assigned(FDriver) then
 begin
  try
   if FBuffersCreated then DisposeBuffers;
  except
  end;
  FDriver := nil;
 end;

 // reset some default values in case another driver is querried
 FInputLatency := 0;
 FOutputLatency := 0;
 FInputChannelCount := 0;
 FOutputChannelCount := 0;
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
    if assigned(FDriver)
     then FDriver.GetLatencies(FInputLatency, FOutputLatency);
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
  then FOnUpdateSamplePos(Self, ASIOSamplesToInt64(Samples));
end;

procedure TCustomASIOHostBasic.BufferSwitch(Index: Integer);
begin
 FillChar(ASIOTime.FBufferTime, SizeOf(TASIOTime), 0);
 // get the time stamp of the buffer, not necessary if no
 // synchronization to other media is required
 if FDriver.GetSamplePosition(ASIOTime.FBufferTime.TimeInfo.samplePosition,
   ASIOTime.FBufferTime.TimeInfo.SystemTime) = ASE_OK then
   ASIOTime.Flags := ASIOTime.Flags + [atSystemTimeValid,atSamplePositionValid];

 BufferSwitchTimeInfo(Index, ASIOTime.FBufferTime);
end;

procedure TCustomASIOHostBasic.BufferSwitchTimeInfo(Index: Integer;
 const params: TASIOTime);
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := params.TimeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.TimeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);

 if assigned(FOnBufferSwitch) then FOnBufferSwitch(Self,@(FInputBuffer^), Index);
 FDriver.OutputReady;
end;

procedure TCustomASIOHostBasic.SetSampleRate(Value: Double);
begin
 // check for a valid samplerate
 Value := abs(Value);
 if (Value = 0) or (Value > 1048575)
  then Value := 44100;

 // check if samplerate is supported 
 if assigned(FDriver) then
  if FDriver.CanSampleRate(Value) <> ASE_OK
   then Exit;

 if FSampleRate <> Value then
  begin
   if assigned(FDriver) then
    if FDriver.SetSampleRate(Value) = ASE_OK then
     begin
      FSampleRate := Value;
      ASIOTime.SampleRate := FSampleRate;
     end
   else
    begin
     FSampleRate := Value;
     ASIOTime.SampleRate := FSampleRate;
    end;
  end;
end;

procedure TCustomASIOHostBasic.SetActive(Value: Boolean);
begin
 // make sure a driver is assigned and something changed
 if (FDriver = nil) then Value := False;
 if FActive = Value then exit;

 if Value = True then
  begin
   FActive := (FDriver.Start = ASE_OK);
   if FActive = False then FDriver.Stop;
  end
 else
  begin
   FActive := False;
   FDriver.Stop;
   if FBuffersCreated
    then ClearBuffers;
  end;
end;

procedure TCustomASIOHostBasic.ClearBuffers;
var
  Buffer     : PASIOBufferInfo;
  Channel    : Integer;
  SampleSize : Word;
begin
 try
  // clear output buffer
  Buffer := FOutputBuffer;
  if assigned(Buffer) then
   for Channel := 0 to FOutputChannelCount - 1 do
    with FOutputChannelInfos[Channel] do
     begin
      // determine sample size
      if SampleType in [ASIOSTInt16MSB, ASIOSTInt16LSB]     then SampleSize := SizeOf(Word) else
      if SampleType in [ASIOSTInt24MSB, ASIOSTInt24LSB]     then SampleSize := 3 else
      if SampleType in [ASIOSTFloat32LSB, ASIOSTFloat32MSB] then SampleSize := SizeOf(Single) else
      if SampleType in [ASIOSTFloat64LSB, ASIOSTFloat64MSB] then SampleSize := SizeOf(Double)
       else SampleSize := SizeOf(Integer);

      // finally clear buffer
      assert(assigned(Buffer));
      with Buffer^ do
       begin
        if assigned(Buffers[0]) then FillChar(Buffers[0]^, FBufferSize * SampleSize, 0);
        if assigned(Buffers[1]) then FillChar(Buffers[1]^, FBufferSize * SampleSize, 0);
       end;
      Inc(Buffer);
     end;

  // clear input buffer
  Buffer := FInputBuffer;
  if assigned(Buffer) then
   for Channel := 0 to FInputChannelCount - 1 do
    with FInputChannelInfos[Channel] do
     begin
      // determine sample size
      if SampleType in [ASIOSTInt16MSB, ASIOSTInt16LSB]     then SampleSize := SizeOf(Word) else
      if SampleType in [ASIOSTInt24MSB, ASIOSTInt24LSB]     then SampleSize := 3 else
      if SampleType in [ASIOSTFloat32LSB, ASIOSTFloat32MSB] then SampleSize := SizeOf(Single) else
      if SampleType in [ASIOSTFloat64LSB, ASIOSTFloat64MSB] then SampleSize := SizeOf(Double)
       else SampleSize := SizeOf(Integer);

      // finally clear buffer
      assert(assigned(Buffer));
      with Buffer^ do
       begin
        if assigned(Buffers[0]) then FillChar(Buffers[0]^, FBufferSize * SampleSize, 0);
        if assigned(Buffers[1]) then FillChar(Buffers[1]^, FBufferSize * SampleSize, 0);
       end;
      Inc(Buffer);
     end;
 except
 end;
end;

function TCustomASIOHostBasic.GetNumDrivers: Integer;
begin
 Result := length(FAsioDriverList);
end;

function TCustomASIOHostBasic.CanSampleRate(sampleRate: TASIOSampleRate): TASIOError;
begin
 if assigned(FDriver)
  then Result := FDriver.CanSampleRate(SampleRate)
  else Result := ASE_NotPresent;
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

function TCustomASIOHostBasic.GetInputChannelInfo(Index: Integer): TASIOChannelInfo;
begin
 if (Index < 0) or (Index >= FInputChannelCount)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Result := FInputChannelInfos[Index];
end;

function TCustomASIOHostBasic.GetOutputChannelInfo(Index: Integer): TASIOChannelInfo;
begin
 if (Index < 0) or (Index >= FOutputChannelCount)
  then raise Exception.CreateFmt(RCStrIndexOutOfBounds, [Index]);

 Result := FOutputChannelInfos[Index];
end;

function TCustomASIOHostBasic.GetInputMeter(Channel: Integer): Integer;
var
  ACC : TASIOChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdInputMeter in FASIOCanDos) then
  begin
   Result := -1;
   Exit;
  end;

 ACC.isInput := 1; ACC.Channel := Channel;
 FDriver.Future(kAsioGetInputMeter,@ACC);
 Result := ACC.meter;
end;

function TCustomASIOHostBasic.GetOutputMeter(Channel: Integer): Integer;
var
  ACC : TASIOChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdOutputMeter in FASIOCanDos) then
  begin
   Result := -1;
   Exit;
  end;

 if FDriver = nil then
 ACC.isInput := 0; ACC.Channel := Channel;
 FDriver.Future(kAsioGetOutputMeter, @ACC);
 Result := ACC.meter;
end;

procedure TCustomASIOHostBasic.SetInputGain(Channel:Integer; Gain: Integer);
var
  ACC : TASIOChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdInputGain in FASIOCanDos)
  then Exit;

 ACC.IsInput := 1;
 ACC.Channel := Channel;
 ACC.Gain := Gain;
 FDriver.Future(kAsioSetInputGain, @ACC);
end;

procedure TCustomASIOHostBasic.SetOutputGain(Channel:Integer; Gain: Integer);
var
  ACC : TASIOChannelControls;
begin
 // check if command can be transmitted
 if (FDriver = nil) and not (acdOutputGain in FASIOCanDos)
  then Exit;

 ACC.isInput := 0; ACC.Channel := Channel; ACC.Gain := Gain;
 FDriver.Future(kAsioSetOutputGain, @ACC);
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////
/////////////////////////////// TCustomASIOHost ////////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomASIOHost implementation'} {$ENDIF}

constructor TCustomASIOHost.Create(AOwner: TComponent);
begin
  FClipPrevent          := ClipDigital;
  FConvertOptimizations := [coSSE, co3DNow];
  FOutputDither         := odNone;
  FInputMonitor         := False;
  FConvertMethod        := cmNone;

  {$IFDEF ASIOMixer} FASIOMixer := TFmASIOMixer.Create(nil); {$ENDIF}
  inherited;
end;

destructor TCustomASIOHost.Destroy;
var
  Channel : Integer;
begin
 SetLength(FOutputVolume, 0);

 // dispose single input buffers
 for Channel := 0 to Length(FSingleInBuffer) - 1
  do Dispose(FSingleInBuffer[Channel]);

 // dispose single output buffers
 for Channel := 0 to Length(FSingleOutBuffer) - 1
  do Dispose(FSingleOutBuffer[Channel]);

 // dispose double input buffers
 for Channel := 0 to Length(FDoubleInBuffer) - 1
  do Dispose(FDoubleInBuffer[Channel]);

 // dispose double output buffers
 for Channel := 0 to Length(FDoubleOutBuffer) - 1
  do Dispose(FDoubleOutBuffer[Channel]);

 {$IFDEF ASIOMixer} FASIOMixer.Free; {$ENDIF}
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TCustomASIOHost.SetOnBufferSwitch32(const Value: TBufferSwitchEvent32);
begin
 FOnBufferSwitch32 := Value;
 OnBufferSwitchChanged;
end;

procedure TCustomASIOHost.SetOnBufferSwitch64(const Value: TBufferSwitchEvent64);
begin
 FOnBufferSwitch64 := Value;
 OnBufferSwitchChanged;
end;

procedure TCustomASIOHost.OnBufferSwitchChanged;
begin
 if assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomASIOHost.SetConvertMethod(const Value: TConvertMethod);
begin
 if ConvertMethod <> Value then
  begin
   FConvertMethod := Value;
   ConvertMethodChanged;
  end;
end;

procedure TCustomASIOHost.ConvertMethodChanged;
begin
 CreateFloatBuffers;
end;

procedure TCustomASIOHost.SetConvertOptimizations(const Value: TConvertOptimizations);
begin
 if FConvertOptimizations <> Value then
  begin
   FConvertOptimizations := Value;
   ConvertOptimizationsChanged;
  end;
end;

procedure TCustomASIOHost.ConvertOptimizationsChanged;
begin
 Use_FPU;
 case ProcessorType of
  ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
  pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
 end;
end;

procedure TCustomASIOHost.SetASIOGenerator(const Value: TASIOGenerator);
begin
 if Value <> FASIOGenerator then
  begin
   FASIOGenerator := Value;
   ASIOGeneratorChanged;
  end;
end;

procedure TCustomASIOHost.ASIOGeneratorChanged;
begin
 if Assigned(FASIOGenerator) then
  begin
   FASIOGenerator.BlockSize := FBufferSize;
   FASIOGenerator.SampleRate := FSampleRate;
  end;
end;

procedure TCustomASIOHost.SetPreventClipping(Value : TPreventClipping);
begin
 if FPreventClipping <> Value then
  begin
   FPreventClipping := Value;
   PreventClippingChanged;
  end;
end;

procedure TCustomASIOHost.PreventClippingChanged;
begin
 case FPreventClipping of
  pcDigital: FClipPrevent := ClipDigital;
  pcAnalog: FClipPrevent := ClipAnalog;
 end;
end;

procedure TCustomASIOHost.DetermineBuffersize;
begin
 inherited;
 if Assigned(FASIOGenerator)
  then FASIOGenerator.BlockSize := FBufferSize;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHost.VolumeChange(Sender: TObject);
begin
 Assert(Sender is TFrChannelStrip);
 with TFrChannelStrip(Sender) do
  begin
   FOutputVolume[Channel] := Volume;
   if Mute then FOutputVolume[Channel] := 0;
  end;
end;

procedure TCustomASIOHost.SetupMixer;
var
  Channel: Integer;
begin
 with FASIOMixer do
  begin
   for Channel := 0 to Length(ChannelsStrips) - 1
    do FreeAndNil(ChannelsStrips[Channel]);
   SetLength(ChannelsStrips, FOutputChannels);
   for Channel := FOutputChannels - 1 downto 0 do
    begin
     ChannelsStrips[Channel] := TFrChannelStrip.Create(FASIOMixer);
     with ChannelsStrips[Channel] do
      begin
       Width := 44;
       Name := 'ChannelStrip' + IntToStr(Channel);
       Parent := FASIOMixer.MixerPanel;
       Align := alLeft;
       OnVolumeChange := VolumeChange;
       OnMuteChange := VolumeChange;
       Channel := FOutputChannels - 1 - Channel;
      end;
    end;
   if FOutputChannels > 0 then
    begin
     ClientHeight := 20 + ChannelsStrips[0].Height;
     ClientWidth := 20 + FOutputChannels * ChannelsStrips[0].Width;
    end;
  end;  
end;
{$ENDIF ASIOMixer}

function TCustomASIOHost.CreateBuffers: Boolean;
var
  Channel : Integer;
begin
 Result := inherited CreateBuffers;

 if Result then
  begin
   SetLength(FOutputVolume, FOutputChannelCount);
   for Channel := 0 to FOutputChannelCount - 1
    do FOutputVolume[Channel] := 1;
   {$IFDEF ASIOMixer} SetupMixer; {$ENDIF}

   CreateFloatBuffers;
  end;
end;

procedure TCustomASIOHost.CreateFloatBuffers;
var
  Channel : Integer;
begin
 if FBufferSize > 0 then
  case FConvertMethod of
   cm32 :
    begin
     // input buffers
     SetLength(FSingleInBuffer, FInputChannelCount);
     for Channel := 0 to Length(FSingleInBuffer) - 1 do
      begin
       ReallocMem(FSingleInBuffer[Channel], FBufferSize * SizeOf(Single));
       FillChar(FSingleInBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
      end;

     // output buffers
     SetLength(FSingleOutBuffer, FOutputChannelCount);
     for Channel := 0 to Length(FSingleOutBuffer) - 1 do
      begin
       ReallocMem(FSingleOutBuffer[Channel], FBufferSize * SizeOf(Single));
       FillChar(FSingleOutBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
      end;

     // dispose unused input buffers
     for Channel := 0 to Length(FDoubleInBuffer) - 1
      do Dispose(FDoubleInBuffer[Channel]);
     SetLength(FDoubleInBuffer, 0);

     // dispose unused output buffers
     for Channel := 0 to Length(FDoubleOutBuffer) - 1
      do Dispose(FDoubleOutBuffer[Channel]);
     SetLength(FDoubleOutBuffer, 0);
    end;
   cm64 :
    begin
     // input buffers
     SetLength(FDoubleInBuffer, FInputChannelCount);
     for Channel := 0 to FInputChannelCount - 1 do
      begin
       ReallocMem(FDoubleInBuffer[Channel], FBufferSize * SizeOf(Double));
       FillChar(FDoubleInBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
      end;

     // output buffers
     SetLength(FDoubleOutBuffer, FOutputChannelCount);
     for Channel := 0 to FOutputChannelCount - 1 do
      begin
       ReallocMem(FDoubleOutBuffer[Channel], FBufferSize * SizeOf(Double));
       FillChar(FDoubleOutBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
      end;

     // dispose unused input buffers
     for Channel := 0 to Length(FSingleInBuffer) - 1
      do Dispose(FSingleInBuffer[Channel]);
     SetLength(FSingleInBuffer, 0);

     // dispose unused output buffers
     for Channel := 0 to Length(FSingleOutBuffer) - 1
      do Dispose(FSingleOutBuffer[Channel]);
     SetLength(FSingleOutBuffer, 0);
    end;
   cmNone :
    begin
     // dispose unused single input buffers
     for Channel := 0 to Length(FSingleInBuffer) - 1
      do Dispose(FSingleInBuffer[Channel]);
     SetLength(FSingleInBuffer, 0);

     // dispose unused double input buffers
     for Channel := 0 to Length(FDoubleInBuffer) - 1
      do Dispose(FDoubleInBuffer[Channel]);
     SetLength(FDoubleInBuffer, 0);

     // dispose unused single output buffers
     for Channel := 0 to Length(FSingleOutBuffer) - 1
      do Dispose(FSingleOutBuffer[Channel]);
     SetLength(FSingleOutBuffer, 0);

     // dispose unused double output buffers
     for Channel := 0 to Length(FDoubleOutBuffer) - 1
      do Dispose(FDoubleOutBuffer[Channel]);
     SetLength(FDoubleOutBuffer, 0);
    end;
  end;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHost.Mixer;
begin
 FASIOMixer.Show;
end;
{$ENDIF}

procedure TCustomASIOHost.BufferSwitchTimeInfo(Index: Integer; const Params: TASIOTime);
var
  Sample, Channel : Integer;
  CurrentBuffer   : PASIOBufferInfo;
  ChannelData     : Pointer;
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := Params.TimeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := Params.TimeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);

 CurrentBuffer := FInputBuffer;

 // native processing
 if assigned(FOnBufferSwitchNative)
  then FOnBufferSwitchNative(Self, @(FInputBuffer^), Index);

 if FConvertMethod = cm64 then
  begin
   // 64bit float processing
   case FInBufferPreFill of
      bpfZero : for Channel := 0 to FInputChannelCount - 1
                 do FillChar(FDoubleInBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
     bpfNoise : for Channel := 0 to FInputChannelCount - 1 do
                 for Sample := 0 to FBufferSize - 1
                  do FDoubleInBuffer[Channel, Sample] := 2 * Random - 1;
    bpfCustom : if Assigned(FASIOGenerator) then FASIOGenerator.ProcessBuffer64(FDoubleInBuffer, False);
    else
     for Channel := 0 to FInputChannelCount - 1 do
      begin
       ChannelData := CurrentBuffer^.Buffers[Index];
       assert(assigned(FDoubleInBuffer[Channel]));
       assert(Length(FInConverters) > Channel);
       assert(assigned(FInConverters[Channel].ic64));
       if Assigned(ChannelData)
        then FInConverters[Channel].ic64(ChannelData, @FDoubleInBuffer[Channel, 0], FBufferSize);
       inc(CurrentBuffer);
      end;
   end;

   if FPreventClipping <> pcNone then
    for Channel := 0 to FInputChannelCount - 1
     do FClipPrevent.cb64(@FDoubleInBuffer[Channel, 0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for Channel := 0 to FOutputChannelCount - 1
               do FillChar(FDoubleOutBuffer[Channel, 0], FBufferSize * SizeOf(Double), 0);
    bpfNoise: for Channel := 0 to FOutputChannelCount - 1 do
               for Sample := 0 to FBufferSize - 1
                do FDoubleOutBuffer[Channel, Sample] := 2 * Random - 1;
    bpfCustom: if Assigned(FASIOGenerator)
                then FASIOGenerator.ProcessBuffer64(FDoubleOutBuffer, True);
   end;

   if FInputMonitor then
    for Channel := 0 to min(FInputChannelCount, FOutputChannelCount) - 1
     do Move(FDoubleInBuffer[Channel, 0],
             FDoubleOutBuffer[Channel, 0],
             FBufferSize * SizeOf(Double));

   FOnBufferSwitch64(Self, FDoubleInBuffer, FDoubleOutBuffer);

   if FPreventClipping <> pcNone then
    for Channel := 0 to FOutputChannelCount - 1
     do FClipPrevent.cb64(@FDoubleOutBuffer[Channel, 0] ,FBufferSize);

   CurrentBuffer := FOutputBuffer;
   for Channel := 0 to FOutputChannelCount - 1 do
    begin
     ChannelData := CurrentBuffer^.Buffers[Index];
     if assigned(ChannelData)
      then FOutConverters[Channel].oc64(@FDoubleOutBuffer[Channel, 0], ChannelData, FBufferSize);
     inc(CurrentBuffer);
    end;
  end else
 if FConvertMethod = cm32 then
  begin
   // 32bit float processing
   case FInBufferPreFill of
      bpfZero : for Channel := 0 to FInputChannelCount - 1
                 do FillChar(FSingleInBuffer[Channel,0], FBufferSize * SizeOf(Single), 0);
     bpfNoise : for Channel := 0 to FInputChannelCount - 1 do
                 for Sample := 0 to FBufferSize - 1 do FSingleInBuffer[Channel, Sample] := 2 * Random - 1;
    bpfCustom : if Assigned(FASIOGenerator) then FASIOGenerator.ProcessBuffer32(FSingleInBuffer, False);
    else
     begin
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        ChannelData := CurrentBuffer^.Buffers[Index];
        assert(ChannelData <> nil);
        assert(assigned(FSingleInBuffer[Channel]));
        assert(Length(FInConverters) > Channel);
        assert(assigned(FInConverters[Channel].ic32));
        if Assigned(ChannelData)
         then FInConverters[Channel].ic32(ChannelData, @FSingleInBuffer[Channel, 0], FBufferSize);
        inc(CurrentBuffer);
       end;
     end;
   end;

   if FPreventClipping <> pcNone then
    for Channel := 0 to FInputChannelCount - 1
     do FClipPrevent.cb32(@FSingleInBuffer[Channel, 0], FBufferSize);

   case FOutBufferPreFill of
    bpfZero : for Channel := 0 to FOutputChannelCount - 1 do
               begin
                assert(FSingleOutBuffer[Channel] <> nil);
                FillChar(FSingleOutBuffer[Channel, 0], FBufferSize * SizeOf(Single), 0);
               end;
    bpfNoise: for Channel := 0 to FOutputChannelCount - 1 do
               begin
                assert(FSingleOutBuffer[Channel] <> nil);
                for Sample := 0 to FBufferSize - 1
                 do FSingleOutBuffer[Channel, Sample] := 2 * Random - 1;
               end;
    bpfCustom: if Assigned(FASIOGenerator)
                then FASIOGenerator.ProcessBuffer32(FSingleOutBuffer, True);
   end;

   if FInputMonitor then
    for Channel := 0 to min(FInputChannelCount, FOutputChannelCount) - 1
     do Move(FSingleInBuffer[Channel, 0],
             FSingleOutBuffer[Channel, 0],
             FBufferSize * SizeOf(Single));

   if Assigned(FOnBufferSwitch32)
    then FOnBufferSwitch32(Self, FSingleInBuffer, FSingleOutBuffer);

   if FPreventClipping <> pcNone then
    for Channel := 0 to FOutputChannelCount - 1
     do FClipPrevent.cb32(@FSingleOutBuffer[Channel, 0] ,FBufferSize);

   CurrentBuffer := FOutputBuffer;
   for Channel := 0 to FOutputChannelCount - 1 do
    begin
     ChannelData := CurrentBuffer^.Buffers[Index];
     if assigned(ChannelData)
      then FOutConverters[Channel].oc32(@FSingleOutBuffer[Channel, 0], ChannelData, FBufferSize);
     inc(CurrentBuffer);
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
       Use_FPU;
       case ProcessorType of
        ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
        pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
       end;
      end;
    odUDF  : Use_FPU_UDF;
    odTDF  : Use_FPU_TDF;
   end;
  end;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

////////////////////////////////////////////////////////////////////////////////
////////////////////////// TCustomASIOHostAudioData ////////////////////////////
////////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$region 'TCustomASIOHostAudioData implementation'} {$ENDIF}

constructor TCustomASIOHostAudioData.Create(AOwner: TComponent);
begin
  FClipPrevent          := ClipDigital;
  FConvertOptimizations := [coSSE, co3DNow];
  FConvertMethod        := cmNone;
  FOutputDither         := odNone;

  {$IFDEF ASIOMixer} FASIOMixer := TFmASIOMixer.Create(nil); {$ENDIF}
  inherited;
end;

destructor TCustomASIOHostAudioData.Destroy;
begin
 if assigned(FAudioDataInput)  then FreeAndNil(FAudioDataInput);
 if assigned(FAudioDataOutput) then FreeAndNil(FAudioDataOutput);

 {$IFDEF ASIOMixer} FreeAndNil(FASIOMixer); {$ENDIF}
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TCustomASIOHostAudioData.SetOnBufferSwitch32(const Value: TBufferSwitchAudioData32Event);
begin
 FOnBufferSwitch32 := Value;
 if assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomASIOHostAudioData.SetOnBufferSwitch64(const Value: TBufferSwitchAudioData64Event);
begin
 FOnBufferSwitch64 := Value;
 if assigned(FOnBufferSwitch64) then ConvertMethod := cm64 else
 if assigned(FOnBufferSwitch32) then ConvertMethod := cm32
  else ConvertMethod := cmNone;
end;

procedure TCustomASIOHostAudioData.ConvertMethodChanged;
var
  OldIn, OldOut  : TCustomAudioDataCollection;
begin
 OldIn  := FAudioDataInput;
 OldOut := FAudioDataOutput;
 case FConvertMethod of
  cm32 : begin
          FAudioDataInput  := TASIOAudioDataCollection32.Create(Self, InputChannelCount, BufferSize);
          FAudioDataOutput := TASIOAudioDataCollection32.Create(Self, OutputChannelCount, BufferSize);
         end;
  cm64 : begin
          FAudioDataInput  := TASIOAudioDataCollection64.Create(Self, InputChannelCount, BufferSize);
          FAudioDataOutput := TASIOAudioDataCollection64.Create(Self, OutputChannelCount, BufferSize);
         end;
 end;
 if assigned(OldIn)  then FreeAndNil(OldIn);
 if assigned(OldOut) then FreeAndNil(OldOut);
end;

procedure TCustomASIOHostAudioData.SetConvertMethod(
  const Value: TConvertMethod);
begin
 if FConvertMethod <> Value then
  begin
   FConvertMethod := Value;
   ConvertMethodChanged;
  end;
end;

procedure TCustomASIOHostAudioData.SetConvertOptimizations(const Value: TConvertOptimizations);
begin
 if FConvertOptimizations <> Value then
  begin
   FConvertOptimizations := Value;
   ConvertOptimizationsChanged;
  end;
end;

procedure TCustomASIOHostAudioData.ConvertOptimizationsChanged;
begin
 Use_FPU;
 case ProcessorType of
  ptSSE: if coSSE in FConvertOptimizations then Use_SSE;
  pt3DNow: if co3DNow in FConvertOptimizations then Use_3DNow;
 end;
end;

procedure TCustomASIOHostAudioData.SetPreventClipping(v : TPreventClipping);
begin
 FPreventClipping := v;
 case FPreventClipping of
  pcDigital: FClipPrevent := ClipDigital;
   pcAnalog: FClipPrevent := ClipAnalog;
 end;
end;

procedure TCustomASIOHostAudioData.DetermineBuffersize;
begin
 inherited;
 if assigned(FAudioDataInput) then
  with FAudioDataInput do
   begin
    ChannelCount := InputChannelCount;
    SampleFrames := BufferSize;
   end;
 if assigned(FAudioDataOutput) then
  with FAudioDataOutput do
   begin
    ChannelCount := InputChannelCount;
    SampleFrames := BufferSize;
   end;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHostAudioData.VolumeChange(Sender: TObject);
begin
 assert(Sender is TFrChannelStrip);
 with TFrChannelStrip(Sender) do
  begin
   FOutputVolume[Channel] := Volume;
   if Mute then FOutputVolume[Channel] := 0;
  end;
end;

procedure TCustomASIOHostAudioData.SetupMixer;
var
  Channel: Integer;
begin
 with FASIOMixer do
  begin
   for Channel := 0 to Length(ChannelsStrips) - 1
    do FreeAndNil(ChannelsStrips[Channel]);
   SetLength(ChannelsStrips, FOutputChannels);
   for Channel := FOutputChannels - 1 downto 0 do
    begin
     ChannelsStrips[Channel] := TFrChannelStrip.Create(FASIOMixer);
     with ChannelsStrips[Channel] do
      begin
       Width := 44;
       Name := 'ChannelStrip' + IntToStr(Channel);
       Parent := FASIOMixer.MixerPanel;
       Align := alLeft;
       OnVolumeChange := VolumeChange;
       OnMuteChange := VolumeChange;
       Channel := FOutputChannels - 1 - Channel;
      end; 
    end;
   if FOutputChannels > 0 then
    begin
     ClientHeight := 20 + ChannelsStrips[0].Height;
     ClientWidth := 20 + FOutputChannels * ChannelsStrips[0].Width;
    end;
  end;  
end;
{$ENDIF ASIOMixer}

function TCustomASIOHostAudioData.CreateBuffers: Boolean;
var
  Channel : Integer;
begin
 Result := inherited CreateBuffers;

 if Result then
  begin
   SetLength(FOutputVolume, FOutputChannelCount);
   for Channel := 0 to FOutputChannelCount - 1 do FOutputVolume[Channel] := 1;
   {$IFDEF ASIOMixer} SetupMixer; {$ENDIF}

   if assigned(FAudioDataInput)
    then FAudioDataInput.ChannelCount := FInputChannelCount;
   if assigned(FAudioDataOutput)
    then FAudioDataOutput.ChannelCount := FOutputChannelCount;
  end;
end;

{$IFDEF ASIOMixer}
procedure TCustomASIOHostAudioData.Mixer;
begin
 FASIOMixer.Show;
end;
{$ENDIF}

procedure TCustomASIOHostAudioData.BufferSwitchTimeInfo(Index: Integer; const params: TASIOTime);
var
  Channel        : Integer;
  CurrentBuffer  : PASIOBufferInfo;
  PChannelArray  : Pointer;
begin
 if FDriver = nil then exit;
 PMUpdSamplePos.wParam := params.TimeInfo.samplePosition.hi;
 PMUpdSamplePos.LParam := params.TimeInfo.samplePosition.lo;
 Dispatch(PMUpdSamplePos);
 CurrentBuffer := FInputBuffer;

 if FConvertMethod = cm64 then
  begin
   // 64bit float processing

   // process input
   with TASIOAudioDataCollection64(FAudioDataInput) do
    case FInBufferPreFill of
      bpfZero : FAudioDataInput.Clear;
     bpfNoise : FAudioDataInput.GenerateWhiteNoise(1);
     else
      // convert soundcard dependent format to float data
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        PChannelArray := CurrentBuffer^.buffers[Index];
        if Assigned(PChannelArray)
         then FInConverters[Channel].ic64(PChannelArray,
                PDouble(TASIOAudioDataCollection64(FAudioDataInput).ChannelDataPointerList[Channel]),
                FBufferSize);
        Inc(CurrentBuffer);
       end;
    end;

   // process output
   case FOutBufferPreFill of
    bpfZero : FAudioDataOutput.Clear;
    bpfNoise: FAudioDataOutput.GenerateWhiteNoise(1);
   end;

   // call event to send in and get output data
   FOnBufferSwitch64(Self,
     TASIOAudioDataCollection64(FAudioDataInput),
     TASIOAudioDataCollection64(FAudioDataOutput));

   with TASIOAudioDataCollection64(FAudioDataOutput) do
    begin
     // eventually clip data to avoid ugly artifacts caused by the soundcard
     if FPreventClipping <> pcNone then
      for Channel := 0 to FOutputChannelCount - 1
       do FClipPrevent.cb64(PDouble(ChannelDataPointerList[Channel]) ,FBufferSize);

     // convert float data to soundcard dependent format
     CurrentBuffer := FOutputBuffer;
     for Channel := 0 to FOutputChannelCount - 1 do
      begin
       PChannelArray := CurrentBuffer^.buffers[Index];
       if assigned(PChannelArray)
        then FOutConverters[Channel].oc64(PDouble(ChannelDataPointerList[Channel]),
               PChannelArray, FBufferSize);
       inc(CurrentBuffer);
      end;
    end;
  end
 else
  begin
   // 32bit float processing

   // process input
   with TASIOAudioDataCollection32(FAudioDataInput) do
    case FInBufferPreFill of
      bpfZero : FAudioDataInput.Clear;
     bpfNoise : FAudioDataInput.GenerateWhiteNoise(1);
     else
      // convert soundcard dependent format to float data
      for Channel := 0 to FInputChannelCount - 1 do
       begin
        PChannelArray := CurrentBuffer^.buffers[Index];
        if Assigned(PChannelArray)
         then FInConverters[Channel].ic32(PChannelArray,
                PSingle(ChannelDataPointer[Channel]),
                FBufferSize);
        Inc(CurrentBuffer);
       end;
    end;

   // process output
   case FOutBufferPreFill of
    bpfZero : FAudioDataOutput.Clear;
    bpfNoise: FAudioDataOutput.GenerateWhiteNoise(1);
   end;

   // call event to send in and get output data
   FOnBufferSwitch32(Self,
     TASIOAudioDataCollection32(FAudioDataInput),
     TASIOAudioDataCollection32(FAudioDataOutput));

   with TASIOAudioDataCollection32(FAudioDataOutput) do
    begin
     // eventually clip data to avoid ugly artifacts caused by the soundcard
     if FPreventClipping <> pcNone then
      for Channel := 0 to FOutputChannelCount - 1
       do FClipPrevent.cb32(PSingle(ChannelDataPointer[Channel]) ,FBufferSize);

     // convert float data to soundcard dependent format
     CurrentBuffer := FOutputBuffer;
     for Channel := 0 to FOutputChannelCount - 1 do
      begin
       PChannelArray := CurrentBuffer^.buffers[Index];
       if assigned(PChannelArray)
        then FOutConverters[Channel].oc32(PSingle(ChannelDataPointer[Channel]),
               PChannelArray, FBufferSize);
       inc(CurrentBuffer);
      end;
    end;

   end;
 FDriver.OutputReady;
end;

{$IFDEF DELPHI10_UP} {$endregion} {$ENDIF}

initialization
 PMUpdSamplePos.Msg := PM_UpdateSamplePos;
 PMBufSwitch.Msg := PM_BufferSwitch;
 PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
 PMReset.Msg := PM_Reset;

end.
