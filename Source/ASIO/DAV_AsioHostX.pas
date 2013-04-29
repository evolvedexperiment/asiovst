{******************************************************************************}
{                                                                              }
{  Version: MPL 1.1 or LGPL 2.1 with linking exception                         }
{                                                                              }
{  The contents of this file are subject to the Mozilla Public License         }
{  Version 1.1 (the "License"); you may not use this file except in            }
{  compliance with the License. You may obtain a copy of the License at        }
{  http://www.mozilla.org/MPL/                                                 }
{                                                                              }
{  Software distributed under the License is distributed on an "AS IS"         }
{  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the     }
{  License for the specific language governing rights and limitations under    }
{  the License.                                                                }
{                                                                              }
{  Alternatively, the contents of this file may be used under the terms of     }
{  the Free Pascal modified version of the GNU Lesser General Public           }
{  License Version 2.1 (the "FPC modified LGPL License"), in which case the    }
{  provisions of this license are applicable instead of those above.           }
{  Please see the file LICENSE.txt for additional information concerning       }
{  this license.                                                               }
{                                                                              }
{  The code is part of the Delphi ASIO & VST Project                           }
{                                                                              }
{  The initial developer of this code is Christian-W. Budde                    }
{                                                                              }
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2013          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

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
{$ELSE}Windows, Messages, {$ENDIF}
{$IFDEF OpenASIO} DAV_OpenAsio {$ELSE} DAV_BeroASIO {$ENDIF},
{$IFDEF ASIOMixer} Forms, ComCtrls, Graphics, StdCtrls, DAVASIOMixer, {$ENDIF}
{$IFDEF DELPHI5} Forms, DsgnIntf, {$ENDIF} SysUtils, Classes, Controls,
  DAV_ASIO, DAV_ASIOConvert, DAV_ASIOGenerator, DAV_Common, DAV_AudioData;

const
  // private message
  PM_ASIO = WM_User + 1652; // unique we hope
  // ASIO message(s), as wParam for PM_ASIO
  AM_ResetRequest = 0;
  AM_BufferSwitch = 1; // new buffer index in lParam
  AM_BufferSwitchTimeInfo = 2; // new buffer index in lParam
  // time passed in MainForm.BufferTime
  AM_LatencyChanged = 3;

  PM_UpdateSamplePos = PM_ASIO + 1; // sample pos in wParam (hi) and lParam (lo)

  PM_BufferSwitch = PM_ASIO + 2;
  PM_BufferSwitchTimeInfo = PM_ASIO + 3;
  PM_Reset = PM_ASIO + 4;

type
  TAsioDriverDesc = packed record
    id: TGUID; // TCLSID;
    name: array [0 .. 511] of AnsiChar;
    path: array [0 .. 511] of AnsiChar;
  end;

  PAsioDriverDesc = ^TAsioDriverDesc;
  TASIOBufferList = array [0 .. 0] of TASIOBufferInfo;
  PASIOBufferList = ^TASIOBufferList;

  TASIOSelectorSupport = (assEngineVersion, assResetRequest,
    assBufferSizeChange, assResyncRequest, assLatenciesChanged,
    assSupportsTimeInfo, assSupportsTimeCode, assSupportsInputMonitor);
  TASIOSelectorSupports = set of TASIOSelectorSupport;

  TAsioDriverList = array of TAsioDriverDesc;
  TASIOCanDo = (acdInputMonitor, acdTimeInfo, acdTimeCode, acdTransport,
    acdInputGain, acdInputMeter, acdOutputGain, acdOutputMeter);
  TASIOCanDos = set of TASIOCanDo;
  TASIOOutputDither = (odNone, odUDF, odTDF);

  TConvertMethod = (cmNone, cm32, cm64);
  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TSamplePositionUpdateEvent = procedure(Sender: TObject; SamplePosition: Int64)
    of object;
  TSample2Event = procedure(Sender: TObject; Sample: array of Single) of object;
  TBufferSwitchEvent32 = procedure(Sender: TObject;
    const InBuffer, OutBuffer: TDAVArrayOfSingleDynArray) of object;
  TBufferSwitchEvent64 = procedure(Sender: TObject;
    const InBuffer, OutBuffer: TDAVArrayOfDoubleDynArray) of object;
  TBufferSwitchEventNative = procedure(Sender: TObject;
    const BufferInfo: PASIOBufferList; const BufferIndex: Integer) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfCustom);

  TPreventClipping = (pcNone, pcDigital, pcAnalog);

  TInputMonitor = (imDisabled, imMono, imStereo, imAll);

{$IFDEF DELPHI10_UP} {$REGION 'TASIOTimeSub'} {$ENDIF}
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
    property Speed: Double index 0 read GetATdouble write SetATdouble;
    // absolute speed (1. = nominal)
    property SampleRate: Double Index 1 read GetATdouble write SetATdouble;
    property Flags: TATFlags read GetATflags Write SetATflags;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'TASIOTimeSub'} {$ENDIF}
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

{$IFDEF DELPHI10_UP} {$REGION 'TASIOHostBasic'} {$ENDIF}

  TCustomASIOHostBasic = class(TCustomAudioDevice)
  private
    Fmin, Fmax, Fpref, Fgran: Integer;
    FOnBufferSwitchNative: TBufferSwitchEventNative;
    function GetInputChannelInfo(Index: Integer): TASIOChannelInfo;
    function GetOutputChannelInfo(Index: Integer): TASIOChannelInfo;
    function GetOutConverter(ConverterType: TASIOSampleType): TOutConverter;
  protected
    FHandle: THandle;
    FHandleOwned: Boolean;
    FASIOTime: TASIOTimeSub;
    FBuffersCreated: Boolean;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FOnReset: TNotifyEvent;
    FOnDriverChanged: TNotifyEvent;
    FOnLatencyChanged: TNotifyEvent;
    FOnSampleRateChanged: TNotifyEvent;
    FOnBuffersCreate: TNotifyEvent;
    FOnBuffersDestroy: TNotifyEvent;
    FOnUpdateSamplePos: TSamplePositionUpdateEvent;
    FOnBufferSwitch: TBufferSwitchEventNative;
    FASIOCanDos: TASIOCanDos;
    FASIOdriverlist: TAsioDriverList;
    FCallbacks: TASIOCallbacks;
    FUnAlignedBuffer: PASIOBufferInfo;
    FSampleRate: Double;
    FInputBuffer: PASIOBufferInfo;
    FOutputBuffer: PASIOBufferInfo;
    FActive: Boolean;
    FDriverIndex: Integer;
    FDriverList: TStrings;
    FDriverName: String;
    FDriverVersion: Integer;
    FInputLatency: Integer;
    FOutputLatency: Integer;
    FInputChannelCount: Integer;
    FOutputChannelCount: Integer;
    FBufferSize: Cardinal;
    FInputChannelInfos: array of TASIOChannelInfo;
    FOutputChannelInfos: array of TASIOChannelInfo;
    FInConverters: array of TInConverter;
    FOutConverters: array of TOutConverter;
{$IFDEF OpenASIO}
    FDriver: IOpenAsio;
{$ELSE}
    FDriver: IBeroASIO;
{$ENDIF}
    FASIOSelectorSupport: TASIOSelectorSupports;
    procedure SetActive(Value: Boolean); virtual;
    procedure SetDriverIndex(Value: Integer); virtual;
    procedure SetDriverName(const s: String); virtual;
{$IFDEF FPC}
    procedure WndProc(var Msg: TLMessage);
    procedure PMASIO(var Message: TLMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TLMessage);
      message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TLMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TLMessage);
      message PM_BufferSwitchTimeInfo;
{$ELSE}
    procedure WndProc(var Msg: TMessage);
    procedure PMASIO(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage);
      message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage);
      message PM_BufferSwitchTimeInfo;
{$ENDIF}
    function CanInputGain: Boolean; virtual;
    function CanInputMeter: Boolean; virtual;
    function CanOutputGain: Boolean; virtual;
    function CanOutputMeter: Boolean; virtual;
    function CanTimeCode: Boolean; virtual;
    function CanTimeInfo: Boolean; virtual;
    function CanTransport: Boolean; virtual;
    function CreateBuffers: Boolean; virtual;
    function GetDriverList: TStrings;
    function GetInputMeter(Channel: Integer): Integer; virtual;
    function GetOutputMeter(Channel: Integer): Integer; virtual;
    function GetInConverter(ConverterType: TASIOSampleType): TInConverter;
    procedure BufferSwitch(Index: Integer); virtual;
    procedure BufferSwitchTimeInfo(Index: Integer;
      const params: TASIOTime); virtual;
    procedure DestroyBuffers; virtual;
    procedure ReadState(Reader: TReader); override;
    procedure SetupBuffersize; virtual;
    procedure SetupSampleRate;
    procedure SetASIOCanDos(const Value: TASIOCanDos); virtual;
    procedure SetSampleRate(const Value: Double); virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; virtual;
    function GetNumDrivers: Integer; virtual;
    procedure CloseDriver; virtual;
    procedure ControlPanel; virtual;
    procedure GetOutputGain(Channel, Gain: Integer); virtual;
    procedure OpenDriver; virtual;
    procedure Reset; virtual;
    procedure SetInputGain(Channel, Gain: Integer); virtual;

    property Active: Boolean read FActive write SetActive default False;
    property ASIOTime: TASIOTimeSub read FASIOTime Write FASIOTime;
    property BufferGranularity: Integer read Fgran stored False;
    property BufferMaximum: Integer read Fmax stored False;
    property BufferMinimum: Integer read Fmin stored False;
    property BufferPreferredSize: Integer read Fpref stored False;
    property BufferSize: Cardinal read FBufferSize stored False default 1;
    property CanDos: TASIOCanDos read FASIOCanDos write SetASIOCanDos;
    property DriverIndex: Integer read FDriverIndex Write SetDriverIndex
      default -1;
    property DriverList: TStrings read FDriverList;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: Integer read FDriverVersion;
    property InputChannelCount: Integer read FInputChannelCount stored False
      default 0;
    property InputChannelInfos[index: Integer]: TASIOChannelInfo
      read GetInputChannelInfo;
    property InputLatency: Integer read FInputLatency stored False default 0;
    property InputMeter[Channel: Integer]: Integer read GetInputMeter;
    property OnBuffersCreate: TNotifyEvent read FOnBuffersCreate
      write FOnBuffersCreate;
    property OnBuffersDestroy: TNotifyEvent read FOnBuffersDestroy
      write FOnBuffersDestroy;
    property OnBufferSwitch: TBufferSwitchEventNative read FOnBufferSwitchNative
      write FOnBufferSwitchNative;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged
      write FOnDriverChanged;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged
      write FOnLatencyChanged;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged
      write FOnSampleRateChanged;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent
      read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OutputChannelCount: Integer read FOutputChannelCount stored False
      default 0;
    property OutputChannelInfos[index: Integer]: TASIOChannelInfo
      read GetOutputChannelInfo;
    property OutputLatency: Integer read FOutputLatency stored False default 0;
    property OutputMeter[Channel: Integer]: Integer read GetOutputMeter;
    property SampleRate: Double read FSampleRate write SetSampleRate;
    property SelectorSupport: TASIOSelectorSupports read FASIOSelectorSupport
      write FASIOSelectorSupport;
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
    property OnBuffersDestroy;
    property OnBufferSwitch;
    property OnCreate;
    property OnDestroy;
    property OnDriverChanged;
    property OnLatencyChanged;
    property OnReset;
    property OnSampleRateChanged;
    property OnUpdateSamplePos;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'TASIOHostBasic'} {$ENDIF}

  TASIOAudioData32 = class(TAudioData32);
  TASIOAudioData64 = class(TAudioData64);

{$IFDEF DELPHI10_UP} {$REGION 'TASIOHost'} {$ENDIF}

  TCustomASIOHost = class(TCustomASIOHostBasic)
  private
    FPreventClipping: TPreventClipping;
    FInBufferPreFill: TBufferPreFill;
    FOutBufferPreFill: TBufferPreFill;
    FOnSample2Output: TSample2Event;
    FOnInput2Sample: TSample2Event;
    FOnBufferSwitch32: TBufferSwitchEvent32;
    FOnBufferSwitch64: TBufferSwitchEvent64;
    FOnBufferSwitchNative: TBufferSwitchEventNative;
    FInputChannelOffset: Word;
    FOutputChannelOffset: Word;
    FASIOGenerator: TASIOGenerator;
    FSingleInBuffer: TDAVArrayOfSingleDynArray;
    FSingleOutBuffer: TDAVArrayOfSingleDynArray;
    FDoubleInBuffer: TDAVArrayOfDoubleDynArray;
    FDoubleOutBuffer: TDAVArrayOfDoubleDynArray;
    FInputMonitor: TInputMonitor;
    FConvertOptimizations: TConvertOptimizations;
    FOutputVolume: TDAVSingleDynArray;
    FClipPrevent: TClipBuffer;
    FConvertMethod: TConvertMethod;
    FOutputDither: TASIOOutputDither;
{$IFDEF ASIOMixer}
    FASIOMixer: TFmASIOMixer;
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
    procedure SetConvertMethod(const Value: TConvertMethod);
  protected
    function CreateBuffers: Boolean; override;
    procedure BufferSwitchTimeInfo(Index: Integer;
      const params: TASIOTime); override;
    procedure SetupBuffersize; override;

    property ConvertMethod: TConvertMethod read FConvertMethod
      write SetConvertMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF ASIOMixer}
    procedure Mixer;
{$ENDIF}
    property ConvertOptimizations: TConvertOptimizations
      read FConvertOptimizations write SetConvertOptimizations;
    property CustomGenerator: TASIOGenerator read FASIOGenerator
      Write SetASIOGenerator;
    property InputChannelOffset: Word read FInputChannelOffset
      write SetInputChannelOffset default 0;
    property InputMonitor: TInputMonitor read FInputMonitor write FInputMonitor
      default imDisabled;
    property OnBufferSwitch32: TBufferSwitchEvent32 read FOnBufferSwitch32
      write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchEvent64 read FOnBufferSwitch64
      write SetOnBufferSwitch64;
    property OnBufferSwitchNative: TBufferSwitchEventNative
      read FOnBufferSwitchNative write FOnBufferSwitchNative;
    property OnInput2Sample: TSample2Event read FOnInput2Sample
      write FOnInput2Sample;
    property OnSample2Output: TSample2Event read FOnSample2Output
      write FOnSample2Output;
    property OutputChannelOffset: Word read FOutputChannelOffset
      write SetOutputChannelOffset default 0;
    property OutputDither: TASIOOutputDither read FOutputDither
      write SetOutputDither default odNone;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill
      write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill
      write FOutBufferPreFill default bpfNone;
    property PreventClipping: TPreventClipping read FPreventClipping
      write SetPreventClipping default pcNone;
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
    property InputChannelOffset;
    property InputLatency;
    property InputMonitor;
    property OutputChannelCount;
    property OutputChannelOffset;
    property OutputDither;
    property OutputLatency;
    property PreFillInBuffer;
    property PreFillOutBuffer;
    property PreventClipping;
    property SampleRate;
    property SelectorSupport;
    property OnBuffersCreate;
    property OnBuffersDestroy;
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
{$IFDEF DELPHI10_UP} {$ENDREGION 'TASIOHost'} {$ENDIF}

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

  TBufferSwitchAudioData32Event = procedure(Sender: TObject;
    const InBuffer, OutBuffer: TASIOAudioDataCollection32) of object;
  TBufferSwitchAudioData64Event = procedure(Sender: TObject;
    const InBuffer, OutBuffer: TASIOAudioDataCollection64) of object;

{$IFDEF DELPHI10_UP} {$REGION 'TASIOHostAudioData'} {$ENDIF}

  TCustomASIOHostAudioData = class(TCustomASIOHostBasic)
  private
    FPreventClipping: TPreventClipping;
    FInBufferPreFill: TBufferPreFill;
    FOutBufferPreFill: TBufferPreFill;

    FOnBufferSwitch32: TBufferSwitchAudioData32Event;
    FOnBufferSwitch64: TBufferSwitchAudioData64Event;

    FConvertOptimizations: TConvertOptimizations;
    FOutputVolume: TDAVSingleDynArray;
    FClipPrevent: TClipBuffer;
    FConvertMethod: TConvertMethod;
    FOutputDither: TASIOOutputDither;

    FAudioDataInput: TCustomAudioDataCollection;
    FAudioDataOutput: TCustomAudioDataCollection;

{$IFDEF ASIOMixer}
    FASIOMixer: TFmASIOMixer;
{$ENDIF}
    procedure SetConvertOptimizations(const co: TConvertOptimizations);
    procedure SetPreventClipping(v: TPreventClipping);
{$IFDEF ASIOMixer}
    procedure SetupMixer;
    procedure VolumeChange(Sender: TObject);
{$ENDIF}
    procedure SetOnBufferSwitch32(const Value: TBufferSwitchAudioData32Event);
    procedure SetOnBufferSwitch64(const Value: TBufferSwitchAudioData64Event);
    procedure SetConvertMethod(const Value: TConvertMethod);
  protected
    function CreateBuffers: Boolean; override;
    procedure BufferSwitchTimeInfo(Index: Integer;
      const params: TASIOTime); override;
    procedure SetupBuffersize; override;

    property ConvertMethod: TConvertMethod read FConvertMethod
      write SetConvertMethod;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
{$IFDEF ASIOMixer}
    procedure Mixer;
{$ENDIF}
    property ConvertOptimizations: TConvertOptimizations
      read FConvertOptimizations write SetConvertOptimizations;

    property OnBufferSwitch32: TBufferSwitchAudioData32Event
      read FOnBufferSwitch32 write SetOnBufferSwitch32;
    property OnBufferSwitch64: TBufferSwitchAudioData64Event
      read FOnBufferSwitch64 write SetOnBufferSwitch64;

    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill
      write FInBufferPreFill default bpfNone;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill
      write FOutBufferPreFill default bpfNone;
    property PreventClipping: TPreventClipping read FPreventClipping
      write SetPreventClipping default pcNone;
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
    property OnBuffersDestroy;
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
{$IFDEF DELPHI10_UP} {$ENDREGION 'TASIOHostAudioData'} {$ENDIF}

var
  theHost: TCustomASIOHostBasic;
{$IFDEF FPC}
  PMUpdSamplePos: TLMessage;
  PMBufSwitch: TLMessage;
  PMBufSwitchTimeInfo: TLMessage;
  PMReset: TLMessage;
{$ELSE}
  PMUpdSamplePos: TMessage;
  PMBufSwitch: TMessage;
  PMBufSwitchTimeInfo: TMessage;
  PMReset: TMessage;
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

const
  ASIODRV_DESC = 'description';
  INPROC_SERVER = 'InprocServer32';
  ASIO_PATH = 'software\asio';
  COM_CLSID = 'clsid';

function findDrvPath(const clsidstr: string; var dllpath: string): Integer;
var
  reg: TRegistry;
  success: Boolean;
{$IFNDEF FPC}
  buf: array [0 .. 1024] of AnsiChar;
  s: string;
  temps: string;
{$ENDIF}
begin
  Result := -1;

  // CharLowerBuff(clsidstr,strlen(clsidstr));
  reg := TRegistry.Create;
  try
    reg.RootKey := HKEY_CLASSES_ROOT;
    success := reg.OpenKeyReadOnly(COM_CLSID + '\' + clsidstr + '\' +
      INPROC_SERVER);
    if success then
    begin
      dllpath := reg.ReadString('');
      if (ExtractFilePath(dllpath) = '') and (dllpath <> '') then
      begin
{$IFNDEF FPC}
        buf[0] := #0;
        temps := dllpath; // backup the value
        if GetSystemDirectory(buf, 1023) <> 0 then
        // try the system directory first
        begin
          s := buf;
          dllpath := s + '\' + temps;
        end;

        if not FileExists(dllpath) then // try the windows dir if necessary
        begin
          buf[0] := #0;
          if GetWindowsDirectory(buf, 1023) <> 0 then
          // try the system directory first
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
  r: TRegistry;
  keys: TStringList;
  success: Boolean;
  i: Integer;
  id: string;
  dllpath: string;
  count: Integer;
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
    for i := 0 to keys.count - 1 do
    begin
      success := r.OpenKeyReadOnly(ASIO_PATH + '\' + keys[i]);
      if success then
      begin
        id := r.ReadString(COM_CLSID);
        if findDrvPath(id, dllpath) = 0 then // check if the dll exists
        begin
          SetLength(List, count + 1);
          List[count].id := StringToGUID(id);
          StrPLCopy(List[count].name, keys[i], 512);
          StrPLCopy(List[count].path, dllpath, 512);
          inc(count);
        end;
        r.CloseKey;
      end;
    end;
  finally
    FreeAndNil(keys);
    FreeAndNil(r);
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
    0:
      Result := 'Control Panel';
  end;
end;

function TASIOControlPanel.GetVerbCount: Integer;
begin
  Result := Integer((Component as TCustomASIOHost).DriverIndex >= 0);
end;

procedure TASIOControlPanel.ExecuteVerb(Index: Integer);
begin
  case Index of
    0:
      if (Component as TCustomASIOHost).DriverIndex >= 0 then
        (Component as TCustomASIOHost).ControlPanel;
    end;
  end;
{$ENDIF}
{$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'TASIOTimeSub implementation'} {$ENDIF}

  constructor TASIOTimeSub.Create;
  begin
    with FBufferTime.timeInfo do
    begin
      Speed := 1;
      SampleRate := 44100;
      SamplePosition := Int64ToASIOSamples(0);
    end;
    Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
      atSpeedValid];
  end;

  procedure TASIOTimeSub.Change;
  begin
    if Assigned(FOnChange) then
      FOnChange(Self);
  end;

  procedure TASIOTimeSub.AssignTo(Dest: TPersistent);
  begin
    if Dest is TASIOTimeSub then
      with TASIOTimeSub(Dest) do
      begin
        FBufferTime := Self.FBufferTime;
        Change;
      end
    else
      inherited AssignTo(Dest);
  end;

  function TASIOTimeSub.GetATflags: TATFlags;
  begin
    Result := [];
    if (FBufferTime.timeInfo.Flags and kSystemTimeValid) <> 0 then
      Result := Result + [atSystemTimeValid]
    else
      Result := Result - [atSystemTimeValid];
    if (FBufferTime.timeInfo.Flags and kSamplePositionValid) <> 0 then
      Result := Result + [atSamplePositionValid]
    else
      Result := Result - [atSamplePositionValid];
    if (FBufferTime.timeInfo.Flags and kSampleRateValid) <> 0 then
      Result := Result + [atSampleRateValid]
    else
      Result := Result - [atSampleRateValid];
    if (FBufferTime.timeInfo.Flags and kSpeedValid) <> 0 then
      Result := Result + [atSpeedValid]
    else
      Result := Result - [atSpeedValid];
    if (FBufferTime.timeInfo.Flags and kSampleRateChanged) <> 0 then
      Result := Result + [atSampleRateChanged]
    else
      Result := Result - [atSampleRateChanged];
    if (FBufferTime.timeInfo.Flags and kClockSourceChanged) <> 0 then
      Result := Result + [atClockSourceChanged]
    else
      Result := Result - [atClockSourceChanged];
  end;

  procedure TASIOTimeSub.SetATflags(Flags: TATFlags);
  var
    temp: Integer;
  begin
    temp := 0;
    if (atSystemTimeValid in Flags) then
      temp := temp + kSystemTimeValid;
    if (atSamplePositionValid in Flags) then
      temp := temp + kSamplePositionValid;
    if (atSampleRateValid in Flags) then
      temp := temp + kSampleRateValid;
    if (atSpeedValid in Flags) then
      temp := temp + kSpeedValid;
    if (atSampleRateChanged in Flags) then
      temp := temp + kSampleRateChanged;
    if (atClockSourceChanged in Flags) then
      temp := temp + kClockSourceChanged;
    FBufferTime.timeInfo.Flags := temp;
  end;

  function TASIOTimeSub.GetATdouble(Index: Integer): Double;
  begin
    Result := 0;
    case Index of
      0:
        Result := FBufferTime.timeInfo.Speed;
      1:
        Result := FBufferTime.timeInfo.SampleRate;
    end;
  end;

  procedure TASIOTimeSub.SetATdouble(Index: Integer; Value: Double);
  begin
    case Index of
      0:
        if Value <> FBufferTime.timeInfo.Speed then
        begin
          FBufferTime.timeInfo.Speed := Value;
          Change;
        end;
      1:
        if Value <> FBufferTime.timeInfo.SampleRate then
        begin
          FBufferTime.timeInfo.SampleRate := Value;
          Change;
        end;
    end;
  end;

  function TASIOTimeSub.GetATInt64(Index: Integer): Int64;
  begin
    Result := 0;
    case Index of
      0:
        Result := ASIOSamplesToInt64(FBufferTime.timeInfo.SamplePosition);
    end;
  end;

  procedure TASIOTimeSub.SetATInt64(Index: Integer; Value: Int64);
  begin
    case Index of
      0:
        if Value <> ASIOSamplesToInt64(FBufferTime.timeInfo.SamplePosition) then
        begin
          FBufferTime.timeInfo.SamplePosition := Int64ToASIOSamples(Value);
          Change;
        end;
    end;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'TASIOTimeSub implementation'} {$ENDIF}

  function ChannelTypeToString(vType: TASIOSampleType): string;
  begin
    Result := '';
    case vType of
      ASIOSTInt16MSB:
        Result := 'Int16MSB';
      ASIOSTInt24MSB:
        Result := 'Int24MSB';
      ASIOSTInt32MSB:
        Result := 'Int32MSB';
      ASIOSTFloat32MSB:
        Result := 'Float32MSB';
      ASIOSTFloat64MSB:
        Result := 'Float64MSB';

      // these are used for 32 bit data buffer, with different alignment of the data inside
      // 32 bit PCI bus systems can be more easily used with these
      ASIOSTInt32MSB16:
        Result := 'Int32MSB16';
      ASIOSTInt32MSB18:
        Result := 'Int32MSB18';
      ASIOSTInt32MSB20:
        Result := 'Int32MSB20';
      ASIOSTInt32MSB24:
        Result := 'Int32MSB24';

      ASIOSTInt16LSB:
        Result := 'Int16LSB';
      ASIOSTInt24LSB:
        Result := 'Int24LSB';
      ASIOSTInt32LSB:
        Result := 'Int32LSB';
      ASIOSTFloat32LSB:
        Result := 'Float32LSB';
      ASIOSTFloat64LSB:
        Result := 'Float64LSB';

      // these are used for 32 bit data buffer, with different alignment of the data inside
      // 32 bit PCI bus systems can more easily used with these
      ASIOSTInt32LSB16:
        Result := 'Int32LSB16';
      ASIOSTInt32LSB18:
        Result := 'Int32LSB18';
      ASIOSTInt32LSB20:
        Result := 'Int32LSB20';
      ASIOSTInt32LSB24:
        Result := 'Int32LSB24';
    end;
  end;

  procedure ASIOBufferSwitch(doubleBufferIndex: Integer;
    directProcess: TASIOBool); cdecl;
  begin
    directProcess := ASIOFalse;
    case directProcess of
      ASIOFalse:
        begin
          PMBufSwitch.WParam := AM_BufferSwitch;
          PMBufSwitch.LParam := doubleBufferIndex;
          theHost.Dispatch(PMBufSwitch);
        end;
      ASIOTrue:
        theHost.BufferSwitch(doubleBufferIndex);
    end;
  end;

  function ASIOBufferSwitchTimeInfo(var params: TASIOTime;
    doubleBufferIndex: Integer; directProcess: TASIOBool): PASIOTime; cdecl;
  begin
    directProcess := ASIOFalse;
    case directProcess of
      ASIOFalse:
        begin
          theHost.ASIOTime.FBufferTime := params;
          PMBufSwitchTimeInfo.WParam := AM_BufferSwitchTimeInfo;
          PMBufSwitchTimeInfo.LParam := doubleBufferIndex;
          theHost.Dispatch(PMBufSwitchTimeInfo);
        end;
      ASIOTrue:
        theHost.BufferSwitchTimeInfo(doubleBufferIndex, params);
    end;
    Result := nil;
  end;

  procedure ASIOSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
  begin
    if Assigned(theHost) and Assigned(theHost.FOnSampleRateChanged) then
      theHost.FOnSampleRateChanged(theHost);
  end;

  function ASIOMessage(selector, Value: Integer; Message: pointer; opt: pdouble)
    : Integer; cdecl;
  begin
    Result := 0;
    case selector of
      kASIOSelectorSupported: // return 1 if a selector is supported
        begin
          case Value of
            kASIOEngineVersion:
              if Assigned(theHost) then
                Result := Integer(assEngineVersion
                  in theHost.FASIOSelectorSupport)
              else
                Result := 1;
            kASIOResetRequest:
              if Assigned(theHost) then
                Result := Integer(assResetRequest
                  in theHost.FASIOSelectorSupport)
              else
                Result := 1;
            kASIOBufferSizeChange:
              if Assigned(theHost) then
                Result := Integer(assBufferSizeChange
                  in theHost.FASIOSelectorSupport)
              else
                Result := 1;
            kASIOResyncRequest:
              if Assigned(theHost) then
                Result := Integer(assResyncRequest
                  in theHost.FASIOSelectorSupport)
              else
                Result := 1;
            kASIOLatenciesChanged:
              if Assigned(theHost) then
                Result := Integer(assLatenciesChanged
                  in theHost.FASIOSelectorSupport)
              else
                Result := 1;
            kASIOSupportsTimeInfo:
              if Assigned(theHost) then
                Result := Integer(assSupportsTimeInfo
                  in theHost.FASIOSelectorSupport)
              else
                Result := 0;
            kASIOSupportsTimeCode:
              if Assigned(theHost) then
                Result := Integer(assSupportsTimeCode
                  in theHost.FASIOSelectorSupport)
              else
                Result := 0;
            kASIOSupportsInputMonitor:
              if Assigned(theHost) then
                Result := Integer(assSupportsInputMonitor
                  in theHost.FASIOSelectorSupport)
              else
                Result := 0;
          end;
        end;
      kASIOEngineVersion:
        Result := 2; // ASIO 2 is supported
      kASIOResetRequest:
        if Assigned(theHost) then
        begin
          PMReset.Msg := PM_ASIO;
          PMReset.WParam := AM_ResetRequest;
          PMReset.LParam := 0;
          theHost.Dispatch(PMReset);
          Result := 1;
        end;
      kASIOBufferSizeChange:
        if Assigned(theHost) then
        begin
          PMReset.Msg := PM_ASIO;
          PMReset.WParam := AM_ResetRequest;
          PMReset.LParam := 0;
          theHost.Dispatch(PMReset);
          Result := 1;
        end;
      kASIOResyncRequest:
        ;
      kASIOLatenciesChanged:
        if Assigned(theHost) then
        begin
          PMReset.Msg := PM_ASIO;
          PMReset.WParam := AM_LatencyChanged;
          PMReset.LParam := 0;
          theHost.Dispatch(PMReset);
          Result := 1;
        end;
      kASIOSupportsTimeInfo:
        Result := 1;
      kASIOSupportsTimeCode:
        begin
          Result := 0;
        end;
      kASIOSupportsInputMonitor:
        begin
          Result := 1;
        end;
    end;
  end;

  /// /////////////////////////////////////////////////////////////////////////////
  /// ////////////////////////// TCustomASIOHostBasic /////////////////////////////
  /// /////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$REGION 'TCustomASIOHostBasic implementation'} {$ENDIF}
  { TCustomASIOHostBasic }

  constructor TCustomASIOHostBasic.Create(AOwner: TComponent);
  begin
    FHandleOwned := False;
    if AOwner is TWinControl then
      FHandle := TWinControl(AOwner).Handle
    else
    begin
      FHandle := AllocateHWnd(WndProc);
      FHandleOwned := True;
    end;

    // if theHost <> nil then
    theHost := Self;
    FUnAlignedBuffer := nil;
    FInputBuffer := nil;
    FOutputBuffer := nil;
    FSampleRate := 44100;
    FASIOTime := TASIOTimeSub.Create;
    FDriverList := GetDriverList;
    FASIOSelectorSupport := [assEngineVersion, assResetRequest,
      assBufferSizeChange, assResyncRequest, assLatenciesChanged];

    // set the callbacks record fields
    with FCallbacks do
    begin
      BufferSwitch := ASIOBufferSwitch;
      sampleRateDidChange := ASIOSampleRateDidChange;
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
      if Assigned(FOnDestroy) then
        FOnDestroy(Self);
      FCallbacks.BufferSwitchTimeInfo := nil;
      if Active then
        Active := False;
      CloseDriver;
      if FHandleOwned then
        DeallocateHWnd(FHandle);
      SetLength(FASIOdriverlist, 0);
      SetLength(FInConverters, 0);
      SetLength(FOutConverters, 0);
      FreeAndNil(FDriverList);
      FreeAndNil(FASIOTime);
    finally
      inherited;
      theHost := nil;
    end;
  end;

{$IFNDEF FPC}

  procedure TCustomASIOHostBasic.WndProc(var Msg: TMessage);
  begin
    with Msg do
      Result := DefWindowProc(FHandle, Msg, WParam, LParam);
  end;
{$ELSE}
  function DefWindowProc(hWnd: THandle; Msg: UINT; WParam: WParam;
    LParam: LParam): LRESULT; external 'user32' name 'DefWindowProcA';

  procedure TCustomASIOHostBasic.WndProc(var Msg: TLMessage);
  begin
    with Msg do
      Result := DefWindowProc(FHandle, Msg, WParam, LParam);
  end;
{$ENDIF}
  /// /////////////////////////////////////////////////////////////////////////////

  function TCustomASIOHostBasic.GetDriverList: TStrings;
  var
    i: Integer;
  begin
    Result := TStringList.Create;
    SetLength(FASIOdriverlist, 0);
    ListAsioDrivers(FASIOdriverlist);
    for i := Low(FASIOdriverlist) to High(FASIOdriverlist) do
      Result.Add(FASIOdriverlist[i].name);
  end;

  procedure TCustomASIOHostBasic.SetDriverName(const s: string);
  begin
    if FDriverList.IndexOf(s) > -1 then
      DriverIndex := FDriverList.IndexOf(s);
  end;

  procedure TCustomASIOHostBasic.SetDriverIndex(Value: Integer);
  var
    DrName: array [0 .. 255] of AnsiChar;
    tmpActive: Boolean;
  begin
    if (Value <> FDriverIndex) then
    begin
      tmpActive := Active;
      Active := False;
      if Value < -1 then
        FDriverIndex := -1
      else if Value >= FDriverList.count then
        FDriverIndex := FDriverList.count - 1
      else
        FDriverIndex := Value;
      if FDriverIndex = -1 then
      begin
        FDriverName := '';
        FInputLatency := 0;
        FOutputLatency := 0;
        FInputChannelCount := 0;
        FOutputChannelCount := 0;
        FBufferSize := 0;
        CloseDriver;
      end
      else
      begin
        try
          CloseDriver;
          FDriverName := FDriverList[FDriverIndex];
          OpenDriver;
        except
          exit;
        end;
        if Assigned(FDriver) then
        begin
          FDriver.GetDriverName(DrName);
          FDriverVersion := FDriver.GetDriverVersion;
          CanTimeCode;
          CanTimeInfo;
          CanTransport;
          CanInputGain;
          CanInputMeter;
          CanOutputGain;
          CanOutputMeter;
        end;
      end;
      if Assigned(FOnDriverChanged) then
        OnDriverChanged(Self);
      Active := tmpActive;
    end;
  end;

  procedure TCustomASIOHostBasic.SetupBuffersize;
  begin
    FDriver.GetBufferSize(Fmin, Fmax, Fpref, Fgran);
    if Fmin = Fmax then
      Fpref := Fmin;
    if (Fpref < 1) or (Fpref > 65530) then
      FBufferSize := 4096
    else
      FBufferSize := Fpref;
  end;

  procedure TCustomASIOHostBasic.SetupSampleRate;
  begin
    FDriver.GetSampleRate(FSampleRate);
    SetSampleRate(FSampleRate);
  end;

  function TCustomASIOHostBasic.GetInConverter(ConverterType: TASIOSampleType)
    : TInConverter;
  begin
    case ConverterType of
      ASIOSTInt16MSB:
        Result := FromInt16MSB;
      ASIOSTInt24MSB:
        Result := FromInt24MSB;
      ASIOSTInt32MSB:
        Result := FromInt32MSB;
      ASIOSTFloat32MSB:
        Result := FromSingleMSB;
      ASIOSTFloat64MSB:
        Result := FromDoubleMSB;
      ASIOSTInt32MSB16:
        Result := FromInt32MSB16;
      ASIOSTInt32MSB18:
        Result := FromInt32MSB18;
      ASIOSTInt32MSB20:
        Result := FromInt32MSB20;
      ASIOSTInt32MSB24:
        Result := FromInt32MSB24;
      ASIOSTInt16LSB:
        Result := FromInt16LSB;
      ASIOSTInt24LSB:
        Result := FromInt24LSB;
      ASIOSTInt32LSB:
        Result := FromInt32LSB;
      ASIOSTFloat32LSB:
        Result := FromSingleLSB;
      ASIOSTFloat64LSB:
        Result := FromDoubleLSB;
      ASIOSTInt32LSB16:
        Result := FromInt32LSB16;
      ASIOSTInt32LSB18:
        Result := FromInt32LSB18;
      ASIOSTInt32LSB20:
        Result := FromInt32LSB20;
      ASIOSTInt32LSB24:
        Result := FromInt32LSB24;
    else
      raise Exception.Create(RStrConverterTypeUnknown);
    end;
  end;

  function TCustomASIOHostBasic.GetOutConverter(ConverterType: TASIOSampleType)
    : TOutConverter;
  begin
    case ConverterType of
      ASIOSTInt16MSB:
        Result := ToInt16MSB;
      ASIOSTInt24MSB:
        Result := ToInt24MSB;
      ASIOSTInt32MSB:
        Result := ToInt32MSB;
      ASIOSTFloat32MSB:
        Result := ToSingleMSB;
      ASIOSTFloat64MSB:
        Result := ToDoubleMSB;
      ASIOSTInt32MSB16:
        Result := ToInt32MSB16;
      ASIOSTInt32MSB18:
        Result := ToInt32MSB18;
      ASIOSTInt32MSB20:
        Result := ToInt32MSB20;
      ASIOSTInt32MSB24:
        Result := ToInt32MSB24;
      ASIOSTInt16LSB:
        Result := ToInt16LSB;
      ASIOSTInt24LSB:
        Result := ToInt24LSB;
      ASIOSTInt32LSB:
        Result := ToInt32LSB;
      ASIOSTFloat32LSB:
        Result := ToSingleLSB;
      ASIOSTFloat64LSB:
        Result := ToDoubleLSB;
      ASIOSTInt32LSB16:
        Result := ToInt32LSB16;
      ASIOSTInt32LSB18:
        Result := ToInt32LSB18;
      ASIOSTInt32LSB20:
        Result := ToInt32LSB20;
      ASIOSTInt32LSB24:
        Result := ToInt32LSB24;
    else
      raise Exception.Create(RStrConverterTypeUnknown);
    end;
  end;

  function TCustomASIOHostBasic.CreateBuffers: Boolean;
  var
    i: Integer;
    currentbuffer: PASIOBufferInfo;
  begin
    if FDriver = nil then
    begin
      Result := False;
      exit;
    end;
    if FBuffersCreated then
      DestroyBuffers;
    SetupBuffersize;
    SetupSampleRate;
    FDriver.GetChannels(FInputChannelCount, FOutputChannelCount);

    GetMem(FUnAlignedBuffer, SizeOf(TASIOBufferInfo) *
      (FInputChannelCount + FOutputChannelCount) + 16);
    FInputBuffer := PASIOBufferInfo(Integer(FUnAlignedBuffer) + 16 -
      (Integer(FUnAlignedBuffer) mod 16));

    SetLength(FInputChannelInfos, FInputChannelCount);
    SetLength(FInConverters, FInputChannelCount);
    currentbuffer := FInputBuffer;
    for i := 0 to FInputChannelCount - 1 do
    begin
      FInputChannelInfos[i].Channel := i;
      FInputChannelInfos[i].isInput := ASIOTrue;
      FDriver.GetChannelInfo(FInputChannelInfos[i]);
      FInConverters[i] := GetInConverter(FInputChannelInfos[i].vType);

      currentbuffer^.isInput := ASIOTrue;
      currentbuffer^.channelNum := i;
      currentbuffer^.buffers[0] := nil;
      currentbuffer^.buffers[1] := nil;
      inc(currentbuffer);
    end;

    FOutputBuffer := currentbuffer;
    SetLength(FOutputChannelInfos, FOutputChannelCount);
    SetLength(FOutConverters, FOutputChannelCount);
    for i := 0 to FOutputChannelCount - 1 do
    begin
      FOutputChannelInfos[i].Channel := i;
      FOutputChannelInfos[i].isInput := ASIOFalse; // output
      FDriver.GetChannelInfo(FOutputChannelInfos[i]);
      FOutConverters[i] := GetOutConverter(FOutputChannelInfos[i].vType);

      currentbuffer^.isInput := ASIOFalse; // create an output buffer
      currentbuffer^.channelNum := i;
      currentbuffer^.buffers[0] := nil;
      currentbuffer^.buffers[1] := nil;
      inc(currentbuffer);
    end;

    assert(Fpref > 0);
    Result := (FDriver.CreateBuffers(FInputBuffer,
      (FInputChannelCount + FOutputChannelCount), Fpref, FCallbacks) = ASE_OK);
    FDriver.GetLatencies(FInputLatency, FOutputLatency);
    if Assigned(FOnBuffersCreate) then
      FOnBuffersCreate(Self);
    if Assigned(FOnLatencyChanged) then
      FOnLatencyChanged(Self);
    Randomize;

    {
      ASIOMessage()
      ASIOInputMonitor  TASIOInputMonitor
    }
  end;

  procedure TCustomASIOHostBasic.DestroyBuffers;
  begin
    if (FDriver = nil) then
      exit;
    if FBuffersCreated then
    begin
      if Assigned(FOnBuffersDestroy) then
        FOnBuffersDestroy(Self);
      Dispose(FUnAlignedBuffer);
      FUnAlignedBuffer := nil;
      FInputBuffer := nil;
      FOutputBuffer := nil;
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
    tmpActive: Boolean;

    function Succeeded(Res: HResult): Boolean;
    begin
      Result := Res and $80000000 = 0;
    end;

  begin
    tmpActive := False;
    if Assigned(FDriver) then
    begin
      try
        tmpActive := Active;
        Active := False;
        CloseDriver;
      except
      end;
    end;
    if FDriverIndex >= 0 then
      try
{$IFDEF OpenASIO}
        if OpenASIOCreate(FASIOdriverlist[FDriverIndex].id, FDriver) then
{$ELSE}
        if CreateBeRoASIO(FASIOdriverlist[FDriverIndex].id, FDriver) then
{$ENDIF}
          try
            if Assigned(FDriver) then
              case FDriver.Init(FHandle) of
                0:
                  FDriver := nil; // equals to false here
                ASE_NotPresent:
                  raise Exception.Create('Driver not present');
                ASE_HWMalfunction:
                  raise Exception.Create('Hardware malfunctioning');
                ASE_InvalidParameter:
                  raise Exception.Create('input parameter invalid');
                ASE_InvalidMode:
                  raise Exception.Create
                    ('hardware is in a bad mode or used in a bad mode');
                ASE_SPNotAdvancing:
                  raise Exception.Create
                    ('hardware is not running when sample position is inquired');
                ASE_NoClock:
                  raise Exception.Create
                    ('sample clock or rate cannot be determined or is not present');
                ASE_NoMemory:
                  raise Exception.Create
                    ('not enough memory for completing the request');
              end;
          except
            FDriver := nil;
          end;
      except
        FDriver := nil;
      end;

    if FDriver = nil then
      raise Exception.Create(RStrASIODriverFailed);
    FBuffersCreated := CreateBuffers;
    if not FBuffersCreated then
      raise Exception.Create(RStrASIONoBuffersCreated);
    if tmpActive and FBuffersCreated then
      Active := True;
  end;

  procedure TCustomASIOHostBasic.CloseDriver;
  begin
    if Assigned(FDriver) then
    begin
      try
        if FBuffersCreated then
          DestroyBuffers;
      except
      end;
      FDriver := nil; // RELEASE;
    end;
    FInputLatency := 0;
    FOutputLatency := 0;
    FInputChannelCount := 0;
    FOutputChannelCount := 0;
    FSampleRate := 44100;
  end;

  procedure TCustomASIOHostBasic.ControlPanel;
  begin
    if Assigned(FDriver) then
      FDriver.ControlPanel;
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
    if Assigned(FOnCreate) then
      FOnCreate(Self);
  end;

  procedure TCustomASIOHostBasic.Reset;
  begin
    OpenDriver; // restart the driver
    if Assigned(FOnReset) then
      FOnReset(Self);
  end;

{$IFDEF FPC}

  procedure TCustomASIOHostBasic.PMASIO(var Message: TLMessage);
{$ELSE}

  procedure TCustomASIOHostBasic.PMASIO(var Message: TMessage);
{$ENDIF}
  var
    inp, outp: Integer;
  begin
    if FDriver = nil then
      exit;
    case Message.WParam of
      AM_ResetRequest:
        begin
          OpenDriver; // restart the driver
          if Assigned(FOnReset) then
            FOnReset(Self);
        end;
      AM_BufferSwitch:
        BufferSwitch(Message.LParam); // process a buffer
      AM_BufferSwitchTimeInfo:
        BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);
        // process a buffer with time
      AM_LatencyChanged:
        begin
          if Assigned(FDriver) then
            FDriver.GetLatencies(inp, outp);
          if Assigned(FOnLatencyChanged) then
            FOnLatencyChanged(Self);
        end;
    end;
  end;

{$IFDEF FPC}

  procedure TCustomASIOHostBasic.PMUpdateSamplePos(var Message: TLMessage);
{$ELSE}

  procedure TCustomASIOHostBasic.PMUpdateSamplePos(var Message: TMessage);
{$ENDIF}
  var
    Samples: TASIOSamples;
  begin
    Samples.hi := Message.WParam;
    Samples.lo := Message.LParam;
    if Assigned(FOnUpdateSamplePos) then
      FOnUpdateSamplePos(Self, ASIOSamplesToInt64(Samples));
  end;

  procedure TCustomASIOHostBasic.BufferSwitch(Index: Integer);
  begin
    FillChar(ASIOTime.FBufferTime, SizeOf(TASIOTime), 0);
    // get the time stamp of the buffer, not necessary if no
    // synchronization to other media is required
    if FDriver.GetSamplePosition(ASIOTime.FBufferTime.timeInfo.SamplePosition,
      ASIOTime.FBufferTime.timeInfo.systemTime) = ASE_OK then
      ASIOTime.Flags := ASIOTime.Flags + [atSystemTimeValid,
        atSamplePositionValid];
    BufferSwitchTimeInfo(index, ASIOTime.FBufferTime);
  end;

  procedure TCustomASIOHostBasic.BufferSwitchTimeInfo(Index: Integer;
    const params: TASIOTime);
  begin
    if FDriver = nil then
      exit;
    PMUpdSamplePos.WParam := params.timeInfo.SamplePosition.hi;
    PMUpdSamplePos.LParam := params.timeInfo.SamplePosition.lo;
    Dispatch(PMUpdSamplePos);

    if Assigned(FOnBufferSwitch) then
      FOnBufferSwitch(Self, @(FInputBuffer^), index);
    FDriver.OutputReady;
  end;

  procedure TCustomASIOHostBasic.SetSampleRate(const Value: Double);
  begin
    FSampleRate := Value;
    if (FSampleRate <= 0) or (FSampleRate > 1048575) then
      FSampleRate := 44100;
    ASIOTime.SampleRate := Value;
    if Assigned(FDriver) then
      FDriver.SetSampleRate(Value);
  end;

  procedure TCustomASIOHostBasic.SetActive(Value: Boolean);
  var
    currentbuffer: PASIOBufferInfo;
    i: Integer;
    sz: Word;
  begin
    if FDriver = nil then
      exit;
    if FActive = Value then
      exit;
    if Value = True then
    begin
      try
        FActive := (FDriver.Start = ASE_OK);
      except
        FBufferSize := 2048;
        FSampleRate := 44100;
      end;
      if FActive = False then
        FDriver.Stop;
    end
    else
    begin
      FActive := False;
      // if GetCurrentThreadID = MainThreadID then
      try
        FDriver.Stop;
      except
      end;
      if FBuffersCreated then
        try
          currentbuffer := FOutputBuffer;
          if Assigned(currentbuffer) then
            for i := 0 to FOutputChannelCount - 1 do
              with FOutputChannelInfos[i] do
              begin
                if vType in [ASIOSTInt16MSB, ASIOSTInt16LSB] then
                  sz := SizeOf(Word)
                else if vType in [ASIOSTInt24MSB, ASIOSTInt24LSB] then
                  sz := 3
                else if vType in [ASIOSTFloat32LSB, ASIOSTFloat32MSB] then
                  sz := SizeOf(Single)
                else if vType in [ASIOSTFloat64LSB, ASIOSTFloat64MSB] then
                  sz := SizeOf(Double)
                else
                  sz := SizeOf(Integer);

                assert(Assigned(currentbuffer));
                with currentbuffer^ do
                begin
                  if Assigned(buffers[0]) then
                    FillChar(buffers[0]^, FBufferSize * sz, 0);
                  if Assigned(buffers[1]) then
                    FillChar(buffers[1]^, FBufferSize * sz, 0);
                end;
                inc(currentbuffer);
              end;
          currentbuffer := FInputBuffer;
          if Assigned(currentbuffer) then
            for i := 0 to FInputChannelCount - 1 do
              with FInputChannelInfos[i] do
              begin
                if vType in [ASIOSTInt16MSB, ASIOSTInt16LSB] then
                  sz := SizeOf(Word)
                else if vType in [ASIOSTInt24MSB, ASIOSTInt24LSB] then
                  sz := 3
                else if vType in [ASIOSTFloat32LSB, ASIOSTFloat32MSB] then
                  sz := SizeOf(Single)
                else if vType in [ASIOSTFloat64LSB, ASIOSTFloat64MSB] then
                  sz := SizeOf(Double)
                else
                  sz := SizeOf(Integer);

                assert(Assigned(currentbuffer));
                with currentbuffer^ do
                begin
                  if Assigned(buffers[0]) then
                    FillChar(buffers[0]^, FBufferSize * sz, 0);
                  if Assigned(buffers[1]) then
                    FillChar(buffers[1]^, FBufferSize * sz, 0);
                end;
                inc(currentbuffer);
              end;
        except
        end;
    end;
  end;

  function TCustomASIOHostBasic.GetNumDrivers: Integer;
  begin
    Result := length(FASIOdriverlist);
  end;

  function TCustomASIOHostBasic.CanSampleRate(SampleRate: TASIOSampleRate)
    : TASIOError;
  begin
    if Assigned(FDriver) then
      Result := FDriver.CanSampleRate(SampleRate)
    else
      Result := ASE_NotPresent;
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

  function TCustomASIOHostBasic.GetInputChannelInfo(Index: Integer)
    : TASIOChannelInfo;
  begin
    assert(index >= 0);
    assert(index < FInputChannelCount);
    Result := FInputChannelInfos[index];
  end;

  function TCustomASIOHostBasic.GetOutputChannelInfo(Index: Integer)
    : TASIOChannelInfo;
  begin
    assert(index >= 0);
    assert(index < FOutputChannelCount);
    Result := FOutputChannelInfos[index];
  end;

  function TCustomASIOHostBasic.GetInputMeter(Channel: Integer): Integer;
  var
    ACC: TASIOChannelControls;
  begin
    if FDriver = nil then
    begin
      Result := -1;
      exit;
    end;
    ACC.isInput := 1;
    ACC.Channel := Channel;
    FDriver.Future(kAsioGetInputMeter, @ACC);
    Result := ACC.meter;
  end;

  function TCustomASIOHostBasic.GetOutputMeter(Channel: Integer): Integer;
  var
    ACC: TASIOChannelControls;
  begin
    if FDriver = nil then
    begin
      Result := -1;
      exit;
    end;
    ACC.isInput := 0;
    ACC.Channel := Channel;
    FDriver.Future(kAsioGetOutputMeter, @ACC);
    Result := ACC.meter;
  end;

  procedure TCustomASIOHostBasic.SetInputGain(Channel: Integer; Gain: Integer);
  var
    ACC: TASIOChannelControls;
  begin
    if FDriver = nil then
      exit;
    ACC.isInput := 1;
    ACC.Channel := Channel;
    ACC.Gain := Gain;
    FDriver.Future(kAsioSetInputGain, @ACC);
  end;

  procedure TCustomASIOHostBasic.GetOutputGain(Channel: Integer; Gain: Integer);
  var
    ACC: TASIOChannelControls;
  begin
    if FDriver = nil then
      exit;
    ACC.isInput := 0;
    ACC.Channel := Channel;
    ACC.Gain := Gain;
    FDriver.Future(kAsioSetOutputGain, @ACC);
  end;

  function TCustomASIOHostBasic.CanTimeInfo: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanTimeInfo, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdTimeInfo];
  end;

  function TCustomASIOHostBasic.CanTimeCode: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanTimeCode, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdTimeCode];
  end;

  function TCustomASIOHostBasic.CanTransport: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanTransport, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdTransport];
  end;

  function TCustomASIOHostBasic.CanInputGain: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanInputGain, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdInputGain];
  end;

  function TCustomASIOHostBasic.CanInputMeter: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanInputMeter, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdInputMeter];
  end;

  function TCustomASIOHostBasic.CanOutputGain: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanOutputGain, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdOutputGain];
  end;

  function TCustomASIOHostBasic.CanOutputMeter: Boolean;
  begin
    if FDriver = nil then
      Result := False
    else
      Result := FDriver.Future(kAsioCanOutputMeter, nil) = ASE_SUCCESS;
    FASIOCanDos := FASIOCanDos + [acdOutputMeter];
  end;

  procedure TCustomASIOHostBasic.SetASIOCanDos(const Value: TASIOCanDos);
  begin
  end;

{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
  /// /////////////////////////////////////////////////////////////////////////////
  /// //////////////////////////// TCustomASIOHost ////////////////////////////////
  /// /////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$REGION 'TCustomASIOHost implementation'} {$ENDIF}

  constructor TCustomASIOHost.Create(AOwner: TComponent);
  begin
    FClipPrevent := ClipDigital;
    FConvertOptimizations := [coSSE, co3DNow];
    FOutputDither := odNone;
    FInputMonitor := imDisabled;

{$IFDEF ASIOMixer} FASIOMixer := TFmASIOMixer.Create(nil); {$ENDIF}
    inherited;
  end;

  destructor TCustomASIOHost.Destroy;
  begin
    SetLength(FOutputVolume, 0);
{$IFDEF ASIOMixer} FASIOMixer.Free; {$ENDIF}
    inherited;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

  procedure TCustomASIOHost.SetInputChannelOffset(const w: Word);
  begin
    if (w <> FInputChannelOffset) and (w < FInputChannelCount) then
      FInputChannelOffset := w;
  end;

  procedure TCustomASIOHost.SetOnBufferSwitch32(const Value
    : TBufferSwitchEvent32);
  begin
    FOnBufferSwitch32 := Value;
    if Assigned(FOnBufferSwitch64) then
      ConvertMethod := cm64
    else if Assigned(FOnBufferSwitch32) then
      ConvertMethod := cm32
    else
      ConvertMethod := cmNone;
  end;

  procedure TCustomASIOHost.SetOnBufferSwitch64(const Value
    : TBufferSwitchEvent64);
  begin
    FOnBufferSwitch64 := Value;
    if Assigned(FOnBufferSwitch64) then
      ConvertMethod := cm64
    else if Assigned(FOnBufferSwitch32) then
      ConvertMethod := cm32
    else
      ConvertMethod := cmNone;
  end;

  procedure TCustomASIOHost.SetOutputChannelOffset(const w: Word);
  begin
    if (w <> FOutputChannelOffset) and (w < FOutputChannelCount) then
      FOutputChannelOffset := w;
  end;

  procedure TCustomASIOHost.SetConvertMethod(const Value: TConvertMethod);
  begin
    if ConvertMethod <> Value then
    begin
      FConvertMethod := Value;
    end;
  end;

  procedure TCustomASIOHost.SetConvertOptimizations
    (const co: TConvertOptimizations);
  begin
    Use_FPU;
    case ProcessorType of
      ptSSE:
        if coSSE in co then
          Use_SSE;
      pt3DNow:
        if co3DNow in co then
          Use_3DNow;
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

  procedure TCustomASIOHost.SetPreventClipping(v: TPreventClipping);
  begin
    FPreventClipping := v;
    case FPreventClipping of
      pcDigital:
        FClipPrevent := ClipDigital;
      pcAnalog:
        FClipPrevent := ClipAnalog;
    end;
  end;

  procedure TCustomASIOHost.SetupBuffersize;
  begin
    inherited;
    if Assigned(FASIOGenerator) then
      FASIOGenerator.BlockSize := FBufferSize;
  end;

{$IFDEF ASIOMixer}

  procedure TCustomASIOHost.VolumeChange(Sender: TObject);
  begin
    assert(Sender is TFrChannelStrip);
    with TFrChannelStrip(Sender) do
    begin
      FOutputVolume[Channel] := Volume;
      if Mute then
        FOutputVolume[Channel] := 0;
    end;
  end;

  procedure TCustomASIOHost.SetupMixer;
  var
    i: Integer;
  begin
    with FASIOMixer do
    begin
      for i := 0 to length(ChannelsStrips) - 1 do
        FreeAndNil(ChannelsStrips[i]);
      SetLength(ChannelsStrips, FOutputChannels);
      for i := FOutputChannels - 1 downto 0 do
      begin
        ChannelsStrips[i] := TFrChannelStrip.Create(FASIOMixer);
        with ChannelsStrips[i] do
        begin
          Width := 44;
          Name := 'ChannelStrip' + IntToStr(i);
          Parent := FASIOMixer.MixerPanel;
          Align := alLeft;
          OnVolumeChange := VolumeChange;
          OnMuteChange := VolumeChange;
          Channel := FOutputChannels - 1 - i;
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
    i: Integer;
  begin
    Result := inherited CreateBuffers;

    if Result then
    begin
      SetLength(FOutputVolume, FOutputChannelCount);
      for i := 0 to FOutputChannelCount - 1 do
        FOutputVolume[i] := 1;
{$IFDEF ASIOMixer} SetupMixer; {$ENDIF}
      SetLength(FSingleInBuffer, FInputChannelCount);
      SetLength(FDoubleInBuffer, FInputChannelCount);
      assert(FBufferSize > 0);
      for i := 0 to FInputChannelCount - 1 do
      begin
        SetLength(FSingleInBuffer[i], FBufferSize);
        SetLength(FDoubleInBuffer[i], FBufferSize);
        FillChar(FSingleInBuffer[i, 0], FBufferSize * SizeOf(Single), 0);
        FillChar(FDoubleInBuffer[i, 0], FBufferSize * SizeOf(Double), 0);
      end;

      SetLength(FSingleOutBuffer, FOutputChannelCount);
      SetLength(FDoubleOutBuffer, FOutputChannelCount);
      for i := 0 to FOutputChannelCount - 1 do
      begin
        SetLength(FSingleOutBuffer[i], FBufferSize);
        SetLength(FDoubleOutBuffer[i], FBufferSize);
        FillChar(FSingleOutBuffer[i, 0], FBufferSize * SizeOf(Single), 0);
        FillChar(FDoubleOutBuffer[i, 0], FBufferSize * SizeOf(Double), 0);
      end;
    end;
  end;

{$IFDEF ASIOMixer}

  procedure TCustomASIOHost.Mixer;
  begin
    FASIOMixer.Show;
  end;
{$ENDIF}

  procedure TCustomASIOHost.BufferSwitchTimeInfo(Index: Integer;
    const params: TASIOTime);
  var
    i, j: Integer;
    currentbuffer: PASIOBufferInfo;
    PChannelArray: pointer;
  begin
    if FDriver = nil then
      exit;
    PMUpdSamplePos.WParam := params.timeInfo.SamplePosition.hi;
    PMUpdSamplePos.LParam := params.timeInfo.SamplePosition.lo;
    Dispatch(PMUpdSamplePos);

    currentbuffer := FInputBuffer;

    if Assigned(FOnBufferSwitchNative) then
      FOnBufferSwitchNative(Self, @(FInputBuffer^), index)
    else if FConvertMethod = cm64 then
    begin
      // 64bit float processing
      case FInBufferPreFill of
        bpfZero:
          for j := 0 to FInputChannelCount - 1 do
            FillChar(FDoubleInBuffer[j, 0], FBufferSize * SizeOf(Double), 0);
        bpfNoise:
          for j := 0 to FInputChannelCount - 1 do
            for i := 0 to FBufferSize - 1 do
              FDoubleInBuffer[j, i] := 2 * Random - 1;
      else
        for j := 0 to FInputChannelCount - 1 do
        begin
          PChannelArray := currentbuffer^.buffers[Index];
          assert(length(FDoubleInBuffer[j]) > 0);
          assert(length(FInConverters) > j);
          assert(Assigned(FInConverters[j].ic64));
          if Assigned(PChannelArray) then
            FInConverters[j].ic64(PChannelArray, @FDoubleInBuffer[j, 0],
              FBufferSize);
          inc(currentbuffer);
        end;
      end;

      if FPreventClipping <> pcNone then
        for j := 0 to FInputChannelCount - 1 do
          FClipPrevent.cb64(@FDoubleInBuffer[j, 0], FBufferSize);

      case FOutBufferPreFill of
        bpfZero:
          for j := 0 to FOutputChannelCount - 1 do
            FillChar(FDoubleOutBuffer[j, 0], FBufferSize * SizeOf(Double), 0);
        bpfNoise:
          for j := 0 to FOutputChannelCount - 1 do
            for i := 0 to FBufferSize - 1 do
              FDoubleOutBuffer[j, i] := 2 * Random - 1;
      end;

      case FInputMonitor of
        imMono:
          Move(FDoubleInBuffer[FInputChannelOffset, 0],
            FDoubleOutBuffer[FOutputChannelOffset, 0],
            FBufferSize * SizeOf(Double));
        imStereo:
          begin
            Move(FDoubleInBuffer[FInputChannelOffset, 0],
              FDoubleOutBuffer[FOutputChannelOffset, 0],
              FBufferSize * SizeOf(Double));
            Move(FDoubleInBuffer[FInputChannelOffset + 1, 0],
              FDoubleOutBuffer[FOutputChannelOffset + 1, 0],
              FBufferSize * SizeOf(Double));
          end;
        imAll:
          for j := 0 to min(FInputChannelCount, FOutputChannelCount) - 1 do
            Move(FDoubleInBuffer[j, 0], FDoubleOutBuffer[j, 0],
              FBufferSize * SizeOf(Double));
      end;

      FOnBufferSwitch64(Self, FDoubleInBuffer, FDoubleOutBuffer);

      if FPreventClipping <> pcNone then
        for j := 0 to FOutputChannelCount - 1 do
          FClipPrevent.cb64(@FDoubleOutBuffer[j, 0], FBufferSize);

      currentbuffer := FOutputBuffer;
      for j := 0 to FOutputChannelCount - 1 do
      begin
        PChannelArray := currentbuffer^.buffers[Index];
        if Assigned(PChannelArray) then
          FOutConverters[j].oc64(@FDoubleOutBuffer[j, 0], PChannelArray,
            FBufferSize);
        inc(currentbuffer);
      end;
    end
    else
    begin
      // 32bit float processing
      case FInBufferPreFill of
        bpfZero:
          for j := 0 to FInputChannelCount - 1 do
            FillChar(FSingleInBuffer[j, 0], FBufferSize * SizeOf(Single), 0);
        bpfNoise:
          for j := 0 to FInputChannelCount - 1 do
            for i := 0 to FBufferSize - 1 do
              FSingleInBuffer[j, i] := 2 * Random - 1;
        bpfCustom:
          if Assigned(FASIOGenerator) then
            FASIOGenerator.ProcessBlock(FSingleInBuffer, False);
      else
        begin
          for j := 0 to FInputChannelCount - 1 do
          begin
            PChannelArray := currentbuffer^.buffers[Index];
            assert(PChannelArray <> nil);
            assert(length(FSingleInBuffer[j]) > 0);
            assert(length(FInConverters) > j);
            assert(Assigned(FInConverters[j].ic32));
            if Assigned(PChannelArray) then
              FInConverters[j].ic32(PChannelArray, @FSingleInBuffer[j, 0],
                FBufferSize);
            inc(currentbuffer);
          end;
        end;
      end;

      if FPreventClipping <> pcNone then
        for j := 0 to FInputChannelCount - 1 do
          FClipPrevent.cb32(@FSingleInBuffer[j, 0], FBufferSize);

      case FOutBufferPreFill of
        bpfZero:
          for j := 0 to FOutputChannelCount - 1 do
          begin
            assert(FSingleOutBuffer[j] <> nil);
            FillChar(FSingleOutBuffer[j, 0], FBufferSize * SizeOf(Single), 0);
          end;
        bpfNoise:
          for j := 0 to FOutputChannelCount - 1 do
          begin
            assert(FSingleOutBuffer[j] <> nil);
            for i := 0 to FBufferSize - 1 do
              FSingleOutBuffer[j, i] := 2 * Random - 1;
          end;
        bpfCustom:
          if Assigned(FASIOGenerator) then
            FASIOGenerator.ProcessBlock(FSingleOutBuffer, True);
      end;

      case FInputMonitor of
        imMono:
          Move(FSingleInBuffer[FInputChannelOffset, 0],
            FSingleOutBuffer[FOutputChannelOffset, 0],
            FBufferSize * SizeOf(Single));
        imStereo:
          begin
            Move(FSingleInBuffer[FInputChannelOffset, 0],
              FSingleOutBuffer[FOutputChannelOffset, 0],
              FBufferSize * SizeOf(Single));
            Move(FSingleInBuffer[FInputChannelOffset + 1, 0],
              FSingleOutBuffer[FOutputChannelOffset + 1, 0],
              FBufferSize * SizeOf(Single));
          end;
        imAll:
          for j := 0 to min(FInputChannelCount, FOutputChannelCount) - 1 do
            Move(FSingleInBuffer[j, 0], FSingleOutBuffer[j, 0],
              FBufferSize * SizeOf(Single));
      end;

      if Assigned(FOnBufferSwitch32) then
        FOnBufferSwitch32(Self, FSingleInBuffer, FSingleOutBuffer);

      if FPreventClipping <> pcNone then
        for j := 0 to FOutputChannelCount - 1 do
          FClipPrevent.cb32(@FSingleOutBuffer[j, 0], FBufferSize);

      currentbuffer := FOutputBuffer;
      for j := 0 to FOutputChannelCount - 1 do
      begin
        PChannelArray := currentbuffer^.buffers[Index];
        if Assigned(PChannelArray) then
          FOutConverters[j].oc32(@FSingleOutBuffer[j, 0], PChannelArray,
            FBufferSize);
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
        odNone:
          begin
            Use_FPU;
            case ProcessorType of
              ptSSE:
                if coSSE in FConvertOptimizations then
                  Use_SSE;
              pt3DNow:
                if co3DNow in FConvertOptimizations then
                  Use_3DNow;
            end;
          end;
        odUDF:
          Use_FPU_UDF;
        odTDF:
          Use_FPU_TDF;
      end;
    end;
  end;

{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}
  /// /////////////////////////////////////////////////////////////////////////////
  /// /////////////////////// TCustomASIOHostAudioData ////////////////////////////
  /// /////////////////////////////////////////////////////////////////////////////

{$IFDEF DELPHI10_UP} {$REGION 'TCustomASIOHostAudioData implementation'} {$ENDIF}

  constructor TCustomASIOHostAudioData.Create(AOwner: TComponent);
  begin
    FClipPrevent := ClipDigital;
    FConvertOptimizations := [coSSE, co3DNow];
    FOutputDither := odNone;

{$IFDEF ASIOMixer} FASIOMixer := TFmASIOMixer.Create(nil); {$ENDIF}
    inherited;
  end;

  destructor TCustomASIOHostAudioData.Destroy;
  begin
    if Assigned(FAudioDataInput) then
      FreeAndNil(FAudioDataInput);
    if Assigned(FAudioDataOutput) then
      FreeAndNil(FAudioDataOutput);

{$IFDEF ASIOMixer} FreeAndNil(FASIOMixer); {$ENDIF}
    inherited;
  end;

  /// /////////////////////////////////////////////////////////////////////////////

  procedure TCustomASIOHostAudioData.SetOnBufferSwitch32
    (const Value: TBufferSwitchAudioData32Event);
  begin
    FOnBufferSwitch32 := Value;
    if Assigned(FOnBufferSwitch64) then
      ConvertMethod := cm64
    else if Assigned(FOnBufferSwitch32) then
      ConvertMethod := cm32
    else
      ConvertMethod := cmNone;
  end;

  procedure TCustomASIOHostAudioData.SetOnBufferSwitch64
    (const Value: TBufferSwitchAudioData64Event);
  begin
    FOnBufferSwitch64 := Value;
    if Assigned(FOnBufferSwitch64) then
      ConvertMethod := cm64
    else if Assigned(FOnBufferSwitch32) then
      ConvertMethod := cm32
    else
      ConvertMethod := cmNone;
  end;

  procedure TCustomASIOHostAudioData.SetConvertMethod
    (const Value: TConvertMethod);
  var
    OldIn, OldOut: TCustomAudioDataCollection;
  begin
    if ConvertMethod <> Value then
    begin
      FConvertMethod := Value;
      OldIn := FAudioDataInput;
      OldOut := FAudioDataOutput;
      case FConvertMethod of
        cm32:
          begin
            FAudioDataInput := TASIOAudioDataCollection32.Create(Self,
              InputChannelCount, BufferSize);
            FAudioDataOutput := TASIOAudioDataCollection32.Create(Self,
              OutputChannelCount, BufferSize);
          end;
        cm64:
          begin
            FAudioDataInput := TASIOAudioDataCollection64.Create(Self,
              InputChannelCount, BufferSize);
            FAudioDataOutput := TASIOAudioDataCollection64.Create(Self,
              OutputChannelCount, BufferSize);
          end;
      end;
      if Assigned(OldIn) then
        FreeAndNil(OldIn);
      if Assigned(OldOut) then
        FreeAndNil(OldOut);
    end;
  end;

  procedure TCustomASIOHostAudioData.SetConvertOptimizations
    (const co: TConvertOptimizations);
  begin
    Use_FPU;
    case ProcessorType of
      ptSSE:
        if coSSE in co then
          Use_SSE;
      pt3DNow:
        if co3DNow in co then
          Use_3DNow;
    end;
    FConvertOptimizations := co;
  end;

  procedure TCustomASIOHostAudioData.SetPreventClipping(v: TPreventClipping);
  begin
    FPreventClipping := v;
    case FPreventClipping of
      pcDigital:
        FClipPrevent := ClipDigital;
      pcAnalog:
        FClipPrevent := ClipAnalog;
    end;
  end;

  procedure TCustomASIOHostAudioData.SetupBuffersize;
  begin
    inherited;
    if Assigned(FAudioDataInput) then
      with FAudioDataInput do
      begin
        ChannelCount := InputChannelCount;
        SampleFrames := BufferSize;
      end;
    if Assigned(FAudioDataOutput) then
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
      if Mute then
        FOutputVolume[Channel] := 0;
    end;
  end;

  procedure TCustomASIOHostAudioData.SetupMixer;
  var
    i: Integer;
  begin
    with FASIOMixer do
    begin
      for i := 0 to length(ChannelsStrips) - 1 do
        FreeAndNil(ChannelsStrips[i]);
      SetLength(ChannelsStrips, FOutputChannels);
      for i := FOutputChannels - 1 downto 0 do
      begin
        ChannelsStrips[i] := TFrChannelStrip.Create(FASIOMixer);
        with ChannelsStrips[i] do
        begin
          Width := 44;
          Name := 'ChannelStrip' + IntToStr(i);
          Parent := FASIOMixer.MixerPanel;
          Align := alLeft;
          OnVolumeChange := VolumeChange;
          OnMuteChange := VolumeChange;
          Channel := FOutputChannels - 1 - i;
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
    i: Integer;
  begin
    Result := inherited CreateBuffers;

    if Result then
    begin
      SetLength(FOutputVolume, FOutputChannelCount);
      for i := 0 to FOutputChannelCount - 1 do
        FOutputVolume[i] := 1;
{$IFDEF ASIOMixer} SetupMixer; {$ENDIF}
      if Assigned(FAudioDataInput) then
        FAudioDataInput.ChannelCount := FInputChannelCount;
      if Assigned(FAudioDataOutput) then
        FAudioDataOutput.ChannelCount := FOutputChannelCount;
    end;
  end;

{$IFDEF ASIOMixer}

  procedure TCustomASIOHostAudioData.Mixer;
  begin
    FASIOMixer.Show;
  end;
{$ENDIF}

  procedure TCustomASIOHostAudioData.BufferSwitchTimeInfo(Index: Integer;
    const params: TASIOTime);
  var
    ch: Integer;
    currentbuffer: PASIOBufferInfo;
    PChannelArray: pointer;
  begin
    if FDriver = nil then
      exit;
    PMUpdSamplePos.WParam := params.timeInfo.SamplePosition.hi;
    PMUpdSamplePos.LParam := params.timeInfo.SamplePosition.lo;
    Dispatch(PMUpdSamplePos);
    currentbuffer := FInputBuffer;

    if FConvertMethod = cm64 then
    begin
      // 64bit float processing

      // process input
      with TASIOAudioDataCollection64(FAudioDataInput) do
        case FInBufferPreFill of
          bpfZero:
            FAudioDataInput.Clear;
          bpfNoise:
            FAudioDataInput.GenerateWhiteNoise(1);
        else
          // convert soundcard dependent format to float data
          for ch := 0 to FInputChannelCount - 1 do
          begin
            PChannelArray := currentbuffer^.buffers[Index];
            if Assigned(PChannelArray) then
              FInConverters[ch].ic64(PChannelArray,
                pdouble(TASIOAudioDataCollection64(FAudioDataInput)
                .ChannelDataPointerList[ch]), FBufferSize);
            inc(currentbuffer);
          end;
        end;

      // process output
      case FOutBufferPreFill of
        bpfZero:
          FAudioDataOutput.Clear;
        bpfNoise:
          FAudioDataOutput.GenerateWhiteNoise(1);
      end;

      // call event to send in and get output data
      FOnBufferSwitch64(Self, TASIOAudioDataCollection64(FAudioDataInput),
        TASIOAudioDataCollection64(FAudioDataOutput));

      with TASIOAudioDataCollection64(FAudioDataOutput) do
      begin
        // eventually clip data to avoid ugly artifacts caused by the soundcard
        if FPreventClipping <> pcNone then
          for ch := 0 to FOutputChannelCount - 1 do
            FClipPrevent.cb64(pdouble(ChannelDataPointerList[ch]), FBufferSize);

        // convert float data to soundcard dependent format
        currentbuffer := FOutputBuffer;
        for ch := 0 to FOutputChannelCount - 1 do
        begin
          PChannelArray := currentbuffer^.buffers[Index];
          if Assigned(PChannelArray) then
            FOutConverters[ch].oc64(pdouble(ChannelDataPointerList[ch]),
              PChannelArray, FBufferSize);
          inc(currentbuffer);
        end;
      end;
    end
    else
    begin
      // 32bit float processing

      // process input
      with TASIOAudioDataCollection32(FAudioDataInput) do
        case FInBufferPreFill of
          bpfZero:
            FAudioDataInput.Clear;
          bpfNoise:
            FAudioDataInput.GenerateWhiteNoise(1);
        else
          // convert soundcard dependent format to float data
          for ch := 0 to FInputChannelCount - 1 do
          begin
            PChannelArray := currentbuffer^.buffers[Index];
            if Assigned(PChannelArray) then
              FInConverters[ch].ic32(PChannelArray,
                PSingle(ChannelDataPointerList[ch]), FBufferSize);
            inc(currentbuffer);
          end;
        end;

      // process output
      case FOutBufferPreFill of
        bpfZero:
          FAudioDataOutput.Clear;
        bpfNoise:
          FAudioDataOutput.GenerateWhiteNoise(1);
      end;

      // call event to send in and get output data
      FOnBufferSwitch32(Self, TASIOAudioDataCollection32(FAudioDataInput),
        TASIOAudioDataCollection32(FAudioDataOutput));

      with TASIOAudioDataCollection32(FAudioDataOutput) do
      begin
        // eventually clip data to avoid ugly artifacts caused by the soundcard
        if FPreventClipping <> pcNone then
          for ch := 0 to FOutputChannelCount - 1 do
            FClipPrevent.cb32(PSingle(ChannelDataPointerList[ch]), FBufferSize);

        // convert float data to soundcard dependent format
        currentbuffer := FOutputBuffer;
        for ch := 0 to FOutputChannelCount - 1 do
        begin
          PChannelArray := currentbuffer^.buffers[Index];
          if Assigned(PChannelArray) then
            FOutConverters[ch].oc32(PSingle(ChannelDataPointerList[ch]),
              PChannelArray, FBufferSize);
          inc(currentbuffer);
        end;
      end;

    end;
    FDriver.OutputReady;
  end;

{$IFDEF DELPHI10_UP} {$ENDREGION} {$ENDIF}

initialization

PMUpdSamplePos.Msg := PM_UpdateSamplePos;
PMBufSwitch.Msg := PM_BufferSwitch;
PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
PMReset.Msg := PM_Reset;

end.
