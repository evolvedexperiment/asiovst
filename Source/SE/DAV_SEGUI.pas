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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2019          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_SEGUI;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, DAV_SECommon;

type
  // Plugin Module opCodes (GUI)

  TSEGuiPluginOpcodes = (seGuiInitialise = 0, seGuiClose, seGuiPaint,
    seGuiLButtonDown, seGuiLButtonUp, seGuiMouseMove, seGuiOnModuleMessage,
    seGuiOnGuiPlugValueChange, seGuiOnWindowOpen, seGuiOnWindowClose,
    seGuiOnIdle, seGuiOnNewConnection, seGuiOnDisconnect,
    seGuiDoNotUseOrRemoveThis = $7FFFFFFF);

  // Host opCodes (GUI)

  TSEGuiHostOpcodes = (seGuiHostRequestRepaint = 0, seGuiHostGetHandle,
    seGuiHostSendStringToAudio, seGuiHostSetWindowSize,
    seGuiHostSetWindowSizeable, seGuiHostGetTotalPinCount,
    seGuiHostPlugSetValText, seGuiHostPlugGetValText, seGuiHostAddGuiPlug,
    seGuiRegisterPatchParameter, seGuiHostGetFontInfo, seGuiHostSetWindowType,
    seGuiHostGetWindowHandle, seGuiHostSetWindowFlags, seGuiHostPlugGetVal,
    seGuiHostPlugSetVal, seGuiHostPlugSetExtraData, seGuiHostPlugGetExtraData,
    seGuiHostSetCapture, seGuiHostReleaseCapture, seGuiHostGetCapture,
    seGuiHostCallVstHost, seGuiHostSetIdle, seGuiHostGetModuleFilename,
    seGuiHostResolveFilename, seGuiHostGetHostType, seGuiHostRemoveGuiPlug,
    seGuiHostGetParentContext, seGuiHostMapWindowPoints,
    seGuiHostMapClientPointToScreen, seGuiHostInvalidateRect,
    seGuiHostIsGraphInitialsed, seGuiHostIsInteger = $7FFFFFFF);

  PSEGUIStructBase = ^TSEGUIStructBase;
  TSEGUIBase = class;

  TSEGuiCallback = function(Effect: PSEGUIStructBase; Opcode: TSEGuiHostOpcodes;
    Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
  TSEGuiDispatcher = function(Effect: PSEGUIStructBase;
    Opcode: TSEGuiPluginOpcodes; Index, Value: Integer; Ptr: Pointer;
    Opt: Single): Integer; cdecl;

  TSEGUIStructBase = record
    Magic: Integer; // magic number
    Version: Integer;
    Dispatcher: TSEGuiDispatcher;
    HostPtr: Pointer; // reserved for host use, must be 0
    SEGUIBase: TSEGUIBase; // for class access
    User: Pointer; // user access
    Future: array [0 .. 15] of Char; // pls zero
  end;

  TSEHostWindowFlag = (hwfResizable = 1, hwfNoCustomGfxOnStructure = 2);
  TSEHostWindowFlags = set of TSEHostWindowFlag;

  // painting info
  TSEpoint = TPoint;

  PSEWndInfo = ^TSEWndInfo;

  TSEWndInfo = record
    Width: Integer;
    Height: Integer;
    ContextHandle: THandle;
  end;

  TSEFontInfo = record
    Size: Integer;
    Color: Integer;
    ColorBackground: Integer;
    Flags: Integer; // alignment etc
    FontHeight: Integer;
    Future: array [0 .. 99] of Char;
  end;

  TSEGuiPin = class(TObject)
  private
    FIndex: Integer;
    FModule: TSEGUIBase;
    function GetValueBool: Boolean;
    function GetValueFloat: Single;
    function GetValueInt: Integer; // int, bool, and list type values
    function GetValueText: TSeSdkString;
    procedure SetValueBool(const Value: Boolean);
    procedure SetValueFloat(Value: Single);
    procedure SetValueInt(Value: Integer);
    procedure SetValueAsString(const Value: TSeSdkString);
  protected
    property Module: TSEGUIBase read FModule;
  public
    constructor Create; overload; virtual;
    constructor Create(const AIndex: Integer; const AModule: TSEGUIBase);
      overload; virtual;
    procedure Init(const AIndex: Integer; const AModule: TSEGUIBase); virtual;
    function GetExtraData: TSeSdkString2;
    procedure SetValueText(var Value: TSeSdkString);

    property PinIndex: Integer read FIndex;
    property ValueAsInteger: Integer read GetValueInt write SetValueInt;
    property ValueAsBoolean: Boolean read GetValueBool write SetValueBool;
    property ValueAsSingle: Single read GetValueFloat write SetValueFloat;
    property ValueAsString: TSeSdkString read GetValueText
      write SetValueAsString;
  end;

  TSEGuiPins = array of TSEGuiPin;

  TSEGuiPinEvent = procedure(Sender: TObject; Pin: TSEGuiPin) of object;
  TSEGuiPinIndexEvent = procedure(Sender: TObject; PinIndex: Integer) of object;

  TSEGUIBase = class(TObject)
  private
    FOnIdle: TNotifyEvent;
    FOnPinValueChange: TSEGuiPinEvent;
    FOnDisconnect: TSEGuiPinIndexEvent;

    FOnLButtonDown: TNotifyEvent;
    FOnLButtonUp: TNotifyEvent;
    FOnMouseMove: TNotifyEvent;
    FOnNewConnection: TNotifyEvent;
    FOnWindowClose: TNotifyEvent;
    FOnWindowOpen: TNotifyEvent;
    FOnPaint: TNotifyEvent;

    // FPins   : array of TSeGuiPin;
    procedure SetupPins;
    function GetPin(Index: Integer): TSEGuiPin;
  protected
    FAudioMaster: TSEGuiCallback;
    FStructBase: TSEGUIStructBase;
    function GetSEGUIStructBase: PSEGUIStructBase;
    function GuiIdle: Boolean; virtual;
    procedure GuiDisconnect(PinIndex: Integer); virtual;
    procedure GuiPinValueChange(CurrentPin: TSEGuiPin); virtual;
    procedure GuiLButtonDown(WI: PSEWndInfo; nFlags: Cardinal;
      Point: TSEpoint); virtual;
    procedure GuiLButtonUp(WI: PSEWndInfo; nFlags: Cardinal;
      Point: TSEpoint); virtual;
    procedure GuiModuleMsg(UserMsg_id: Integer; MsgLength: Integer;
      MsgData: Pointer); virtual;
    procedure GuiMouseMove(WI: PSEWndInfo; nFlags: Cardinal;
      Point: TSEpoint); virtual;
    procedure GuiNewConnection(PinIndex: Integer); virtual;
    procedure GuiWindowClose(WI: PSEWndInfo); virtual;
    procedure GuiWindowOpen(WI: PSEWndInfo); virtual;
    procedure GuiPaint(hDC: hDC; WI: PSEWndInfo); virtual;
  public
    constructor Create(SEGuiCallback: TSEGuiCallback;
      AHostPtr: Pointer); virtual;
    destructor Destroy; override;

    // called from audio master
    function CallHost(Opcode: TSEGuiHostOpcodes; Index: Integer = 0;
      Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0): Integer;
    function Dispatcher(Opcode: TSEGuiPluginOpcodes; Index, Value: Integer;
      Ptr: Pointer; Opt: Single): Integer; virtual;
    function GetCapture(const WI: PSEWndInfo): Boolean;
    procedure AddGuiPlug(ADatatype: TSEPlugDataType; ADirection: TSEDirection;
      const AName: Pchar);
    procedure Close; virtual;
    procedure Initialise(const LoadedFromFile: Boolean); virtual;
    procedure ReleaseCapture(const WI: PSEWndInfo);
    procedure SetCapture(const WI: PSEWndInfo);

    property SEGUIStructBase: PSEGUIStructBase read GetSEGUIStructBase;
    property Pin[Index: Integer]: TSEGuiPin read GetPin;

    property OnIdle: TNotifyEvent read FOnIdle write FOnIdle;
    property OnDisconnect: TSEGuiPinIndexEvent read FOnDisconnect
      write FOnDisconnect;
    property OnPinValueChange: TSEGuiPinEvent read FOnPinValueChange
      write FOnPinValueChange;
  end;

implementation

uses
  SysUtils, Types;

var
  StaticPin: TSEGuiPin;

{ TSeGuiPin }

constructor TSEGuiPin.Create;
begin
  inherited;
end;

constructor TSEGuiPin.Create(const AIndex: Integer; const AModule: TSEGUIBase);
begin
  Create;
  Init(AIndex, AModule);
end;

procedure TSEGuiPin.SetValueText(var Value: TSeSdkString);
begin
  FModule.CallHost(seGuiHostPlugSetValText, FIndex, 0, @Value);
end;

function TSEGuiPin.GetValueText: TSeSdkString;
begin
  // warning, unstable over 2000 bytes  ( that's 1000 UNICODE characters )
  result := Pchar(FModule.CallHost(seGuiHostPlugGetValText, FIndex, 0, nil));
end;

procedure TSEGuiPin.Init(const AIndex: Integer; const AModule: TSEGUIBase);
begin
  FIndex := AIndex;
  FModule := AModule;
end;

function TSEGuiPin.GetValueInt: Integer; // int, bool, and list type values
begin
  FModule.CallHost(seGuiHostPlugGetVal, FIndex, 0, @result);
end;

function TSEGuiPin.GetValueBool: Boolean;
begin
  FModule.CallHost(seGuiHostPlugGetVal, FIndex, 0, @result);
end;

function TSEGuiPin.GetValueFloat: Single;
begin
  FModule.CallHost(seGuiHostPlugGetVal, FIndex, 0, @result);
end;

procedure TSEGuiPin.SetValueAsString(const Value: TSeSdkString);
begin
  FModule.CallHost(seGuiHostPlugSetValText, FIndex, 0, @Value);
end;

procedure TSEGuiPin.SetValueBool(const Value: Boolean);
begin
  FModule.CallHost(seGuiHostPlugSetVal, FIndex, Integer(Value), nil);
end;

procedure TSEGuiPin.SetValueFloat(Value: Single);
begin
  FModule.CallHost(seGuiHostPlugSetVal, FIndex, 0, nil, Value);
end;

procedure TSEGuiPin.SetValueInt(Value: Integer);
begin
  FModule.CallHost(seGuiHostPlugSetVal, FIndex, Value, nil);
end;

function TSEGuiPin.GetExtraData: TSeSdkString2;
var
  StringLength: Integer;
  Temp: PWideChar;
begin
  StringLength := FModule.CallHost(seGuiHostPlugGetExtraData, FIndex, 0, nil);
  GetMem(Temp, StringLength * 2);
  try
    FModule.CallHost(seGuiHostPlugGetExtraData, FIndex, StringLength, Temp);
    result := Temp;
  finally
    Dispose(Temp);
  end;
end;

function DispatchEffectClass(Effect: PSEGUIStructBase; Opcode: Integer;
  Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
  if Assigned(Effect) then
  begin
    assert(assigned(Effect.SEGUIBase));
    result := Effect.SEGUIBase.Dispatcher(TSEGuiPluginOpcodes(Opcode), Index,
      Value, Ptr, Opt);
  end
  else
    result := 0;
end;

{ TSEGUIBase }

constructor TSEGUIBase.Create(SEGuiCallback: TSEGuiCallback; AHostPtr: Pointer);
begin
  FAudioMaster := SEGuiCallback;
  FillChar(FStructBase, SizeOf(TSEGUIStructBase), 0);
  with FStructBase do
  begin
    Magic := CSepMagic2;
    Dispatcher := @DispatchEffectClass;
    SEGUIBase := Self;
    HostPtr := AHostPtr;
    Version := 1;
  end;
end;

destructor TSEGUIBase.Destroy;
begin
  inherited;
end;

function TSEGUIBase.Dispatcher(Opcode: TSEGuiPluginOpcodes;
  Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer;
var
  pnt: TSEpoint;
begin
  result := 0;
  case Opcode of
    seGuiInitialise:
      Initialise(Index = 1);
    seGuiClose:
      begin
        Close;
        Free;
        result := 1; // this object now deleted, must do nothing more.
      end;
    seGuiPaint:
      GuiPaint(hDC(Index), PSEWndInfo(Ptr));
    seGuiLButtonDown:
      begin
        pnt := Point(Index, Value);
        GuiLButtonDown(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
      end;
    seGuiLButtonUp:
      begin
        pnt := Point(Index, Value);
        GuiLButtonUp(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
      end;
    seGuiMouseMove:
      begin
        pnt := Point(Index, Value);
        GuiMouseMove(PSEWndInfo(Ptr), PCardinal(@Opt)^, pnt);
      end;
    seGuiOnModuleMessage:
      GuiModuleMsg(Value, Index, Ptr);
    seGuiOnGuiPlugValueChange:
      begin
        StaticPin.Init(Index, Self);
        GuiPinValueChange(StaticPin);
      end;
    seGuiOnWindowOpen:
      GuiWindowOpen(PSEWndInfo(Ptr));
    seGuiOnWindowClose:
      GuiWindowClose(PSEWndInfo(Ptr));
    seGuiOnIdle:
      result := Integer(GuiIdle);
    seGuiOnNewConnection:
      GuiNewConnection(Index);
    seGuiOnDisconnect:
      GuiDisconnect(Index);
  end;
end;

function TSEGUIBase.CallHost(Opcode: TSEGuiHostOpcodes; Index, Value: Integer;
  Ptr: Pointer; Opt: Single): Integer;
begin
  assert(assigned(FAudioMaster));
  result := FAudioMaster(@FStructBase, Opcode, Index, Value, Ptr, Opt);
end;

procedure TSEGUIBase.Close;
begin
  // do nothing yet
end;

procedure TSEGUIBase.Initialise(const LoadedFromFile: Boolean);
begin
  SetupPins;
end;

procedure TSEGUIBase.GuiDisconnect(PinIndex: Integer);
begin
  if Assigned(FOnDisconnect) then
    FOnDisconnect(Self, PinIndex);
end;

procedure TSEGUIBase.GuiPinValueChange(CurrentPin: TSEGuiPin);
begin
  if Assigned(FOnPinValueChange) then
    FOnPinValueChange(Self, CurrentPin);
end;

function TSEGUIBase.GuiIdle: Boolean;
begin
  if Assigned(FOnIdle) then
    FOnIdle(Self);
  result := Assigned(FOnIdle);
end;

procedure TSEGUIBase.GuiLButtonDown(WI: PSEWndInfo; nFlags: Cardinal;
  Point: TSEpoint);
begin
  if Assigned(FOnLButtonDown) then
    FOnLButtonDown(Self);
end;

procedure TSEGUIBase.GuiLButtonUp(WI: PSEWndInfo; nFlags: Cardinal;
  Point: TSEpoint);
begin
  if Assigned(FOnLButtonUp) then
    FOnLButtonUp(Self);
end;

procedure TSEGUIBase.GuiModuleMsg(UserMsg_id, MsgLength: Integer;
  MsgData: Pointer);
begin
  if Assigned(FOnLButtonUp) then
    FOnLButtonUp(Self);
end;

procedure TSEGUIBase.GuiMouseMove(WI: PSEWndInfo; nFlags: Cardinal;
  Point: TSEpoint);
begin
  if Assigned(FOnMouseMove) then
    FOnMouseMove(Self);
end;

procedure TSEGUIBase.GuiNewConnection(PinIndex: Integer);
begin
  if Assigned(FOnNewConnection) then
    FOnNewConnection(Self);
end;

procedure TSEGUIBase.GuiWindowClose(WI: PSEWndInfo);
begin
  if Assigned(FOnWindowClose) then
    FOnWindowClose(Self);
end;

procedure TSEGUIBase.GuiWindowOpen(WI: PSEWndInfo);
begin
  if Assigned(FOnWindowOpen) then
    FOnWindowOpen(Self);
end;

procedure TSEGUIBase.GuiPaint(hDC: hDC; WI: PSEWndInfo);
begin
  if Assigned(FOnPaint) then
    FOnPaint(Self);
end;

procedure TSEGUIBase.AddGuiPlug(ADatatype: TSEPlugDataType;
  ADirection: TSEDirection; const AName: Pchar);
begin
  CallHost(seGuiHostAddGuiPlug, Integer(ADatatype), Integer(ADirection), AName);
  SetupPins;
end;

procedure TSEGUIBase.SetupPins;
(*
  var
  i, ActualPlugCount: Integer;
*)
begin
  // commented out, may need reinstating if pins ever gain state
  (*
    // get actual number of pins used (may be more or less if auto-duplicating plugs used)
    ActualPlugCount := CallHost(seGuiHostGetTotalPinCount);

    FPins.Resize(ActualPlugCount);

    for i := 0 to ActualPlugCount - 1
    do FPins[i].Init(i, self);
  *)
end;

function TSEGUIBase.GetPin(Index: Integer): TSEGuiPin;
begin
  // there are no pins.
  // pins currently hold no state, implement them as a flyweight (saves having
  // to track pin add/remove, we're not notified of autoduplicate add/remove anyhow)
  StaticPin.Init(Index, Self);
  result := StaticPin;

  // { return &m_pins[Index];}
end;

// capture mouse movement
procedure TSEGUIBase.SetCapture(const WI: PSEWndInfo);
begin
  CallHost(seGuiHostSetCapture, 0, 0, WI);
end;

// release capture mouse movement
procedure TSEGUIBase.ReleaseCapture(const WI: PSEWndInfo);
begin
  CallHost(seGuiHostReleaseCapture, 0, 0, WI);
end;

// query mouse capture state
function TSEGUIBase.GetSEGUIStructBase: PSEGUIStructBase;
begin
  result := @FStructBase;
end;

function TSEGUIBase.GetCapture(const WI: PSEWndInfo): Boolean;
begin
  result := CallHost(seGuiHostGetCapture, 0, 0, WI) <> 0;
end;

initialization

StaticPin := TSEGuiPin.Create;

finalization

FreeAndNil(StaticPin);

end.
