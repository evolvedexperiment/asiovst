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
{  Portions created by Christian-W. Budde are Copyright (C) 2003-2012          }
{  by Christian-W. Budde. All Rights Reserved.                                 }
{                                                                              }
{******************************************************************************}

unit DAV_SEHost;

interface

{$I ..\DAV_Compiler.inc}

uses
{$IFDEF FPC}LCLIntf, {$ELSE}Windows, {$ENDIF} Classes, SysUtils,
  DAV_SECommon, DAV_SEModule, DAV_SEGUI;

type
  TSEGetModuleProperties = function(Index: Integer;
    Properties: PSEModuleProperties): Boolean; cdecl;
  TSEMakeModule = function(Index, ProcessType: Integer; Callback: Pointer;
    Reserved: Pointer): Pointer; cdecl;

  TCustomSEHost = class;
  TCustomSEHostedModule = class;

  TCustomSEHostedModulePart = class(TComponent)
  private
    FIndex: Integer;
    FProperties: TSEModuleProperties;
    FSEHostedModule: TCustomSEHostedModule;
    FSE2ModStructBase: PSE2ModStructBase;
    FSEGUIStructBase: PSEGUIStructBase;

    FOnRepaintRequest: TNotifyEvent;

    function GetAbout: AnsiString;
    function GetActive: Boolean;
    function GetID: AnsiString;
    function GetMagic: Integer;
    function GetName: AnsiString;
    function GetVersion: Integer;
    procedure DisposeStructures;
    procedure SetActive(const Value: Boolean);
    function GetGUIVersion: Integer;
  protected
    function CallPlugin(Opcode: TSEPluginModuleOpcodes; Index: Integer = 0;
      Value: Integer = 0; Ptr: Pointer = nil; Opt: Single = 0)
      : Integer; virtual;
    procedure GuiHostRepaintRequest;
    function GuiHostGetHandle: THandle;
    procedure GuiHostSetWindowSize(const x, y: Integer);
    procedure GuiHostSetWindowSizeable;
    procedure GuiHostAddGuiPlug;
    function GuiHostGetTotalPinCount: Integer;
    procedure GuiHostResolveFilename(FileName: PWideChar;
      MaxStringLength: Integer);
  public
    constructor Create(Owner: TCustomSEHostedModule; Index: Integer = 0;
      Properties: PSEModuleProperties = nil); reintroduce; virtual;
    procedure Instantiation; virtual;
    procedure Open; virtual;
    procedure Close; virtual;
    procedure AddEvent(Event: TSEEvent); virtual;
    function IsEventListEmpty: Boolean; virtual;
    function QueryDebugInfo: Pointer; virtual;
    procedure Resume(Index: Integer = 0); virtual;
    procedure GuiNotify(Value: Integer = 0; Index: Integer = 0;
      Ptr: Pointer = nil); virtual;
    procedure SetSampleRate(Value: Single); virtual;
    procedure SetBlockSize(Value: Integer); virtual;
    function GetPinProperties(Index: Integer;
      var Pin: TSEPinProperties): Boolean;

    property Properties: TSEModuleProperties read FProperties;
  published
    property Active: Boolean read GetActive write SetActive;
    property Name: AnsiString read GetName;
    property ID: AnsiString read GetID;
    property Magic: Integer read GetMagic;
    property Version: Integer read GetVersion;
    property About: AnsiString read GetAbout;

    property GUIVersion: Integer read GetGUIVersion;

    property OnRepaintRequest: TNotifyEvent read FOnRepaintRequest
      write FOnRepaintRequest;
  end;

  TCustomSEHostedModule = class(TCollectionItem)
  private
    FLoaded: Boolean;
    FDisplayName: string;
    FParts: array of TCustomSEHostedModulePart;
    FSEMFileName: TFileName;
    FSEModuleHandle: THandle;
    FGetModuleProperties: TSEGetModuleProperties;
    FMakeModule: TSEMakeModule;
    procedure SetSEMFileName(const Value: TFileName);
    procedure InitializeVariables;
    procedure ListParts;
    procedure CloseParts;
    function GetPart(Index: Integer): TCustomSEHostedModulePart;
    function GetPartCount: Integer;
  protected
    procedure AssignTo(Dest: TPersistent); override;
    function GetDisplayName: string; override;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure LoadFromFile(FileName: TFileName);
    procedure UnLoad;

    property Loaded: Boolean read FLoaded;
    property DisplayName: string read GetDisplayName write FDisplayName;
    property SEMFileName: TFileName read FSEMFileName write SetSEMFileName;
    property Part[index: Integer]: TCustomSEHostedModulePart read GetPart;
    property PartCount: Integer read GetPartCount;
  end;

  TSEHostedModule = class(TCustomSEHostedModule)
  published
    property Loaded;
    property DisplayName;
    property SEMFileName;
  end;

  TSEHostedModules = class(TOwnedCollection)
  private
    FOwner: TComponent;
    function GetSEHost: TCustomSEHost;
    function GetItem(Index: Integer): TSEHostedModule;
    procedure SetItem(Index: Integer; const Value: TSEHostedModule);
  protected
    property Items[Index: Integer]: TSEHostedModule read GetItem
      write SetItem; default;
    property SEHost: TCustomSEHost read GetSEHost;
  public
    constructor Create(AOwner: TComponent);
    function Add: TSEHostedModule;
    function CloneAdd(Source: TSEHostedModule): TSEHostedModule;
    function Insert(Index: Integer): TSEHostedModule;
    procedure Delete(Index: Integer);
    property Count;
  end;

  TCustomSEHost = class(TComponent)
  private
    FSampleRate: Single;
    FBlockSize: Integer;
    FOnCreate: TNotifyEvent;
    FOnDestroy: TNotifyEvent;
    FSEHostedModules: TSEHostedModules;
    function GetItem(Index: Integer): TCustomSEHostedModule;
    procedure SetHostedModules(const Value: TSEHostedModules);
  protected
    property Items[Index: Integer]: TCustomSEHostedModule read GetItem; default;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property OnCreate: TNotifyEvent read FOnCreate write FOnCreate;
    property OnDestroy: TNotifyEvent read FOnDestroy write FOnDestroy;
    property HostedSEModules: TSEHostedModules read FSEHostedModules
      write SetHostedModules;
  end;

  TSEHost = class(TCustomSEHost)
  published
    property OnCreate;
    property OnDestroy;
    property HostedSEModules;
  end;

procedure Register;

implementation

uses
  DAV_Common;

resourcestring
  RStrLoadingFailed = 'Loading failed!';
  RStrFileDoesNotExist = 'File %d does not exists';
  RStrNoEntryPoint = 'SynthEdit entry point could not be detected';
  RStrValue = 'Value';

function HostOpcodeToString(Opcode: TSEHostOpcodes): string;
begin
  case Opcode of
    SEAudioMasterSetPinStatus:
      Result := 'SEAudioMasterSetPinStatus';
    SEAudioMasterIsPinConnected:
      Result := 'SEAudioMasterIsPinConnected';
    SEAudioMasterGetPinInputText:
      Result := 'SEAudioMasterGetPinInputText';
    SEAudioMasterGetSampleClock:
      Result := 'SEAudioMasterGetSampleClock';
    SEAudioMasterSendMIDI:
      Result := 'SEAudioMasterSendMIDI';
    SEAudioMasterGetInputPinCount:
      Result := 'SEAudioMasterGetInputPinCount';
    SEAudioMasterGetOutputPinCount:
      Result := 'SEAudioMasterGetOutputPinCount';
    SEAudioMasterGetPinVarAddress:
      Result := 'SEAudioMasterGetPinVarAddress';
    SEAudioMasterGetBlockStartClock:
      Result := 'SEAudioMasterGetBlockStartClock';
    SEAudioMasterGetTime:
      Result := 'SEAudioMasterGetTime';
    SEAudioMasterSleepMode:
      Result := 'SEAudioMasterSleepMode';
    SEAudioMasterGetRegisteredName:
      Result := 'SEAudioMasterGetRegisteredName';
    SEAudioMasterGetFirstClone:
      Result := 'SEAudioMasterGetFirstClone';
    SEAudioMasterGetNextClone:
      Result := 'SEAudioMasterGetNextClone';
    SEAudioMasterGetTotalPinCount:
      Result := 'SEAudioMasterGetTotalPinCount';
    SEAudioMasterCallVSTHost:
      Result := 'SEAudioMasterCallVSTHost';
    SEAudioMasterResolveFilename:
      Result := 'SEAudioMasterResolveFilename';
    SEAudioMasterSendStringToGui:
      Result := 'SEAudioMasterSendStringToGui';
    SEAudioMasterGetModuleHandle:
      Result := 'SEAudioMasterGetModuleHandle';
    SEAudioMasterAddEvent:
      Result := 'SEAudioMasterAddEvent';
    SEAudioMasterCreateSharedLookup:
      Result := 'SEAudioMasterCreateSharedLookup';
    SEAudioMasterSetPinOutputText:
      Result := 'SEAudioMasterSetPinOutputText';
    SEAudioMasterSetProcessFunction:
      Result := 'SEAudioMasterSetProcessFunction';
    SEAudioMasterResolveFilename2:
      Result := 'SEAudioMasterResolveFilename2';
    SEAudioMasterGetSeVersion:
      Result := 'SEAudioMasterGetSeVersion';
  else
    Result := 'Unknown opcode';
  end;
end;
{ Callbacks }

function SE2AudioMasterCallback(Effect: PSE2ModStructBase;
  Opcode: TSEHostOpcodes; Index, Value: Integer; Ptr: Pointer; Opt: Single)
  : Integer; cdecl;
begin
  Result := 0;
  if (Effect = nil) or not(TObject(Effect.HostPtr) is TCustomSEHostedModulePart)
  then
    Exit;
end;

function SEGuiCallback(Effect: PSEGUIStructBase; Opcode: TSEGuiHostOpcodes;
  Index, Value: Integer; Ptr: Pointer; Opt: Single): Integer; cdecl;
begin
  Result := 0;
  if (Effect = nil) or not(TObject(Effect.HostPtr) is TCustomSEHostedModulePart)
  then
    exit;

  with TCustomSEHostedModulePart(Effect.HostPtr) do
    case Opcode of
      seGuiHostRequestRepaint:
        GuiHostRepaintRequest;
      seGuiHostGetHandle:
        Result := GuiHostGetHandle;
      seGuiHostSendStringToAudio:
        Result := 0; // SendStringToAudio test
      seGuiHostSetWindowSize:
        GuiHostSetWindowSize(Index, Value);
      seGuiHostSetWindowSizeable:
        GuiHostSetWindowSizeable;
      seGuiHostGetTotalPinCount:
        Result := GuiHostGetTotalPinCount;
      seGuiHostPlugSetValText:
        Result := 0;
      seGuiHostPlugGetValText:
        Result := 0;
      seGuiHostAddGuiPlug:
        GuiHostAddGuiPlug;
      seGuiRegisterPatchParameter:
        Result := 0;
      seGuiHostGetFontInfo:
        Result := 0;
      seGuiHostSetWindowType:
        Result := 0;
      seGuiHostGetWindowHandle:
        Result := 0;
      seGuiHostSetWindowFlags:
        Result := 0;
      seGuiHostPlugGetVal:
        Result := 0;
      seGuiHostPlugSetVal:
        Result := 0;
      seGuiHostPlugSetExtraData:
        Result := 0; // sets enum list or file extension (depending on datatype)
      seGuiHostPlugGetExtraData:
        Result := 0;
      seGuiHostSetCapture:
        Result := 0; // see SEGUI_base::SetCapture(...)
      seGuiHostReleaseCapture:
        Result := 0;
      seGuiHostGetCapture:
        Result := 0;
      seGuiHostCallVSTHost:
        Result := 0; // pass se_call_vst_host_params structure in Ptr
      seGuiHostSetIdle:
        Result := 0;
      // pass 1 to receive regular calls to OnIdle(), pass zero to cancel
      seGuiHostGetModuleFilename:
        Result := 0; // returns full module path
      seGuiHostResolveFilename:
        GuiHostResolveFilename(Ptr, Value); // returns full module path
      seGuiHostGetHostType:
        Result := 0;
      // return code 0 = unsuported, 1 = module is running in SynthEdit, 2 = Module is in a VST plugin (made with SE)
      seGuiHostRemoveGuiPlug:
        Result := 0;
      seGuiHostGetParentContext:
        Result := 0;
      // Get 'handle' of parent window.  This is an SE handle, not an HWND. Use seGuiHostGetWindowHandle to convert.
      seGuiHostMapWindowPoints:
        Result := 0;
      // map a point on one window to the co-ordinate system of a 2nd window
      seGuiHostMapClientPointToScreen:
        Result := 0;
      // maps a point on your gui to the system screen (absolute co-ords)
      seGuiHostInvalidateRect:
        Result := 0; // invlalidate (cause redraw) of any SE window
      seGuiHostIsGraphInitialsed:
        Result := 0;
      // test if pin updates are due to file loading, or from user.
    else
      raise Exception.Create('Unknown Opcode');
    end;
end;

{ TCustomSEHostedModulePart }

constructor TCustomSEHostedModulePart.Create(Owner: TCustomSEHostedModule;
  Index: Integer = 0; Properties: PSEModuleProperties = nil);
begin
  inherited Create(TSEHostedModules(Owner.Collection).SEHost);
  FIndex := Index;
  FSE2ModStructBase := nil;
  FSEGUIStructBase := nil;
  FSEHostedModule := Owner;
  if Assigned(Properties) then
    FProperties := Properties^
  else
    FillChar(FProperties, SizeOf(TSEModuleProperties), 0);
end;

function TCustomSEHostedModulePart.GetAbout: AnsiString;
begin
  Result := Properties.About;
end;

function TCustomSEHostedModulePart.GetActive: Boolean;
begin
  Result := FSE2ModStructBase <> nil;
end;

function TCustomSEHostedModulePart.GetGUIVersion: Integer;
begin
  if Assigned(FSEGUIStructBase) then
    Result := FSEGUIStructBase.Version
  else
    Result := -1;
end;

function TCustomSEHostedModulePart.GetID: AnsiString;
begin
  Result := Properties.ID;
end;

function TCustomSEHostedModulePart.GetMagic: Integer;
begin
  Result := FSE2ModStructBase.Magic;
end;

function TCustomSEHostedModulePart.GetName: AnsiString;
begin
  Result := Properties.Name;
end;

function TCustomSEHostedModulePart.GetPinProperties(Index: Integer;
  var Pin: TSEPinProperties): Boolean;
begin
  if Active then
    Result := (CallPlugin(seffGetPinProperties, Index, 0, @Pin) <> 0)
  else
    Result := False
end;

function TCustomSEHostedModulePart.GetVersion: Integer;
begin
  Result := FSE2ModStructBase.Version;
end;

procedure TCustomSEHostedModulePart.GuiHostAddGuiPlug;
begin
  // do nothing yet;
end;

function TCustomSEHostedModulePart.GuiHostGetHandle: THandle;
begin
  Result := 0;
end;

function TCustomSEHostedModulePart.GuiHostGetTotalPinCount: Integer;
begin
  Result := 0; // ToDo
end;

procedure TCustomSEHostedModulePart.GuiHostRepaintRequest;
begin
  if Assigned(FOnRepaintRequest) then
    FOnRepaintRequest(Self);
end;

procedure TCustomSEHostedModulePart.GuiHostResolveFilename(FileName: PWideChar;
  MaxStringLength: Integer);
begin
  // do nothing yet
end;

procedure TCustomSEHostedModulePart.GuiHostSetWindowSize(const x, y: Integer);
begin
  // do nothing yet
end;

procedure TCustomSEHostedModulePart.GuiHostSetWindowSizeable;
begin
  // do nothing yet
end;

procedure TCustomSEHostedModulePart.GuiNotify(Value: Integer = 0;
  Index: Integer = 0; Ptr: Pointer = nil);
begin
  if Active then
    CallPlugin(seffGuiNotify, Index, Value, Ptr);
end;

procedure TCustomSEHostedModulePart.Instantiation;
begin
  try
    DisposeStructures;

    // Instantiate Module
    FSE2ModStructBase := FSEHostedModule.FMakeModule(FIndex, 1,
      @SE2AudioMasterCallback, Self);
    if Assigned(FSE2ModStructBase) then
      FSE2ModStructBase^.HostPtr := Self;

    // Instantiate GUI
    FSEGUIStructBase := FSEHostedModule.FMakeModule(FIndex, 2, @SEGuiCallback,
      Self); // nasty
    if Assigned(FSEGUIStructBase) then
      FSEGUIStructBase^.HostPtr := Self;

  except
    FSE2ModStructBase := nil;
  end;
end;

procedure TCustomSEHostedModulePart.DisposeStructures;
begin
  if Assigned(FSE2ModStructBase) then
  begin
    Dispose(FSE2ModStructBase);
    FSE2ModStructBase := nil;
  end;
  if Assigned(FSEGUIStructBase) then
  begin
    Dispose(FSEGUIStructBase);
    FSEGUIStructBase := nil;
  end;
end;

function TCustomSEHostedModulePart.IsEventListEmpty: Boolean;
begin
  if Properties.SdkVersion >= 2000 then
    raise Exception.Create('not used in SDK2')
  else if Active then
    Result := CallPlugin(seffIsEventListEmpty) <> 0
  else
    Result := False;
end;

procedure TCustomSEHostedModulePart.Open;
begin
  if Active then
    CallPlugin(seffOpen);
end;

function TCustomSEHostedModulePart.QueryDebugInfo: Pointer;
begin
  if Active then
    Result := Pointer(CallPlugin(seffQueryDebugInfo))
  else
    Result := nil;
end;

procedure TCustomSEHostedModulePart.Resume(Index: Integer = 0);
begin
  if Active then
    CallPlugin(seffResume, Index);
end;

procedure TCustomSEHostedModulePart.AddEvent(Event: TSEEvent);
begin
  if Properties.SdkVersion >= 2000 then
    raise Exception.Create('not used in SDK2')
  else if Active then
    CallPlugin(seffAddEvent, 0, 0, @Event);
end;

function TCustomSEHostedModulePart.CallPlugin(Opcode: TSEPluginModuleOpcodes;
  Index: Integer = 0; Value: Integer = 0; Ptr: Pointer = nil;
  Opt: Single = 0): Integer;
begin
  try
    if Active then
      Result := FSE2ModStructBase.Dispatcher(FSE2ModStructBase, Opcode, Index,
        Value, Ptr, Opt)
    else
      raise Exception.Create('SE module part not opened yet');
  except
    Result := 0;
  end;
end;

procedure TCustomSEHostedModulePart.Close;
begin
  try
    if Active then
      CallPlugin(seffClose);
  finally
    (*
      if Assigned(FSE2ModStructBase)
      then Dispose(FSE2ModStructBase);
    *)
    FSE2ModStructBase := nil;
  end;
end;

procedure TCustomSEHostedModulePart.SetActive(const Value: Boolean);
begin
  if Active <> Value then
    if Value then
      Open
    else
      Close;
end;

procedure TCustomSEHostedModulePart.SetBlockSize(Value: Integer);
begin
  if Active then
    CallPlugin(seffSetBlockSize, 0, Value);
end;

procedure TCustomSEHostedModulePart.SetSampleRate(Value: Single);
begin
  if Active then
    CallPlugin(seffSetSampleRate, 0, 0, nil, Value);
end;

{ TCustomSEHostedModule }

constructor TCustomSEHostedModule.Create(Collection: TCollection);
begin
  inherited;
  FDisplayName := inherited GetDisplayName;
  InitializeVariables;
end;

destructor TCustomSEHostedModule.Destroy;
begin
  UnLoad;
  inherited;
end;

procedure TCustomSEHostedModule.AssignTo(Dest: TPersistent);
begin
  if Dest is TCustomSEHostedModule then
    with TCustomSEHostedModule(Dest) do
    begin
      DisplayName := Self.DisplayName;
      FSEMFileName := Self.FSEMFileName;
    end
  else
    inherited;
end;

procedure TCustomSEHostedModule.CloseParts;
var
  Module: Integer;
begin
  for Module := 0 to Length(FParts) - 1 do
    try
      if FParts[Module] <> nil then
        FParts[Module].Close;
    finally
      FreeAndNil(FParts[Module]);
    end;
end;

function TCustomSEHostedModule.GetDisplayName: string;
begin
  Result := FDisplayName;
end;

function TCustomSEHostedModule.GetPart(Index: Integer)
  : TCustomSEHostedModulePart;
begin
  if (Index >= 0) and (Index < Length(FParts)) then
    Result := FParts[Index]
  else
    Result := nil;
end;

function TCustomSEHostedModule.GetPartCount: Integer;
begin
  Result := Length(FParts);
end;

procedure TCustomSEHostedModule.LoadFromFile(FileName: TFileName);
{$IFNDEF FPC}
var
  Buf: array [0 .. 255] of Char;
  LE: Integer;
  Str: string;
{$ENDIF}
begin
  if not FileExists(FileName) then
    raise Exception.CreateFmt(RStrFileDoesNotExist, [FileName]);

  if FLoaded then
    UnLoad;

  FSEMFileName := FileName;

  try
    FSEModuleHandle := SafeLoadLibrary(PChar(FileName), 7);
    if FSEModuleHandle = 0 then
    begin
{$IFNDEF FPC}
      LE := GetLastError;
      FormatMessage(FORMAT_MESSAGE_FROM_SYSTEM, nil, LE, 0, @Buf[0],
        SizeOf(Buf), nil);
      if Buf = '' then
      begin
        Str := IntToStr(LE) + StrPas(Buf);
        raise Exception.Create(Str);
      end
      else
        raise Exception.Create(StrPas(Buf));
{$ENDIF}
    end
    else // GetProcAddresses
    begin
      FGetModuleProperties := GetProcAddress(FSEModuleHandle,
        'getModuleProperties');
      FMakeModule := GetProcAddress(FSEModuleHandle, 'makeModule');

      if not Assigned(FGetModuleProperties) then
        raise Exception.Create(RStrNoEntryPoint);
      if not Assigned(FMakeModule) then
        raise Exception.Create(RStrNoEntryPoint);
      ListParts;
    end;
    FLoaded := True;
  except
    UnLoad;
  end;
end;

procedure TCustomSEHostedModule.ListParts;
var
  ModuleProperties: TSEModuleProperties;
  Module: Integer;
begin
  CloseParts;
  Module := 0;
  SetLength(FParts, 0);
  FillChar(ModuleProperties, SizeOf(TSEModuleProperties), 0);
  while FGetModuleProperties(Module, @ModuleProperties) do
  begin
    SetLength(FParts, Module + 1);
    FParts[Module] := TCustomSEHostedModulePart.Create(Self, Module,
      @ModuleProperties);
    TSEHostedModules(Collection).SEHost.InsertComponent(FParts[Module]);
    Inc(Module);
    FillChar(ModuleProperties, SizeOf(TSEModuleProperties), 0);
  end;
end;

procedure TCustomSEHostedModule.SetSEMFileName(const Value: TFileName);
begin
  if FSEMFileName <> Value then
    if FileExists(Value) then
      LoadFromFile(Value)
    else
      UnLoad;
end;

procedure TCustomSEHostedModule.UnLoad;
begin
  CloseParts;

  if FSEModuleHandle > 0 then
    try
      FreeLibrary(FSEModuleHandle);
    finally
      FSEModuleHandle := 0;
    end;
  FLoaded := False;
  InitializeVariables;
end;

procedure TCustomSEHostedModule.InitializeVariables;
begin
  FSEMFileName := '';
  FGetModuleProperties := nil;
  FMakeModule := nil;
end;

{ TSEHostedModules }

constructor TSEHostedModules.Create(AOwner: TComponent);
begin
  inherited Create(AOwner, TSEHostedModule);
  FOwner := AOwner;
end;

function TSEHostedModules.Add: TSEHostedModule;
begin
  Result := TSEHostedModule(inherited Add);
end;

function TSEHostedModules.CloneAdd(Source: TSEHostedModule): TSEHostedModule;
begin
  Result := TSEHostedModule(inherited Add);
  Source.AssignTo(Result);
end;

procedure TSEHostedModules.Delete(Index: Integer);
begin
  inherited Delete(Index);
end;

function TSEHostedModules.GetItem(Index: Integer): TSEHostedModule;
begin
  Result := TSEHostedModule(inherited GetItem(Index));
end;

function TSEHostedModules.GetSEHost: TCustomSEHost;
begin
  Result := TCustomSEHost(FOwner);
end;

function TSEHostedModules.Insert(Index: Integer): TSEHostedModule;
begin
  Result := TSEHostedModule(inherited Insert(Index));
end;

procedure TSEHostedModules.SetItem(Index: Integer;
  const Value: TSEHostedModule);
begin
  inherited SetItem(Index, Value);
end;

{ TCustomSEHost }

constructor TCustomSEHost.Create(AOwner: TComponent);
begin
  inherited;
  FSampleRate := 44100;
  FBlockSize := 2048;
  FSEHostedModules := TSEHostedModules.Create(Self);
  if Assigned(FOnCreate) then
    FOnCreate(Self);
end;

destructor TCustomSEHost.Destroy;
begin
  if Assigned(FOnDestroy) then
    FOnDestroy(Self);
  FreeAndNil(FSEHostedModules);
  inherited;
end;

function TCustomSEHost.GetItem(Index: Integer): TCustomSEHostedModule;
begin
  assert(Assigned(FSEHostedModules));
  Result := FSEHostedModules[Index];
end;

procedure TCustomSEHost.SetHostedModules(const Value: TSEHostedModules);
begin
  assert(Assigned(FSEHostedModules));
  FSEHostedModules.Assign(Value);
end;

procedure Register;
begin
  RegisterComponents('ASIO/VST Basics', [TSEHost]);
end;

initialization

RegisterClass(TCustomSEHostedModulePart);

end.
