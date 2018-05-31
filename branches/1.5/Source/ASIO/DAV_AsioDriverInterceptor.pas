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

unit DAV_ASIODriverInterceptor;

interface

uses Classes, SysUtils, Windows, Forms,
  DAV_ASIO, DAV_ASIOList, DAV_ASIODriver, DAV_AsioInterface;

type
{$IFDEF DELPHI10_UP} {$REGION 'Interceptor declaration'} {$ENDIF}
  TDavASIOInterceptor = class(TDavASIODriver)
  protected
    FDriverName: string;
    FDriverVersion: Longint;
    FDriverList: TDAVAsioDriverList;
    FHostInterface: IStdCallASIO;
    FDriverIndex: integer;
    FHostCallbacks: PASIOCallbacks;
    FDriverCallbacks: TASIOCallbacks;

    procedure UnloadHostInterface;
    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: Longint);
    function GetDriverNames: TStrings;
    procedure SetDriverIndex(index: integer);
    procedure LoadDriverSettings; virtual;
    procedure SaveDriverSettings; virtual;
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper;
      InterfaceGUID: TGuid); override;
    destructor Destroy; override;

    function Init(SysHandle: HWND): boolean; override;
    function GetDriverName: string; override;
    function GetDriverVersion: Longint; override;
    function GetErrorMessage: string; override;
    function Start: TASIOError; override;
    function Stop: TASIOError; override;
    function GetChannels(out NumInputChannels, NumOutputChannels: Longint)
      : TASIOError; override;
    function GetLatencies(out InputLatency, OutputLatency: Longint)
      : TASIOError; override;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize,
      Granularity: Longint): TASIOError; override;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetSampleRate(out SampleRate: TASIOSampleRate)
      : TASIOError; override;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetClockSources(Clocks: PASIOClockSources; out NumSources: Longint)
      : TASIOError; override;
    function SetClockSource(Reference: Longint): TASIOError; override;
    function GetSamplePosition(out SamplePosition: TASIOSamples;
      out TimeStamp: TASIOTimeStamp): TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfos;
      NumChannels, BufferSize: Longint; const Callbacks: TASIOCallbacks)
      : TASIOError; override;
    function DisposeBuffers: TASIOError; override;
    function ControlPanel: TASIOError; override;
    function Future(Selector: Longint; Opt: Pointer): TASIOError; override;
    function OutputReady: TASIOError; override;

    function DriverControlPanel: TASIOError;

    procedure ASIOBufferSwitch(DoubleBufferIndex: integer;
      DirectProcess: TASIOBool); virtual;
    function ASIOBufferSwitchTimeInfo(var Params: TASIOTime;
      DoubleBufferIndex: integer; DirectProcess: TASIOBool): PASIOTime; virtual;
    procedure ASIOSampleRateDidChange(SampleRate: TASIOSampleRate); virtual;
    function ASIOMessage(Selector, Value: integer; msg: Pointer; Opt: PDouble)
      : integer; virtual;

    procedure ASIORequestReset;

    property DriverNames: TStrings read GetDriverNames;
    property DriverIndex: integer read FDriverIndex write SetDriverIndex;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Interceptor declaration'} {$ENDIF}

implementation

var
  GlobalCallbackInst: TDavASIOInterceptor;

{$IFDEF DELPHI10_UP} {$REGION 'Callback methods'} {$ENDIF}

procedure callbackBufferSwitch(DoubleBufferIndex: integer;
  DirectProcess: TASIOBool); cdecl;
begin
  if Assigned(GlobalCallbackInst) then
    GlobalCallbackInst.ASIOBufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function callbackBufferSwitchTimeInfo(var Params: TASIOTime;
  DoubleBufferIndex: integer; DirectProcess: TASIOBool): PASIOTime; cdecl;
begin
  if Assigned(GlobalCallbackInst) then
    Result := GlobalCallbackInst.ASIOBufferSwitchTimeInfo(Params,
      DoubleBufferIndex, DirectProcess)
  else
    Result := @Params; // dummy
end;

procedure callbackSampleRateDidChange(SampleRate: TASIOSampleRate); cdecl;
begin
  if Assigned(GlobalCallbackInst) then
    GlobalCallbackInst.ASIOSampleRateDidChange(SampleRate);
end;

function callbackMessage(Selector, Value: integer; msg: Pointer; Opt: PDouble)
  : integer; cdecl;
begin
  if Assigned(GlobalCallbackInst) then
    Result := GlobalCallbackInst.ASIOMessage(Selector, Value, msg, Opt)
  else
    Result := 0;
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Callback methods'} {$ENDIF}
{ TDavASIOInterceptor }

{$IFDEF DELPHI10_UP} {$REGION 'Interceptor implementation'} {$ENDIF}

constructor TDavASIOInterceptor.Create(TCWrapper: TDavASIOTCWrapper;
  InterfaceGUID: TGuid);
begin
  inherited;
  GlobalCallbackInst := self;
  FDriverList := TDAVAsioDriverList.Create(InterfaceGUID);
  FDriverList.UpdateList;
  FDriverIndex := 0;
  FHostInterface := nil;
  FDriverName := 'DAV Abstract Int';
  FDriverVersion := 1;

  FHostCallbacks := nil;
  with FDriverCallbacks do
  begin
    BufferSwitch := callbackBufferSwitch;
    SampleRateDidChange := callbackSampleRateDidChange;
    BufferSwitchTimeInfo := callbackBufferSwitchTimeInfo;
    ASIOMessage := callbackMessage;
  end;

  InitializeDriverParams;
  LoadDriverSettings;

  InitControlPanel;
end;

destructor TDavASIOInterceptor.Destroy;
begin
  UnloadHostInterface;
  FDriverList.Free;
  inherited;
end;

procedure TDavASIOInterceptor.InitializeDriverParams;
begin
  raise Exception.Create('You have to overwrite InitializeDriverParams');
end;

procedure TDavASIOInterceptor.SetDriverName(name: string);
begin
  FDriverName := name;
end;

procedure TDavASIOInterceptor.SetDriverVersion(version: Longint);
begin
  FDriverVersion := version;
end;

procedure TDavASIOInterceptor.SetDriverIndex(index: integer);
begin
  if FDriverIndex = index then
    exit;
  // range check is done in the init method
  FDriverIndex := index;
  if Assigned(FHostInterface) then
    ASIORequestReset;
end;

procedure TDavASIOInterceptor.LoadDriverSettings;
begin
  // this is default: does nothing
end;

procedure TDavASIOInterceptor.SaveDriverSettings;
begin
  // this is default: does nothing
end;

procedure TDavASIOInterceptor.UnloadHostInterface;
begin
  if not Assigned(FHostInterface) then
    exit;
  FHostInterface.Stop;
  FHostInterface.DisposeBuffers;
  FHostInterface := nil;
end;

function TDavASIOInterceptor.GetDriverNames: TStrings;
begin
  result := FDriverList.DriverNames;
end;

function TDavASIOInterceptor.Init(SysHandle: HWND): boolean;
begin
  UnloadHostInterface;
  if FDriverIndex > FDriverList.Count - 1 then
    Result := false
  else
  begin
    fParentWindowHandle := SysHandle;
    try
      Result := CreateStdCallASIO
        (TDAVAsioDriverDesc(FDriverList.Items[FDriverIndex]).Guid,
        FHostInterface);
      if Result then
        Result := FHostInterface.Init(SysHandle) = ASIOTrue;
    except
      Result := false;
      FHostInterface := nil;
    end;
  end;
end;

function TDavASIOInterceptor.GetDriverName: string;
begin
  result := FDriverName;
end;

function TDavASIOInterceptor.GetDriverVersion: Longint;
begin
  result := FDriverVersion;
end;

function TDavASIOInterceptor.GetErrorMessage: string;
var
  tmp: array [0 .. 124] of char;
begin
  result := '';
  if Assigned(FHostInterface) then
    try
      FHostInterface.GetErrorMessage(tmp);
      result := tmp;
    except
    end;
end;

function TDavASIOInterceptor.Start: TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.Start;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.Stop: TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.Stop;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetChannels(out NumInputChannels, NumOutputChannels
  : integer): TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetChannels(NumInputChannels, NumOutputChannels);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetLatencies(out InputLatency,
  OutputLatency: integer): TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetLatencies(InputLatency, OutputLatency);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetBufferSize(out MinSize, MaxSize, PreferredSize,
  Granularity: integer): TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetBufferSize(MinSize, MaxSize, PreferredSize,
      Granularity);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CanSampleRate(SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.CanSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSampleRate(out SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetSampleRate(SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.SetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetClockSources(Clocks: PASIOClockSources;
  out NumSources: integer): TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetClockSources(Clocks, NumSources);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetClockSource(Reference: integer): TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.SetClockSource(Reference);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSamplePosition(out SamplePosition: TASIOSamples;
  out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetSamplePosition(SamplePosition, TimeStamp);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetChannelInfo(var Info: TASIOChannelInfo)
  : TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.GetChannelInfo(Info);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CreateBuffers(BufferInfos: PASIOBufferInfos;
  NumChannels, BufferSize: integer; const Callbacks: TASIOCallbacks)
  : TASIOError;
begin
  FHostCallbacks := @Callbacks;

  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.CreateBuffers(BufferInfos, NumChannels, BufferSize,
      FDriverCallbacks);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.DisposeBuffers: TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.DisposeBuffers;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.ControlPanel: TASIOError;
begin
  result := inherited ControlPanel;

  if result <> ASE_OK then
    result := DriverControlPanel;
end;

function TDavASIOInterceptor.DriverControlPanel: TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.ControlPanel;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.Future(Selector: integer; Opt: Pointer)
  : TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.Future(Selector, Opt);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.OutputReady: TASIOError;
begin
  if not Assigned(FHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := FHostInterface.OutputReady;
  except
    result := ASE_NotPresent;
  end;
end;

procedure TDavASIOInterceptor.ASIOBufferSwitch(DoubleBufferIndex: integer;
  DirectProcess: TASIOBool);
begin
  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.BufferSwitch) then
    FHostCallbacks^.BufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function TDavASIOInterceptor.ASIOBufferSwitchTimeInfo(var Params: TASIOTime;
  DoubleBufferIndex: integer; DirectProcess: TASIOBool): PASIOTime;
begin
  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.BufferSwitchTimeInfo)
  then
    result := FHostCallbacks^.BufferSwitchTimeInfo(Params, DoubleBufferIndex,
      DirectProcess)
  else
    result := @Params; // dummy
end;

procedure TDavASIOInterceptor.ASIOSampleRateDidChange
  (SampleRate: TASIOSampleRate);
begin
  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.SampleRateDidChange)
  then
    FHostCallbacks^.SampleRateDidChange(SampleRate);
end;

function TDavASIOInterceptor.ASIOMessage(Selector, Value: integer; msg: Pointer;
  Opt: PDouble): integer;
begin
  if Selector = kAsioResetRequest then
    SaveDriverSettings;

  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.ASIOMessage) then
    result := FHostCallbacks^.ASIOMessage(Selector, Value, msg, Opt)
  else
    result := 0;
end;

procedure TDavASIOInterceptor.ASIORequestReset;
begin
  ASIOMessage(kAsioResetRequest, 0, nil, nil);
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Interceptor implementation'} {$ENDIF}

initialization

GlobalCallbackInst := nil;

end.
