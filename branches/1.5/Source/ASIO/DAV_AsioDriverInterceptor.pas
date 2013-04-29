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
    fDriverName: string;
    fDriverVersion: Longint;
    fDriverList: TDAVAsioDriverList;
    fHostInterface: IStdCallASIO;
    fDriverIndex: integer;
    fHostCallbacks: PASIOCallbacks;
    fDriverCallbacks: TASIOCallbacks;

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
    property DriverIndex: integer read fDriverIndex write SetDriverIndex;
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
    result := GlobalCallbackInst.ASIOBufferSwitchTimeInfo(Params,
      DoubleBufferIndex, DirectProcess)
  else
    result := @Params; // dummy
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
    result := GlobalCallbackInst.ASIOMessage(Selector, Value, msg, Opt)
  else
    result := 0;
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Callback methods'} {$ENDIF}
{ TDavASIOInterceptor }

{$IFDEF DELPHI10_UP} {$REGION 'Interceptor implementation'} {$ENDIF}

constructor TDavASIOInterceptor.Create(TCWrapper: TDavASIOTCWrapper;
  InterfaceGUID: TGuid);
begin
  inherited;
  GlobalCallbackInst := self;
  fDriverList := TDAVAsioDriverList.Create(InterfaceGUID);
  fDriverList.UpdateList;
  fDriverIndex := 0;
  fHostInterface := nil;
  fDriverName := 'DAV Abstract Int';
  fDriverVersion := 1;

  fHostCallbacks := nil;
  with fDriverCallbacks do
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
  fDriverList.Free;
  inherited;
end;

procedure TDavASIOInterceptor.InitializeDriverParams;
begin
  raise Exception.Create('You have to overwrite InitializeDriverParams');
end;

procedure TDavASIOInterceptor.SetDriverName(name: string);
begin
  fDriverName := name;
end;

procedure TDavASIOInterceptor.SetDriverVersion(version: Longint);
begin
  fDriverVersion := version;
end;

procedure TDavASIOInterceptor.SetDriverIndex(index: integer);
begin
  if fDriverIndex = index then
    exit;
  // range check is done in the init method
  fDriverIndex := index;
  if Assigned(fHostInterface) then
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
  if not Assigned(fHostInterface) then
    exit;
  fHostInterface.Stop;
  fHostInterface.DisposeBuffers;
  fHostInterface := nil;
end;

function TDavASIOInterceptor.GetDriverNames: TStrings;
begin
  result := fDriverList.DriverNames;
end;

function TDavASIOInterceptor.Init(SysHandle: HWND): boolean;
begin
  UnloadHostInterface;
  if fDriverIndex > fDriverList.Count - 1 then
    result := false
  else
  begin
    fParentWindowHandle := SysHandle;
    try
      result := CreateStdCallASIO
        (TDAVAsioDriverDesc(fDriverList.Items[fDriverIndex]).Guid,
        fHostInterface);
      if result then
        result := fHostInterface.Init(SysHandle) = ASIOTrue;
    except
      result := false;
      fHostInterface := nil;
    end;
  end;
end;

function TDavASIOInterceptor.GetDriverName: string;
begin
  result := fDriverName;
end;

function TDavASIOInterceptor.GetDriverVersion: Longint;
begin
  result := fDriverVersion;
end;

function TDavASIOInterceptor.GetErrorMessage: string;
var
  tmp: array [0 .. 124] of char;
begin
  result := '';
  if Assigned(fHostInterface) then
    try
      fHostInterface.GetErrorMessage(tmp);
      result := tmp;
    except
    end;
end;

function TDavASIOInterceptor.Start: TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.Start;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.Stop: TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.Stop;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetChannels(out NumInputChannels, NumOutputChannels
  : integer): TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetChannels(NumInputChannels, NumOutputChannels);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetLatencies(out InputLatency,
  OutputLatency: integer): TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetLatencies(InputLatency, OutputLatency);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetBufferSize(out MinSize, MaxSize, PreferredSize,
  Granularity: integer): TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetBufferSize(MinSize, MaxSize, PreferredSize,
      Granularity);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CanSampleRate(SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.CanSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSampleRate(out SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetSampleRate(SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.SetSampleRate(SampleRate);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetClockSources(Clocks: PASIOClockSources;
  out NumSources: integer): TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetClockSources(Clocks, NumSources);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.SetClockSource(Reference: integer): TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.SetClockSource(Reference);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetSamplePosition(out SamplePosition: TASIOSamples;
  out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetSamplePosition(SamplePosition, TimeStamp);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.GetChannelInfo(var Info: TASIOChannelInfo)
  : TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.GetChannelInfo(Info);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.CreateBuffers(BufferInfos: PASIOBufferInfos;
  NumChannels, BufferSize: integer; const Callbacks: TASIOCallbacks)
  : TASIOError;
begin
  fHostCallbacks := @Callbacks;

  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.CreateBuffers(BufferInfos, NumChannels, BufferSize,
      fDriverCallbacks);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.DisposeBuffers: TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.DisposeBuffers;
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
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.ControlPanel;
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.Future(Selector: integer; Opt: Pointer)
  : TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.Future(Selector, Opt);
  except
    result := ASE_NotPresent;
  end;
end;

function TDavASIOInterceptor.OutputReady: TASIOError;
begin
  if not Assigned(fHostInterface) then
  begin
    result := ASE_NotPresent;
    exit;
  end;

  try
    result := fHostInterface.OutputReady;
  except
    result := ASE_NotPresent;
  end;
end;

procedure TDavASIOInterceptor.ASIOBufferSwitch(DoubleBufferIndex: integer;
  DirectProcess: TASIOBool);
begin
  if Assigned(fHostCallbacks) and Assigned(fHostCallbacks^.BufferSwitch) then
    fHostCallbacks^.BufferSwitch(DoubleBufferIndex, DirectProcess);
end;

function TDavASIOInterceptor.ASIOBufferSwitchTimeInfo(var Params: TASIOTime;
  DoubleBufferIndex: integer; DirectProcess: TASIOBool): PASIOTime;
begin
  if Assigned(fHostCallbacks) and Assigned(fHostCallbacks^.BufferSwitchTimeInfo)
  then
    result := fHostCallbacks^.BufferSwitchTimeInfo(Params, DoubleBufferIndex,
      DirectProcess)
  else
    result := @Params; // dummy
end;

procedure TDavASIOInterceptor.ASIOSampleRateDidChange
  (SampleRate: TASIOSampleRate);
begin
  if Assigned(fHostCallbacks) and Assigned(fHostCallbacks^.SampleRateDidChange)
  then
    fHostCallbacks^.SampleRateDidChange(SampleRate);
end;

function TDavASIOInterceptor.ASIOMessage(Selector, Value: integer; msg: Pointer;
  Opt: PDouble): integer;
begin
  if Selector = kAsioResetRequest then
    SaveDriverSettings;

  if Assigned(fHostCallbacks) and Assigned(fHostCallbacks^.ASIOMessage) then
    result := fHostCallbacks^.ASIOMessage(Selector, Value, msg, Opt)
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
