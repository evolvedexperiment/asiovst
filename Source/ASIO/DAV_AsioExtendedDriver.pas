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

unit DAV_ASIOExtendedDriver;

interface

uses Classes, messages, windows, forms, DAV_ASIO, DAV_ASIODriver;

type
  TDavASIOExtendedDriver = class;

  TDavASIOExtDrvrDoubleBuffer = array [0 .. 1] of Pointer;

  TDavASIOExtDrvrTimings = record
    SystemTime: TASIOTimeStamp;
    SamplePos: TASIOSamples;
  end;

  TDavASIOExtDrvrSupported = record
    RequestsDone, // only true, when all requests to the host are made
    EngineVersion, ResetRequest, BufferSizeChange, ResyncRequest,
      LatenciesChanged, SupportsTimeInfo, SupportsTimeCode: Boolean;
  end;

{$IFDEF DELPHI10_UP} {$REGION 'Processing thread declaration'} {$ENDIF}

  TDavASIOExtDrvrProcessingThread = class(TThread)
  protected
    procedure Execute; override;
  public
    Driver: TDavASIOExtendedDriver;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Processing thread declaration'} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'Buffersizes declaration'} {$ENDIF}

  TDavASIOExtDrvrBufferSizes = record
    Minimum, Maximum, Prefered, Granularity, Current: LongInt;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Buffersizes declaration'} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'Clock list declaration'} {$ENDIF}

  TDavASIOExtDrvrClockListItem = class
    ClockName: string;
    ChannelGroup: LongInt;
    IsCurrentSource: Boolean;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Clock list declaration'} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'Channel list declaration'} {$ENDIF}

  TDavASIOExtDrvrChannelListItem = class
    IsActive: Boolean;
    ChannelGroup: LongInt;
    SampleType: TASIOSampleType;
    ChannelName: string;
    DoubleBuffer: TDavASIOExtDrvrDoubleBuffer;
    constructor Create(cname: string; cchannelgroup: LongInt;
      cSampleType: TASIOSampleType; cIsInput: Boolean);
    destructor Destroy; override;
    function CreateBuffers(out Buffers: array of Pointer;
      size: Integer): Boolean;
    procedure DisposeBuffers;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Channel list declaration'} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'Samplerate list declaration'} {$ENDIF}

  TDavASIOExtDrvrSampleRateItem = class
    SampleRate: Double;
  end;

  TTDavASIOExtDrvrSampleRateMode = (edsrm_Single, // Only one sample rate
    edsrm_Range, // A range of sample rates eg. 11025..44100
    edsrm_List, // A list of sample rates eg. 11025,22050,44100
    edsrm_All); // Sample rate doesn't matter, everything is accepted

  TTDavASIOExtDrvrSampleRateManager = class
  private
    FSampleRateMode: TTDavASIOExtDrvrSampleRateMode;
    FSampleRateList: TList;
    FDefaultSampleRate: Double;
  public
    constructor Create;
    destructor Destroy; override;

    procedure AddSampleRate(sr: Double);
    procedure SetDefaultSampleRate(sr: Double);
    procedure SetSampleRateMode(md: TTDavASIOExtDrvrSampleRateMode);
    function CanSampleRate(sr: Double): Boolean;
    function GetDefaultSampleRate: Double;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Samplerate list declaration'} {$ENDIF}
{$IFDEF DELPHI10_UP} {$REGION 'Extended driver declaration'} {$ENDIF}

  TDavASIOExtendedDriver = class(TDavASIODriver)
  protected
    FDriverName: string;
    FDriverVersion: LongInt;
    FLastErrorMsg: string;
    FSampleRateManager: TTDavASIOExtDrvrSampleRateManager;
    FClockList: TList;
    FInChannelList: TList;
    FOutChannelList: TList;
    FBufferSize: TDavASIOExtDrvrBufferSizes;
    FClocksAreDefault: Boolean;
    FChannelsAreDefault: Boolean;
    FSampleRate: Double;
    FInputLatency: LongInt;
    FOutputLatency: LongInt;
    FHostCallbacks: PASIOCallbacks;
    FBuffersCreated: Boolean;
    FSupportedSelectors: TDavASIOExtDrvrSupported;
    FHostEngineVersion: LongInt;
    FSupportsTimeInfo: Boolean;
    FSupportsTimeCode: Boolean;
    FAsioTime: TASIOTime;
    FSwitchTimings: TDavASIOExtDrvrTimings;
    FIsProcessing: Boolean;
    FProcessingThread: TDavASIOExtDrvrProcessingThread;
    FCurrentBuffer: LongInt;

    procedure InitializeDriverParams; virtual;
    procedure SetDriverName(name: string);
    procedure SetDriverVersion(version: LongInt);
    procedure SetErrorMessage(s: string);
    procedure AddClock(name: string; ChannelGroup: LongInt);
    procedure AddChannel(name: string; ChannelGroup: LongInt;
      SampleType: TASIOSampleType; IsInput: Boolean);
    procedure ChangeClockSource(fromIndex, ToIndex: LongInt); virtual;
    function GetCurrentClockSource: Integer;
    procedure AddSampleRate(sr: Double);
    procedure SetSampleRateMode(md: TTDavASIOExtDrvrSampleRateMode);
    function CheckBufferSize(test: Integer): Boolean;

    procedure ClearClockList;
    procedure ClearChannelLists;
    function GetFirstGroupChannel(GroupNr: LongInt; IsInput: Boolean): LongInt;

    procedure ClearSupportedSelectors; virtual;
    procedure CheckSupportedSelectors; virtual;
    procedure InitializeTimeInfo; virtual;

    function GetTimecodeSamples: TASIOSamples; virtual;
    procedure UpdateTimings; virtual;
    procedure StartProcessingThread; virtual;
    procedure StopProcessingThread; virtual;

    procedure ProcessBuffers; virtual;

    procedure LoadDriverSettings; virtual;
    procedure SaveDriverSettings; virtual;
  public
    constructor Create(TCWrapper: TDavASIOTCWrapper;
      InterfaceGUID: TGuid); override;
    destructor Destroy; override;

    function GetClockSources(Clocks: PASIOClockSources; out NumSources: LongInt)
      : TASIOError; override;
    function SetClockSource(Reference: LongInt): TASIOError; override;
    function GetSamplePosition(out SamplePosition: TASIOSamples;
      out TimeStamp: TASIOTimeStamp): TASIOError; override;
    procedure SetBufferSizes(MinSize, MaxSize, PreferredSize,
      Granularity: LongInt); virtual;
    procedure SetLatencies(InLatency, OutLatency: LongInt); virtual;
    procedure GetNanoSeconds(var Time: TASIOTimeStamp);

    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt)
      : TASIOError; override;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; override;
    function GetDriverName: string; override;
    function GetDriverVersion: LongInt; override;
    function GetErrorMessage: string; override;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize,
      Granularity: LongInt): TASIOError; override;
    function CreateBuffers(BufferInfos: PASIOBufferInfos;
      NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks)
      : TASIOError; override;
    function DisposeBuffers: TASIOError; override;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; override;
    function GetSampleRate(out nSampleRate: TASIOSampleRate)
      : TASIOError; override;
    function SetSampleRate(nSampleRate: TASIOSampleRate): TASIOError; override;
    function GetLatencies(out InLatency, OutLatency: LongInt)
      : TASIOError; override;
    function Start: TASIOError; override;
    function Stop: TASIOError; override;

    procedure ASIOBufferSwitch(DoubleBufferIndex: Integer;
      DirectProcess: TASIOBool); virtual;
    procedure ASIOBufferSwitchTimeInfo(DoubleBufferIndex: Integer;
      DirectProcess: TASIOBool); virtual;
    procedure ASIOSampleRateDidChange(SampleRate: TASIOSampleRate); virtual;
    function ASIOMessage(Selector, Value: Integer; msg: Pointer; Opt: PDouble)
      : Integer; virtual;
    procedure ASIORequestReset;
    procedure ASIOResyncRequest;
    procedure ASIOBufferSizeChange(newsize: LongInt);
    procedure ASIOLatenciesChanged;

    property SampleRate: Double read FSampleRate;
    property BufferSize: TDavASIOExtDrvrBufferSizes read FBufferSize;
    property HostEngineVersion: LongInt read FHostEngineVersion;
  end;
{$IFDEF DELPHI10_UP} {$ENDREGION 'Extended driver declaration'} {$ENDIF}

implementation

uses SysUtils, Math, MMSystem;

{ TDavASIOExtDrvrProcessingThread }

{$IFDEF DELPHI10_UP} {$REGION 'Processing thread implementation'} {$ENDIF}

procedure TDavASIOExtDrvrProcessingThread.Execute;
begin
  while not Terminated do
    if Assigned(Driver) then
      Driver.ProcessBuffers
    else
      Terminate;
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Processing thread implementation'} {$ENDIF}
{ TDavASIOExtDrvrChannelListItem }

{$IFDEF DELPHI10_UP} {$REGION 'Channel list implementation'} {$ENDIF}

constructor TDavASIOExtDrvrChannelListItem.Create(cname: string;
  cchannelgroup: Integer; cSampleType: TASIOSampleType; cIsInput: Boolean);
begin
  ChannelName := copy(cname, 0, 32);
  ChannelGroup := cchannelgroup;
  SampleType := cSampleType;
  IsActive := false;
  DoubleBuffer[0] := nil;
  DoubleBuffer[1] := nil;
end;

destructor TDavASIOExtDrvrChannelListItem.Destroy;
begin
  DisposeBuffers;
  inherited;
end;

function TDavASIOExtDrvrChannelListItem.CreateBuffers
  (out Buffers: array of Pointer; size: Integer): Boolean;
var
  samplesize: Integer;
begin
  if IsActive then
    DisposeBuffers;

  samplesize := 4;
  case SampleType of
    ASIOSTDSDInt8LSB1, ASIOSTDSDInt8MSB1, ASIOSTDSDInt8NER8:
      samplesize := 1;
    ASIOSTInt16MSB, ASIOSTInt16LSB:
      samplesize := 2;
    ASIOSTInt24MSB, ASIOSTInt24LSB:
      samplesize := 3;
    ASIOSTFloat64MSB, ASIOSTFloat64LSB:
      samplesize := 8;
  end;

  GetMem(DoubleBuffer[0], size * samplesize);
  GetMem(DoubleBuffer[1], size * samplesize);

  if not Assigned(DoubleBuffer[0]) or not Assigned(DoubleBuffer[1]) then
  begin
    IsActive := false;
    if Assigned(DoubleBuffer[0]) then
      FreeMem(DoubleBuffer[0]);
    if Assigned(DoubleBuffer[1]) then
      FreeMem(DoubleBuffer[1]);
  end
  else
  begin
    IsActive := true;
    Buffers[0] := DoubleBuffer[0];
    Buffers[1] := DoubleBuffer[1];
  end;

  Result := IsActive;
end;

procedure TDavASIOExtDrvrChannelListItem.DisposeBuffers;
begin
  if not IsActive then
    exit;
  IsActive := false;

  if Assigned(DoubleBuffer[0]) then
    FreeMem(DoubleBuffer[0]);
  if Assigned(DoubleBuffer[1]) then
    FreeMem(DoubleBuffer[1]);
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Channel list implementation'} {$ENDIF}
{ TTDavASIOExtDrvrSampleRateManager }

{$IFDEF DELPHI10_UP} {$REGION 'Samplerate list implementation'} {$ENDIF}

constructor TTDavASIOExtDrvrSampleRateManager.Create;
begin
  FSampleRateList := TList.Create;
  FSampleRateList.Clear;
  FSampleRateMode := edsrm_All;
  FDefaultSampleRate := -1;
end;

destructor TTDavASIOExtDrvrSampleRateManager.Destroy;
var
  i: Integer;
begin
  for i := FSampleRateList.count - 1 downto 0 do
    TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[i]).Free;

  FSampleRateList.Clear;
  inherited;
end;

procedure TTDavASIOExtDrvrSampleRateManager.AddSampleRate(sr: Double);
var
  t: TDavASIOExtDrvrSampleRateItem;
begin
  t := TDavASIOExtDrvrSampleRateItem.Create;
  t.SampleRate := sr;
  FSampleRateList.Add(t);
end;

function TTDavASIOExtDrvrSampleRateManager.CanSampleRate(sr: Double): Boolean;
var
  i: Integer;
begin
  case FSampleRateMode of
    edsrm_Single:
      begin
        if FSampleRateList.count < 1 then
          Result := false
        else
          Result := TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[0])
            .SampleRate = sr;

        exit;
      end;

    edsrm_Range:
      begin
        if FSampleRateList.count < 2 then
          Result := false
        else
          Result := (TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[0])
            .SampleRate <= sr) and
            (TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[1]).SampleRate
            >= sr) or (TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[0])
            .SampleRate >= sr) and
            (TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[1])
            .SampleRate <= sr);

        exit;
      end;

    edsrm_List:
      begin
        Result := false;

        if FSampleRateList.count > 0 then
          for i := 0 to FSampleRateList.count - 1 do
            if TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[i])
              .SampleRate = sr then
            begin
              Result := true;
              break;
            end;

        exit;
      end;
  end;

  // ALL
  Result := true;
end;

procedure TTDavASIOExtDrvrSampleRateManager.SetDefaultSampleRate(sr: Double);
begin
  FDefaultSampleRate := sr;
end;

function TTDavASIOExtDrvrSampleRateManager.GetDefaultSampleRate: Double;
begin
  if FDefaultSampleRate > 0 then
    Result := FDefaultSampleRate
  else
  begin
    Result := 44100;
    if CanSampleRate(Result) or (FSampleRateList.count < 1) then
      exit;

    Result := TDavASIOExtDrvrSampleRateItem(FSampleRateList.Items[0])
      .SampleRate;
  end;
end;

procedure TTDavASIOExtDrvrSampleRateManager.SetSampleRateMode
  (md: TTDavASIOExtDrvrSampleRateMode);
begin
  FSampleRateMode := md;
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Samplerate list implementation'} {$ENDIF}
{ TDavASIOExtendedDriver }

{$IFDEF DELPHI10_UP} {$REGION 'Extended driver implemention'} {$ENDIF}

constructor TDavASIOExtendedDriver.Create(TCWrapper: TDavASIOTCWrapper;
  InterfaceGUID: TGuid);
begin
  inherited;
  FClockList := TList.Create;
  FInChannelList := TList.Create;
  FOutChannelList := TList.Create;
  FProcessingThread := nil;
  FHostCallbacks := nil;
  FDriverName := 'DAV Abstract Ext';
  FDriverVersion := 1;
  FHostEngineVersion := 1;
  FSupportsTimeInfo := false;
  FSupportsTimeCode := false;
  FBuffersCreated := false;
  FInputLatency := 0;
  FOutputLatency := 0;
  FCurrentBuffer := 0;
  FIsProcessing := false;

  with FSwitchTimings do
  begin
    SystemTime.Lo := 0;
    SystemTime.Hi := 0;
    SamplePos.Lo := 0;
    SamplePos.Hi := 0;
  end;

  with FBufferSize do
  begin
    Minimum := 64;
    Maximum := 4096;
    Prefered := 512;
    Granularity := -1;
    Current := 512;
  end;

  ClearSupportedSelectors;

  FLastErrorMsg := '';
  FSampleRateManager := TTDavASIOExtDrvrSampleRateManager.Create;

  FClocksAreDefault := false;
  FChannelsAreDefault := false;

  AddClock('Default Clock', 0);
  AddChannel('Default Input', 0, ASIOSTFloat32LSB, true);
  AddChannel('Default Output', 0, ASIOSTFloat32LSB, false);

  FClocksAreDefault := true;
  FChannelsAreDefault := true;

  InitializeDriverParams;
  LoadDriverSettings;

  FSampleRate := FSampleRateManager.GetDefaultSampleRate;

  FillChar(FAsioTime.Reserved, 4, 0);
  InitializeTimeInfo;

  InitControlPanel;
end;

destructor TDavASIOExtendedDriver.Destroy;
begin
  Stop;
  DisposeBuffers;

  FreeAndNil(FSampleRateManager);

  ClearClockList;
  FreeAndNil(FClockList);

  ClearChannelLists;
  FreeAndNil(FInChannelList);
  FreeAndNil(FOutChannelList);

  inherited;
end;

procedure TDavASIOExtendedDriver.LoadDriverSettings;
begin
  // this is default: does nothing
end;

procedure TDavASIOExtendedDriver.SaveDriverSettings;
begin
  // this is default: does nothing
end;

procedure TDavASIOExtendedDriver.ClearClockList;
var
  i: Integer;
begin
  for i := FClockList.count - 1 downto 0 do
    TDavASIOExtDrvrClockListItem(FClockList.Items[i]).Free;
  FClockList.Clear;
end;

procedure TDavASIOExtendedDriver.ClearChannelLists;
var
  i: Integer;
begin
  for i := FInChannelList.count - 1 downto 0 do
    TDavASIOExtDrvrChannelListItem(FInChannelList.Items[i]).Free;
  for i := FOutChannelList.count - 1 downto 0 do
    TDavASIOExtDrvrChannelListItem(FOutChannelList.Items[i]).Free;
  FInChannelList.Clear;
  FOutChannelList.Clear;
end;

procedure TDavASIOExtendedDriver.InitializeDriverParams;
begin
  raise Exception.Create('You have to overwrite InitializeDriverParams');
end;

procedure TDavASIOExtendedDriver.SetDriverName(name: string);
begin
  FDriverName := name;
end;

procedure TDavASIOExtendedDriver.SetDriverVersion(version: LongInt);
begin
  FDriverVersion := version;
end;

procedure TDavASIOExtendedDriver.SetErrorMessage(s: string);
begin
  FLastErrorMsg := s;
end;

procedure TDavASIOExtendedDriver.AddClock(name: string; ChannelGroup: Integer);
var
  t: TDavASIOExtDrvrClockListItem;
begin
  if FClocksAreDefault then
  begin
    ClearClockList;
    FClocksAreDefault := false;
  end;

  t := TDavASIOExtDrvrClockListItem.Create;
  t.ClockName := copy(name, 0, 32);
  t.ChannelGroup := ChannelGroup;
  t.IsCurrentSource := false;
  FClockList.Add(t);
end;

procedure TDavASIOExtendedDriver.AddChannel(name: string; ChannelGroup: Integer;
  SampleType: TASIOSampleType; IsInput: Boolean);
var
  t: TDavASIOExtDrvrChannelListItem;
begin
  if FChannelsAreDefault then
  begin
    ClearChannelLists;
    FChannelsAreDefault := false;
  end;

  t := TDavASIOExtDrvrChannelListItem.Create(name, ChannelGroup,
    SampleType, IsInput);

  if IsInput then
    FInChannelList.Add(t)
  else
    FOutChannelList.Add(t);
end;

function TDavASIOExtendedDriver.GetCurrentClockSource: Integer;
var
  i: Integer;
begin
  Result := -1;
  if FClockList.count < 1 then
    exit;

  for i := 0 to FClockList.count - 1 do
    with TDavASIOExtDrvrClockListItem(FClockList.Items[i]) do
    begin
      if IsCurrentSource then
      begin
        Result := i;
        break;
      end;
    end;
end;

procedure TDavASIOExtendedDriver.AddSampleRate(sr: Double);
begin
  FSampleRateManager.AddSampleRate(sr);
end;

procedure TDavASIOExtendedDriver.SetSampleRateMode
  (md: TTDavASIOExtDrvrSampleRateMode);
begin
  FSampleRateManager.SetSampleRateMode(md);
end;

procedure TDavASIOExtendedDriver.SetBufferSizes(MinSize, MaxSize, PreferredSize,
  Granularity: Integer);
begin
  if PreferredSize < MinSize then
    MinSize := PreferredSize;
  if PreferredSize > MaxSize then
    MaxSize := PreferredSize;
  if MinSize = MaxSize then
    Granularity := 0;

  FBufferSize.Minimum := MinSize;
  FBufferSize.Maximum := MaxSize;
  FBufferSize.Prefered := PreferredSize;
  FBufferSize.Granularity := Granularity;

  if not CheckBufferSize(FBufferSize.Current) then
    ASIOBufferSizeChange(PreferredSize);
end;

procedure TDavASIOExtendedDriver.SetLatencies(InLatency, OutLatency: LongInt);
begin
  if (FInputLatency = InLatency) and (FOutputLatency = OutLatency) then
    exit;

  FInputLatency := InLatency;
  FOutputLatency := OutLatency;
  ASIOLatenciesChanged;
end;

function TDavASIOExtendedDriver.GetFirstGroupChannel(GroupNr: LongInt;
  IsInput: Boolean): LongInt;
var
  querylist: TList;
  i: Integer;
begin
  Result := -1;
  if IsInput then
    querylist := FInChannelList
  else
    querylist := FOutChannelList;

  if querylist.count > 0 then
    for i := 0 to querylist.count - 1 do
      with TDavASIOExtDrvrChannelListItem(querylist.Items[i]) do
        if ChannelGroup = GroupNr then
        begin
          Result := i;
          break;
        end;
end;

function TDavASIOExtendedDriver.GetClockSources(Clocks: PASIOClockSources;
  out NumSources: LongInt): TASIOError;
var
  i: Integer;
begin
  Result := ASE_NotPresent;

  NumSources := min(NumSources, FClockList.count);
  if NumSources > 0 then
  begin
    for i := 0 to FClockList.count - 1 do
      with TDavASIOExtDrvrClockListItem(FClockList.Items[i]) do
      begin
        Clocks^[i].Index := i;
        Clocks^[i].AssociatedChannel :=
          GetFirstGroupChannel(ChannelGroup, true);
        Clocks^[i].AssociatedGroup := ChannelGroup;
        Clocks^[i].IsCurrentSource := TASIOBool(IsCurrentSource);
        StrCopy(Clocks^[i].name, PChar(ClockName));
      end;

    Result := ASE_OK;
  end;
end;

function TDavASIOExtendedDriver.SetClockSource(Reference: Integer): TASIOError;
var
  last: Integer;
begin
  Result := ASE_OK;

  if (Reference >= FClockList.count) or (Reference < 0) then
  begin
    Result := ASE_InvalidParameter;
    exit;
  end;

  last := GetCurrentClockSource;
  if Reference = last then
    exit;

  ChangeClockSource(last, Reference);

  if FSupportsTimeInfo then
  begin
    FAsioTime.TimeInfo.Flags := FAsioTime.TimeInfo.Flags or kClockSourceChanged;
  end;
end;

procedure TDavASIOExtendedDriver.ChangeClockSource(fromIndex, ToIndex: LongInt);
begin
  // you can override this and fill the sample rate list here, to have clock dependent samplerates
  if fromIndex >= 0 then
    TDavASIOExtDrvrClockListItem(FClockList.Items[fromIndex])
      .IsCurrentSource := false;
  TDavASIOExtDrvrClockListItem(FClockList.Items[ToIndex])
    .IsCurrentSource := true;
end;

function TDavASIOExtendedDriver.GetSamplePosition(out SamplePosition
  : TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  TimeStamp := FSwitchTimings.SystemTime;
  SamplePosition := FSwitchTimings.SamplePos;
  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetChannels(out NumInputChannels,
  NumOutputChannels: LongInt): TASIOError;
begin
  NumInputChannels := FInChannelList.count;
  NumOutputChannels := FOutChannelList.count;

  if NumInputChannels + NumOutputChannels < 1 then
    Result := ASE_NotPresent
  else
    Result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetChannelInfo(var Info: TASIOChannelInfo)
  : TASIOError;
var
  querylist: TList;
begin
  if Info.IsInput = ASIOTrue then
    querylist := FInChannelList
  else
    querylist := FOutChannelList;

  if Info.Channel >= querylist.count then
    Result := ASE_InvalidParameter
  else
    with TDavASIOExtDrvrChannelListItem(querylist.Items[Info.Channel]) do
    begin
      Info.SampleType := SampleType;
      Info.ChannelGroup := ChannelGroup;
      Info.IsActive := TASIOBool(IsActive);
      StrPCopy(Info.name, PChar(ChannelName));
      Result := ASE_OK;
    end;
end;

function TDavASIOExtendedDriver.GetDriverName: string;
begin
  Result := FDriverName;
end;

function TDavASIOExtendedDriver.GetDriverVersion: LongInt;
begin
  Result := FDriverVersion;
end;

function TDavASIOExtendedDriver.GetErrorMessage: string;
begin
  Result := FLastErrorMsg;
end;

function TDavASIOExtendedDriver.CanSampleRate(SampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if FSampleRateManager.CanSampleRate(SampleRate) then
    Result := ASE_OK
  else
    Result := ASE_NoClock;
end;

function TDavASIOExtendedDriver.GetSampleRate(out nSampleRate: TASIOSampleRate)
  : TASIOError;
begin
  nSampleRate := FSampleRate;
  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.SetSampleRate(nSampleRate: TASIOSampleRate)
  : TASIOError;
begin
  if FSampleRateManager.CanSampleRate(nSampleRate) then
  begin
    if FSampleRate <> nSampleRate then
    begin
      FSampleRate := nSampleRate;
      if FSupportsTimeInfo then
      begin
        FAsioTime.TimeInfo.SampleRate := FSampleRate;
        FAsioTime.TimeInfo.Flags := FAsioTime.TimeInfo.Flags or
          kSampleRateChanged;
      end;
      ASIOSampleRateDidChange(FSampleRate);
    end;
    Result := ASE_OK;
  end
  else
    Result := ASE_NoClock;
end;

function TDavASIOExtendedDriver.GetLatencies(out InLatency, OutLatency: LongInt)
  : TASIOError;
begin
  InLatency := FInputLatency;
  OutLatency := FOutputLatency;

  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.GetBufferSize(out MinSize, MaxSize,
  PreferredSize, Granularity: Integer): TASIOError;
begin
  MinSize := FBufferSize.Minimum;
  MaxSize := FBufferSize.Maximum;
  PreferredSize := FBufferSize.Prefered;
  Granularity := FBufferSize.Granularity;

  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.CheckBufferSize(test: Integer): Boolean;
var
  tmp: LongInt;
begin
  with FBufferSize do
  begin
    if (test = Minimum) or (test = Maximum) or (test = Prefered) then
    begin
      // in this case it should be valid
      Result := true;
      exit;
    end;

    if Granularity = 0 then
    begin
      // no chance
      Result := false;
      exit;
    end;

    Result := false;
    if (test >= Minimum) and (test <= Maximum) then
    begin
      if Granularity > 0 then
        Result := (test - Minimum) mod Granularity = 0
      else
      begin
        if Minimum > 0 then
        begin
          tmp := test div Minimum;
          Result := ((test mod Minimum) = 0) and (tmp and (tmp - 1) = 0);
        end
        else if Maximum > 0 then
        begin
          tmp := Maximum div test;
          Result := ((Maximum mod test) = 0) and (tmp and (tmp - 1) = 0);
        end;
      end;
    end;
  end;
end;

procedure TDavASIOExtendedDriver.ClearSupportedSelectors;
begin
  with FSupportedSelectors do
  begin
    RequestsDone := false;
    EngineVersion := false;
    ResetRequest := false;
    BufferSizeChange := false;
    ResyncRequest := false;
    LatenciesChanged := false;
    SupportsTimeInfo := false;
    SupportsTimeCode := false;
  end;
end;

procedure TDavASIOExtendedDriver.CheckSupportedSelectors;
begin
  with FSupportedSelectors do
  begin
    EngineVersion := ASIOMessage(kAsioSelectorSupported, kAsioEngineVersion,
      nil, nil) > 0;
    ResetRequest := ASIOMessage(kAsioSelectorSupported, kAsioResetRequest,
      nil, nil) > 0;
    BufferSizeChange := ASIOMessage(kAsioSelectorSupported,
      kAsioBufferSizeChange, nil, nil) > 0;
    ResyncRequest := ASIOMessage(kAsioSelectorSupported, kAsioResyncRequest,
      nil, nil) > 0;
    LatenciesChanged := ASIOMessage(kAsioSelectorSupported,
      kAsioLatenciesChanged, nil, nil) > 0;
    SupportsTimeInfo := ASIOMessage(kAsioSelectorSupported,
      kAsioSupportsTimeInfo, nil, nil) > 0;
    SupportsTimeCode := ASIOMessage(kAsioSelectorSupported,
      kAsioSupportsTimeCode, nil, nil) > 0;

    RequestsDone := true;
  end;
end;

procedure TDavASIOExtendedDriver.InitializeTimeInfo;
begin
  with FAsioTime.TimeInfo do
  begin
    Speed := 1;
    SystemTime.Lo := 0;
    SystemTime.Hi := 0;
    SamplePosition.Lo := 0;
    SamplePosition.Hi := 0;
    SampleRate := FSampleRate;
    Flags := kSystemTimeValid or kSamplePositionValid or kSampleRateValid;
  end;

  with FAsioTime.TimeCode do
  begin
    Speed := 1;
    TimeCodeSamples.Lo := 0;
    TimeCodeSamples.Hi := 0;
    Flags := kTcValid or kTcRunning;
  end;
end;

function TDavASIOExtendedDriver.CreateBuffers(BufferInfos: PASIOBufferInfos;
  NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks)
  : TASIOError;
var
  i: Integer;
  querylist: TList;
begin
  ClearSupportedSelectors;

  FSupportsTimeInfo := false;
  FSupportsTimeCode := false;

  DisposeBuffers; // clear the previous buffers
  Result := ASE_OK;

  if not CheckBufferSize(BufferSize) then
    Result := ASE_InvalidMode
  else
  begin
    FBufferSize.Current := BufferSize;

    for i := 0 to NumChannels - 1 do
      with BufferInfos^[i] do
      begin
        if IsInput = ASIOTrue then
          querylist := FInChannelList
        else
          querylist := FOutChannelList;

        if (ChannelNum < 0) or (ChannelNum >= querylist.count) then
        begin
          Result := ASE_InvalidParameter;
          break;
        end;

        if not TDavASIOExtDrvrChannelListItem(querylist.Items[ChannelNum])
          .CreateBuffers(Buffers, FBufferSize.Current) then
        begin
          Result := ASE_NoMemory;
          break;
        end;
      end;
  end;

  if Result <> ASE_OK then
  begin
    DisposeBuffers;
    exit;
  end;

  FBuffersCreated := true;
  FHostCallbacks := @Callbacks;
  CheckSupportedSelectors;

  if FSupportedSelectors.SupportsTimeInfo then
    FSupportsTimeInfo := ASIOMessage(kAsioSupportsTimeInfo, 0, nil, nil) > 0;
  if FSupportedSelectors.SupportsTimeCode then
    FSupportsTimeCode := ASIOMessage(kAsioSupportsTimeCode, 0, nil, nil) > 0;
  if FSupportedSelectors.EngineVersion then
    FHostEngineVersion := ASIOMessage(kAsioEngineVersion, 0, nil, nil);

  if FSupportsTimeInfo then
    InitializeTimeInfo;
end;

function TDavASIOExtendedDriver.DisposeBuffers: TASIOError;
var
  i: Integer;
begin
  for i := 0 to FInChannelList.count - 1 do
    TDavASIOExtDrvrChannelListItem(FInChannelList.Items[i]).DisposeBuffers;
  for i := 0 to FOutChannelList.count - 1 do
    TDavASIOExtDrvrChannelListItem(FOutChannelList.Items[i]).DisposeBuffers;

  FBuffersCreated := false;
  Result := ASE_OK;
end;

function TDavASIOExtendedDriver.Start: TASIOError;
begin
  if Assigned(FHostCallbacks) then
  begin
    InitializeTimeInfo;
    FCurrentBuffer := 0;
    StartProcessingThread;
    FIsProcessing := true;
    Result := ASE_OK;
  end
  else
    Result := ASE_NotPresent;
end;

function TDavASIOExtendedDriver.Stop: TASIOError;
begin
  FIsProcessing := false;
  StopProcessingThread;
  Result := ASE_OK;
end;

procedure TDavASIOExtendedDriver.StartProcessingThread;
begin
  if Assigned(FProcessingThread) then
    StopProcessingThread;

  FProcessingThread := TDavASIOExtDrvrProcessingThread.Create(true);
  FProcessingThread.Driver := self;
  FProcessingThread.Resume;
end;

procedure TDavASIOExtendedDriver.StopProcessingThread;
begin
  if Assigned(FProcessingThread) then
  begin
    with FProcessingThread do
    begin
      if Suspended then
        Resume;

      Terminate;
      WaitFor;
    end;
    FreeAndNil(FProcessingThread);
  end;
end;

procedure TDavASIOExtendedDriver.ProcessBuffers;
begin
  // this is a dummy, override it
  ASIOBufferSwitch(FCurrentBuffer, ASIOTrue);
  Sleep(Round(1000 * FBufferSize.Current / FSampleRate));
  FCurrentBuffer := 1 - FCurrentBuffer;
end;

procedure TDavASIOExtendedDriver.GetNanoSeconds(var Time: TASIOTimeStamp);
var
  NanoSeconds: Double;
const
  CTwoRaisedTo32: Double = 4294967296;
begin
  // it looks stupid, but this has to be in to lines, otherwise it would be an integer multiplication
  // this fucking bullshit took me 10 hours to find it :)
  NanoSeconds := timegettime;
  NanoSeconds := NanoSeconds * 1000000;
  Time.Hi := floor(NanoSeconds / CTwoRaisedTo32);
  Time.Lo := floor(NanoSeconds - Time.Hi * CTwoRaisedTo32);
end;

function TDavASIOExtendedDriver.GetTimecodeSamples: TASIOSamples;
begin
  // override this if you need a custom timecode
  Result := FSwitchTimings.SamplePos;
end;

procedure TDavASIOExtendedDriver.UpdateTimings;
var
  tmp: int64;
begin
  with FSwitchTimings.SamplePos do
  begin
    tmp := int64(Lo) + FBufferSize.Current;

    Lo := tmp and $FFFFFFFF;
    Hi := Hi + (tmp shr 32);
  end;

  GetNanoSeconds(FSwitchTimings.SystemTime);

  if FSupportsTimeInfo then
  begin
    FAsioTime.TimeInfo.SamplePosition := FSwitchTimings.SamplePos;
    FAsioTime.TimeInfo.SystemTime := FSwitchTimings.SystemTime;

    if FSupportsTimeCode then
      FAsioTime.TimeCode.TimeCodeSamples := GetTimecodeSamples;
  end;
end;

procedure TDavASIOExtendedDriver.ASIOBufferSwitch(DoubleBufferIndex: Integer;
  DirectProcess: TASIOBool);
begin
  if FSupportsTimeInfo then
  begin
    ASIOBufferSwitchTimeInfo(DoubleBufferIndex, DirectProcess);
    exit;
  end;

  UpdateTimings;

  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.bufferSwitch) then
    FHostCallbacks^.bufferSwitch(DoubleBufferIndex, DirectProcess);
end;

procedure TDavASIOExtendedDriver.ASIOBufferSwitchTimeInfo(DoubleBufferIndex
  : Integer; DirectProcess: TASIOBool);
begin
  if not FSupportsTimeInfo then
  begin
    ASIOBufferSwitch(DoubleBufferIndex, DirectProcess);
    exit;
  end;

  UpdateTimings;

  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.bufferSwitchTimeInfo)
  then
    FHostCallbacks^.bufferSwitchTimeInfo(FAsioTime, DoubleBufferIndex,
      DirectProcess);

  FAsioTime.TimeInfo.Flags := FAsioTime.TimeInfo.Flags and
    not(kSampleRateChanged or kClockSourceChanged);
end;

procedure TDavASIOExtendedDriver.ASIOSampleRateDidChange
  (SampleRate: TASIOSampleRate);
begin
  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.sampleRateDidChange)
  then
    FHostCallbacks^.sampleRateDidChange(SampleRate);
end;

function TDavASIOExtendedDriver.ASIOMessage(Selector, Value: Integer;
  msg: Pointer; Opt: PDouble): Integer;
begin
  if Selector = kAsioResetRequest then
    SaveDriverSettings;

  if Assigned(FHostCallbacks) and Assigned(FHostCallbacks^.ASIOMessage) then
    Result := FHostCallbacks^.ASIOMessage(Selector, Value, msg, Opt)
  else
    Result := 0;
end;

procedure TDavASIOExtendedDriver.ASIORequestReset;
begin
  if FSupportedSelectors.ResetRequest then
    ASIOMessage(kAsioResetRequest, 0, nil, nil);
end;

procedure TDavASIOExtendedDriver.ASIOResyncRequest;
begin
  if FSupportedSelectors.ResyncRequest then
    ASIOMessage(kAsioResyncRequest, 0, nil, nil);
end;

procedure TDavASIOExtendedDriver.ASIOBufferSizeChange(newsize: LongInt);
var
  tmp: LongInt;
begin
  tmp := 0;
  if FSupportedSelectors.BufferSizeChange then
    tmp := ASIOMessage(kAsioBufferSizeChange, newsize, nil, nil);
  if tmp = 0 then
    ASIORequestReset;
end;

procedure TDavASIOExtendedDriver.ASIOLatenciesChanged;
begin
  if FSupportedSelectors.LatenciesChanged then
    ASIOMessage(kAsioLatenciesChanged, 0, nil, nil);
end;

{$IFDEF DELPHI10_UP} {$ENDREGION 'Extended driver implemention'} {$ENDIF}

end.
