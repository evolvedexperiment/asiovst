unit ASIOVIObject;

////////////////////////////////////////////////////////////////////////////////
//                                                                            //
//  Version: MPL 1.1 or LGPL 2.1 with linking exception                       //
//                                                                            //
//  The contents of this file are subject to the Mozilla Public License       //
//  Version 1.1 (the "License"); you may not use this file except in          //
//  compliance with the License. You may obtain a copy of the License at      //
//  http://www.mozilla.org/MPL/                                               //
//                                                                            //
//  Software distributed under the License is distributed on an "AS IS"       //
//  basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the   //
//  License for the specific language governing rights and limitations under  //
//  the License.                                                              //
//                                                                            //
//  Alternatively, the contents of this file may be used under the terms of   //
//  the Free Pascal modified version of the GNU Lesser General Public         //
//  License Version 2.1 (the "FPC modified LGPL License"), in which case the  //
//  provisions of this license are applicable instead of those above.         //
//  Please see the file LICENSE.txt for additional information concerning     //
//  this license.                                                             //
//                                                                            //
//  The code is part of the Delphi ASIO & VST Project                         //
//                                                                            //
//  The initial developer of this code is Christian-W. Budde                  //
//                                                                            //
//  Portions created by Christian-W. Budde are Copyright (C) 2008-2015        //
//  by Christian-W. Budde. All Rights Reserved.                               //
//                                                                            //
////////////////////////////////////////////////////////////////////////////////

interface

{$I DAV_Compiler.Inc}

{$IFNDEF FPC} {$INLINE AUTO} {$ENDIF}

uses
  Windows, {$IFDEF FPC} LCLIntf, {$ENDIF} Classes,
  DAV_ASIO, DAV_AsioInterface, DAV_AsioConvert;

const
{$EXTERNALSYM WM_USER}
  WM_USER = $0400;

  PM_ASIO = WM_USER + 1652; // unique we hope
  // ASIO message(s), as wParam for PM_ASIO
  AM_ResetRequest = 0;
  AM_BufferSwitch = 1; // new buffer index in lParam
  AM_BufferSwitchTimeInfo = 2; // new buffer index in lParam

  // time passed in MainForm.BufferTime
  AM_LatencyChanged = 3;
  PM_UpdateSamplePos = PM_ASIO + 1; // sample pos in wParam (hi) and lParam (lo)

  PM_BufferSwitch = PM_ASIO + 2;
  PM_BufferSwitchTimeInfo = PM_ASIO + 3;

  HKEY_CLASSES_ROOT = LongWord($80000000);
  HKEY_LOCAL_MACHINE = LongWord($80000002);

type
  TMessage = packed record
    Msg: Cardinal;
    case Integer of
      0:
        (WParam: Integer;
         LParam: Integer;
         Result: Integer);
      1:
        (WParamLo: Word;
         WParamHi: Word;
         LParamLo: Word;
         LParamHi: Word;
         ResultLo: Word;
         ResultHi: Word);
  end;
  PMessage = ^TMessage;

  TSingleArray = array of Single;
  PSingleArray = ^TSingleArray;
  TDoubleArray = array of Double;
  PDoubleArray = ^TDoubleArray;
  TArrayOfSingleArray = array of TSingleArray;
  PArrayOfSingleArray = ^TArrayOfSingleArray;

  TLVSingleArray = array [0 .. 0] of Single;
  PLVSingleArray = ^TLVSingleArray;
  TLVDoubleArray = array [0 .. 0] of Double;
  PLVDoubleArray = ^TLVDoubleArray;

  TLVArray = record
    Pointer: PDoubleArray;
    Data: TDoubleArray;
  end;
  PLVArray = ^TLVArray;

  TComplex = record
    Im: Double;
    Re: Double;
  end;

  TAsioDriverDesc = packed record
    id: TGUID;
    name: array [0 .. 511] of Char;
    path: array [0 .. 511] of Char;
  end;
  PAsioDriverDesc = ^TAsioDriverDesc;

  TAsioDriverList = array of TAsioDriverDesc;

  TConvertOptimization = (coSSE, co3DNow);
  TConvertOptimizations = set of TConvertOptimization;

  TClipPreventer = procedure(InBuffer: PSingle; Samples: Integer);
  TSamplePositionUpdateEvent = procedure(Sender: TObject; SamplePosition: Int64) of object;
  TSample2Event = procedure(Sender: TObject; Sample: array of Single) of object;

  TBufferPreFill = (bpfNone, bpfZero, bpfNoise, bpfSine);
  TPreventClipping = (pcNone, pcDigital, pcAnalog);
  TInputMonitor = (imDisabled, imMono, imStereo, imAll);

  TATFlag = (atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
    atSpeedValid, atSampleRateChanged, atClockSourceChanged);
  TATFlags = set of TATFlag;
  TNotifyEvent = procedure(Sender: TObject) of object;

  TASIOTimeSub = class(TObject)
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
  public
    property OnChanged: TNotifyEvent read FOnChange write FOnChange;
    constructor Create;

    property SamplePos: Int64 index 0 read GetATInt64 write SetATInt64;
    property Speed: Double index 0 read GetATdouble write SetATdouble;
    // absolute speed (1. = nominal)
    property SampleRate: Double Index 1 read GetATdouble write SetATdouble;
    property Flags: TATFlags read GetATflags Write SetATflags;
  end;

type
  TLabviewASIO = class(TObject)
  private
    FActive: Boolean;
    FPreventClipping: TPreventClipping;
    FInBufferPreFill: TBufferPreFill;
    FOutBufferPreFill: TBufferPreFill;

    FDriver: IStdCallAsio;
    FDriverIndex: Integer;
    FDriverList: TStrings;
    FDriverName: String;
    FDriverVersion: Integer;

    FInputLatency: Integer;
    FOutputLatency: Integer;
    FInputChannels: Integer;
    FOutputChannels: Integer;
    FSampleRate: Double;
    FBufferSize: Cardinal;
    FASIOTime: TASIOTimeSub;

    FInputChannelOffset: Word;
    FOutputChannelOffset: Word;
    FMin, FMax, FPref, FGranularity: Integer;

    FInConverters: array of TInConverter;
    FOutConverters: array of TOutConverter;

    FAsioDriverList: TAsioDriverList;
    FBuffersCreated: Boolean;
    FCallbacks: TASIOCallbacks;

    FSingleInBuffer: TArrayOfSingleArray;
    FSingleOutBuffer: TArrayOfSingleArray;

    FUnAlignedBuffer: PASIOBufferInfo;
    FInputBuffer: PASIOBufferInfo;
    FOutputBuffer: PASIOBufferInfo;

    FInputChannelInfos: array of TASIOChannelInfo;
    FOutputChannelInfos: array of TASIOChannelInfo;
    FOutBuf, FInBuf: TArrayOfSingleArray;
    FLastSamples: TSingleArray;
    FLoopCounts, FBufferUnderruns, FReadPosition, FWritePosition: Integer;

    FInputMonitor: TInputMonitor;
    FConvertOptimizations: TConvertOptimizations;
    FOutputVolume: TSingleArray;
    FClipPrevent: TClipPreventer;
    FASIODone: Boolean;
    FInMeter: array of Single;
    FOutMeter: array of Single;
    FSineFrequencies: array of Single;
    FSineStarts: array of TComplex;
    FSineStates: array of TComplex;
    FXBSize: Integer;
    FXBSizeH, FXBSizeV: Integer;
    FCalcMeters: Boolean;
    FWatchDog: Boolean;

    FOnReset: TNotifyEvent;
    FOnDriverChanged: TNotifyEvent;
    FOnLatencyChanged: TNotifyEvent;
    FOnSampleRateChanged: TNotifyEvent;

    FOnSample2Output: TSample2Event;
    FOnInput2Sample: TSample2Event;

    FOnUpdateSamplePos: TSamplePositionUpdateEvent;
    procedure SetActive(Value: Boolean);
    procedure SetDriverIndex(Value: Integer);
    function CreateBuffers: Boolean;
    procedure DestroyBuffers;
    procedure BufferSwitch(Index: Integer);
    procedure BufferSwitchTimeInfo(Index: Integer; const params: TASIOTime);
    procedure SetSampleRate(const Value: Double);
    procedure SetDriverName(const s: String);
    procedure SetInputChannelOffset(const w: Word);
    procedure SetOutputChannelOffset(const w: Word);
    procedure SetConvertOptimizations(const co: TConvertOptimizations);
    procedure SetPreventClipping(v: TPreventClipping);
    function GetBufferSize: Cardinal;
    function GetSampleRate: Double;
    function GetSineFrequency(Index: Integer): Single;
    function GetInConverter(ConverterType: TAsioSampleType): TInConverter;
    function GetOutConverter(ConverterType: TAsioSampleType): TOutConverter;
    procedure SetSineFrequency(Index: Integer; const Value: Single);
    procedure SetXBSize(const Value: Integer);
    procedure ResetPositions;
  protected
    procedure PMASIO(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage);
      message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage);
      message PM_BufferSwitchTimeInfo;
    function GetDriverList: TStrings;
  public
    constructor Create;
    destructor Destroy; override;

    procedure Reset;
    function GetNumDrivers: Integer;
    procedure OpenDriver;
    procedure CloseDriver;
    function ControlPanel: Integer;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;

    property Active: Boolean read FActive write SetActive;
    property ASIODone: Boolean read FASIODone;
    property ASIOTime: TASIOTimeSub read FASIOTime Write FASIOTime;
    property BufferGranularity: Integer read FGranularity;
    property BufferMaximum: Integer read FMax;
    property BufferMinimum: Integer read FMin;
    property BufferPreferredSize: Integer read FPref;
    property BufferSize: Cardinal read GetBufferSize default 1;
    property ConvertOptimizations: TConvertOptimizations read FConvertOptimizations write SetConvertOptimizations;
    property DriverIndex: Integer read FDriverIndex write SetDriverIndex default -1;
    property DriverList: TStrings read FDriverList;
    property DriverName: string read FDriverName write SetDriverName;
    property DriverVersion: Integer read FDriverVersion;
    property ExtraBufferSize: Integer read FXBSize write SetXBSize;
    property InputChannelOffset: Word read FInputChannelOffset write SetInputChannelOffset default 0;
    property InputChannels: Integer read FInputChannels default 0;
    property InputLatency: Integer read FInputLatency default 0;
    property InputMonitor: TInputMonitor read FInputMonitor write FInputMonitor default imDisabled;
    property OnDriverChanged: TNotifyEvent read FOnDriverChanged write FOnDriverChanged;
    property OnInput2Sample: TSample2Event read FOnInput2Sample write FOnInput2Sample;
    property OnLatencyChanged: TNotifyEvent read FOnLatencyChanged write FOnLatencyChanged;
    property OnReset: TNotifyEvent read FOnReset write FOnReset;
    property OnSample2Output: TSample2Event read FOnSample2Output write FOnSample2Output;
    property OnSampleRateChanged: TNotifyEvent read FOnSampleRateChanged write FOnSampleRateChanged;
    property OnUpdateSamplePos: TSamplePositionUpdateEvent read FOnUpdateSamplePos write FOnUpdateSamplePos;
    property OutputChannelOffset: Word read FOutputChannelOffset write SetOutputChannelOffset default 0;
    property OutputChannels: Integer read FOutputChannels default 0;
    property OutputLatency: Integer read FOutputLatency default 0;
    property PreFillInBuffer: TBufferPreFill read FInBufferPreFill write FInBufferPreFill;
    property PreFillOutBuffer: TBufferPreFill read FOutBufferPreFill write FOutBufferPreFill;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property SineFrequency[index: Integer]: Single read GetSineFrequency write SetSineFrequency;
  end;

function ASIOInitDriver(DriverName: PAnsiChar): Integer; cdecl;
function ASIOInitDriverIndex(Index: Integer): Integer; cdecl;
function ASIOGetNumDevices: Integer; cdecl;
function ASIOSetDriverIndex(Index: Integer): Integer; cdecl;
function ASIOGetDriverName(Index: Integer): PAnsiChar; cdecl;
function ASIOGetDriverNames(Names: PAnsiChar;
  lmaxDriverCount, lDriverNumber: Integer): Integer; cdecl;
function ASIODriverStart: Integer; cdecl;
function ASIODriverStop: Integer; cdecl;
function ASIOGetBufferSize(minSize, maxSize, preferredSize,
  granularity: PInteger): Integer; cdecl;
function ASIOControlPanel: Integer; cdecl;
function ASIOCanSampleRate(SampleRate: Double): Integer; cdecl;
function ASIOSetSampleRate(SampleRate: Double): Integer; cdecl;
function ASIOGetChannels(InputChannels, OutputChannels: PInteger)
  : Integer; cdecl;
function ASIOOutputType(Index: Integer): Integer; cdecl;
function ASIOSetOutputVolume(Channel: Integer; Volume: Single): Integer; cdecl;
function ASIOSetOutputVolumedB(Channel: Integer; Volume: Single)
  : Integer; cdecl;
function ASIOSetSineFrequency(Channel: Integer; Frequency: Single)
  : Integer; cdecl;
function ASIOGetInputLevel(Channel: Integer): Single; cdecl;
function ASIOGetOutputLevel(Channel: Integer): Single; cdecl;
function ASIOReadWriteSize: Integer; cdecl;
function ASIOReadWriteSizeFixed: Integer; cdecl;
function ASIOReadWrite(Buffer: PDouble; Length, Channel: Integer)
  : Integer; cdecl;
function ASIOReadWriteX(Buffer: Pointer; Length: Integer): Integer; cdecl;
function ASIOAutopilot(Buffer: Pointer; Length: Integer): Integer; cdecl;
function ASIOSetExtraBufferSize(Size: Integer): Integer; cdecl;
function ASIOBufferUnderrun: Integer; cdecl;
procedure ASIOResetBufferUnderruns; cdecl;
function ASIOGetLoopCounts: Integer; cdecl;
procedure ASIOSetLoopCounts(Loops: Integer); cdecl;
function ASIOSetClipFunction(ClipFunction: Integer): Integer; cdecl;
procedure ASIOCalcMeters(CalcMeters: Integer); cdecl;

implementation

uses
  AnsiStrings, Registry, SysUtils;

const
  ASIODRV_DESC = 'description';
  INPROC_SERVER = 'InprocServer32';
  ASIO_PATH = 'software\asio';
  COM_CLSID = 'clsid';
  CLogFile = 'C:\ASIOLabVIEW.log';

resourcestring
  RStrConverterTypeUnknown = 'Converter type unknown';

var
  GHost: TLabviewASIO;
  PMUpdSamplePos: TMessage;
  PMBufSwitch: TMessage;
  PMBufSwitchTimeInfo: TMessage;
  PMReset: TMessage;
  ASIODriverInfo: TASIODriverInfo;
  RandSeed: Cardinal = $DEADBEAF;

{$IFDEF Debug}
  GLog: TStringList;

procedure LogMessage(Text: string);
begin
  GLog.Add(Text);
  GLog.SaveToFile(CLogFile);
end;
{$ENDIF}

function ASIOInitDriver(DriverName: PAnsiChar): Integer; cdecl;
var
  i: Integer;
  s: string;
begin
  s := string(DriverName);

  if s = GHost.DriverName then
  begin
    Result := 0;
    exit;
  end
  else
    Result := 1;

  for i := 0 to GHost.DriverList.Count - 1 do
  begin
    if GHost.DriverList[i] = s then
    begin
      GHost.DriverIndex := i;
      Result := 0;

      {$IFDEF Debug}
      LogMessage(Format('InitDriver (%s)', [s]));
      {$ENDIF}

      Break;
    end;
  end;
end;

function ASIOInitDriverIndex(Index: Integer): Integer; cdecl;
begin
  {$IFDEF Debug}
  LogMessage(Format('InitDriver (%d)', [Index]));
  {$ENDIF}

  try
    GHost.DriverIndex := Index;
    Result := 0;
  except
    Result := 1;
  end;
end;

function ASIOGetNumDevices: Integer; cdecl;
begin
  {$IFDEF Debug}
  LogMessage('GetNumDevices');
  {$ENDIF}

  if Assigned(GHost) then
    Result := GHost.DriverList.Count
  else
    Result := 0;
end;

function ASIOBufferUnderrun: Integer; cdecl;
begin
  {$IFDEF Debug}
  LogMessage('BufferUnderrun');
  {$ENDIF}

  Result := GHost.FBufferUnderruns;
end;

procedure ASIOResetBufferUnderruns; cdecl;
begin
  {$IFDEF Debug}
  LogMessage('BResetufferUnderrun');
  {$ENDIF}

  GHost.FBufferUnderruns := 0;
end;

function ASIOGetLoopCounts: Integer; cdecl;
begin
  {$IFDEF Debug}
  LogMessage('GetLoopCounts');
  {$ENDIF}

  Result := GHost.FLoopCounts;
end;

procedure ASIOSetLoopCounts(Loops: Integer); cdecl;
begin
  GHost.FLoopCounts := Loops;
{$IFDEF Debug}
  LogMessage('Set Loopcounts to: ' + IntToStr(Loops));
{$ENDIF}
end;

function ASIOSetOutputVolume(Channel: Integer; Volume: Single): Integer; cdecl;
var
  i: Integer;
begin
  Result := 0;
  if Assigned(GHost) then
    if (Channel > 0) and (Channel <= GHost.FOutputChannels) then
      GHost.FOutputVolume[Channel - 1] := Volume
    else
      for i := 0 to GHost.FOutputChannels - 1 do
        GHost.FOutputVolume[i] := Volume
  else
    Result := 1;
{$IFDEF Debug}
  LogMessage(Format('Set Output Volume on Channel %d to: %g', [Channel, Volume]));
{$ENDIF}
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(g: Single): Single;
begin
  if (g > -90.0) then
    Result := Exp(g * 0.05 * Ln(10))
  else
    Result := 0;
end;

function ASIOSetOutputVolumedB(Channel: Integer; Volume: Single)
  : Integer; cdecl;
var
  i: Integer;
begin
  Volume := dB_to_Amp(Volume);
  Result := 0;
  if Assigned(GHost) then
    if (Channel > 0) and (Channel <= GHost.FOutputChannels) then
      GHost.FOutputVolume[Channel - 1] := Volume
    else
      for i := 0 to GHost.FOutputChannels - 1 do
        GHost.FOutputVolume[i] := Volume
  else
    Result := 1;
{$IFDEF Debug}
  LogMessage(Format('Set Output Volume on Channel %d to: %g dB', [Channel,
    Volume]));
{$ENDIF}
end;

function ASIOSetSineFrequency(Channel: Integer; Frequency: Single)
  : Integer; cdecl;
begin
{$IFDEF Debug}
  LogMessage('SetSineFrequency');
{$ENDIF}

  Result := 0;
  if Assigned(GHost) then
    GHost.SineFrequency[Channel] := Frequency
  else
    Result := 1;
end;

function ASIOGetInputLevel(Channel: Integer): Single; cdecl;
begin
{$IFDEF Debug}
  LogMessage('GetInputLevel');
{$ENDIF}

  if (Channel > 0) and (Channel <= GHost.FInputChannels) then
    Result := GHost.FInMeter[Channel - 1]
  else
    Result := 0;
end;

function ASIOGetOutputLevel(Channel: Integer): Single; cdecl;
begin
{$IFDEF Debug}
  LogMessage('GetOutputLevel');
{$ENDIF}

  if (Channel > 0) and (Channel <= GHost.FOutputChannels) then
    Result := GHost.FOutMeter[Channel - 1]
  else
    Result := 0;
end;

procedure ASIOCalcMeters(CalcMeters: Integer); cdecl;
begin
{$IFDEF Debug}
  LogMessage('CalcMeters');
{$ENDIF}

  try
    GHost.FCalcMeters := CalcMeters > 0;
  except
  end;
end;

function ASIOReadWriteSize: Integer; cdecl;
begin
{$IFDEF Debug}
  LogMessage('ReadWriteSize');
{$ENDIF}

  with GHost do
  begin
    if FWritePosition < FReadPosition then
      Result := FXBSize - FReadPosition + FWritePosition
    else
      Result := FWritePosition - FReadPosition;
  end;
end;

function ASIOReadWriteSizeFixed: Integer; cdecl;
begin
  with GHost do
  begin
    if FWritePosition < FReadPosition then
      Result := FXBSize - FReadPosition + FWritePosition
    else
      Result := FWritePosition - FReadPosition;
    if Result > FXBSizeV then
      Result := FXBSizeV
    else
      Result := 0;
  end;
end;

function ASIOSetExtraBufferSize(Size: Integer): Integer; cdecl;
begin
{$IFDEF Debug}
  LogMessage(Format('Extra Buffer set to: %d samples', [Size]));
{$ENDIF}
  GHost.ExtraBufferSize := Size;
  Result := GHost.ExtraBufferSize;
end;

function ASIOReadWrite(Buffer: PDouble; Length, Channel: Integer)
  : Integer; cdecl;
var
  tmp: TDoubleArray absolute Buffer;
  n: Integer;
begin
  Result := 0;
  with GHost do
    if (Channel > 0) then
      if (Channel <= OutputChannels) and (Channel <= InputChannels) then
        try
          while Result < Length do
          begin
            GHost.FOutBuf[Channel - 1, FReadPosition] := tmp[Result];
            tmp[Result] := GHost.FInBuf[Channel - 1, FReadPosition];
            Inc(FReadPosition);
            if FReadPosition >= FXBSize then
              FReadPosition := 0;
            Inc(Result);
          end;
        except
          Result := 0;
        end
      else
        try
          while Result < Length do
          begin
            for n := 0 to GHost.FOutputChannels - 1 do
              GHost.FOutBuf[n, FReadPosition] := tmp[Result];
            for n := 0 to GHost.FInputChannels - 1 do
              tmp[Result] := GHost.FInBuf[n, FReadPosition];
            Inc(FReadPosition);
            if FReadPosition >= FXBSize then
              FReadPosition := 0;
            Inc(Result);
          end;
        except
          Result := 0;
        end;
end;

function ASIOReadWriteX(Buffer: Pointer; Length: Integer): Integer; cdecl;
var
  i, j: Integer;
  Channels: Integer;
begin
  try
    Result := Length;
    Buffer := Pointer(Buffer^);

    i := PInteger(Buffer)^;
    if i = Length then
      Channels := 1
    else
    begin
      Channels := i;
      Inc(PInteger(Buffer), 1);  // was Inc(Integer(Buffer), 4);
      i := PInteger(Buffer)^;
      if i <> Length then
        raise Exception.Create('ASIO: Length doesn''t fit');
      Inc(PInteger(Buffer), 1);  // was Inc(Integer(Buffer), 4);
    end;

    with GHost do
    begin
      if Channels > OutputChannels then
        Channels := OutputChannels;
      if FReadPosition + Length > FXBSize then
      begin
        for j := 0 to Channels - 1 do
          for i := 0 to FXBSize - FReadPosition - 1 do
          begin
            FOutBuf[j, FReadPosition + i] := PLVDoubleArray(Buffer)^[j * Length + i];
            PLVDoubleArray(Buffer)^[j * Length + i] := FInBuf[j, FReadPosition + i];
          end;
        FReadPosition := Length - (FXBSize - FReadPosition);
        for j := 0 to Channels - 1 do
          for i := 0 to FReadPosition - 1 do
          begin
            FOutBuf[j, i] := PLVDoubleArray(Buffer)^[j * Length + i];
            PLVDoubleArray(Buffer)^[j * Length + i] := FInBuf[j, i];
          end;
      end
      else
      begin
        for j := 0 to Channels - 1 do
          for i := 0 to Length - 1 do
          begin
            FOutBuf[j, FReadPosition + i] := PLVDoubleArray(Buffer)^[j * Length + i];
            PLVDoubleArray(Buffer)^[j * Length + i] := FInBuf[j, FReadPosition + i];
          end;
        FReadPosition := FReadPosition + Length;
      end;
    end;
  except
    Result := 0;
  end;
end;

function ASIOAutopilot(Buffer: Pointer; Length: Integer): Integer; cdecl;
var
  i, j: Integer;
  Channels: Integer;
  oldXBufSz: Integer;
begin
  try
    Buffer := Pointer(Buffer^);

    i := PInteger(Buffer)^;
    if i = Length then
      Channels := 1
    else
    begin
      Channels := i;
      Inc(PInteger(Buffer), 1);  // was Inc(Integer(Buffer), 4);
      i := PInteger(Buffer)^;
      if i <> Length then
        raise Exception.Create('ASIO: Length doesn''t fit');
      Inc(PInteger(Buffer), 1);  // was Inc(Integer(Buffer), 4);
    end;

    with GHost do
    begin
{$IFDEF Debug}
      LogMessage('Autopilot started');
{$ENDIF}
      PreventClipping := pcDigital;
      Active := False;
      oldXBufSz := ExtraBufferSize;
      ExtraBufferSize := Length + FInputLatency + FOutputLatency + BufferSize;
      FReadPosition := 0;
      FWritePosition := 0;
      Result := ExtraBufferSize;
      if Channels > OutputChannels then
        Channels := OutputChannels;

      for j := 0 to Channels - 1 do
        try
          FillChar(FOutBuf[j, 0], System.Length(FOutBuf[j]) * SizeOf(Single), 0);
          for i := 0 to Length - 1 do
            FOutBuf[j, i] := TLVDoubleArray(PLVDoubleArray(Buffer)^)[j * Length + i];
        except
        end;

{$IFDEF Debug}
      LogMessage('Autopilot before playing/recording');
{$ENDIF}
      FWatchDog := False;
      FLoopCounts := 1;
      Active := True;

      if Active then
      begin
        Sleep(Round(2008 / SampleRate * (FInputLatency + FOutputLatency +
          BufferSize)));
        if FWatchDog then
        begin
          Sleep(Round(1008 / SampleRate * (Length)));
          if FWatchDog then
            while (FBufferUnderruns < FLoopCounts) do
            begin
              FWatchDog := False;
              Sleep(Round(2008 / SampleRate * BufferSize));
              if FWatchDog = False then
                break;
            end;
        end;
      end;
      Active := False;
{$IFDEF Debug}
      LogMessage('Autopilot before copying');
{$ENDIF}
      for j := 0 to Channels - 1 do
        try
          for i := 0 to Length - 1 do
            PLVDoubleArray(Buffer)^[j * Length + i] :=
              FInBuf[j, i + FInputLatency + FOutputLatency];
        except
        end;
      ExtraBufferSize := oldXBufSz;
{$IFDEF Debug}
      LogMessage('Autopilot finished');
{$ENDIF}
    end;
  except
    Result := 0;
{$IFDEF Debug}
    LogMessage('Autopilot error');
{$ENDIF}
  end;
end;

function ASIOSetClipFunction(ClipFunction: Integer): Integer; cdecl;
begin
  Result := 0;
  try
    if (ClipFunction < 0) or (ClipFunction > 2) then
      GHost.PreventClipping := TPreventClipping(ClipFunction)
    else
      Result := 1;
  except
    Result := 1;
  end;
end;

function ASIOSetDriverIndex(Index: Integer): Integer; cdecl;
begin
  GHost.DriverIndex := Index;
  Result := Integer(GHost.DriverIndex = Index);
{$IFDEF Debug}
  LogMessage('Driver set to: ' + GHost.FDriverList[Index]);
{$ENDIF}
end;

function ASIOGetDriverName(Index: Integer): PAnsiChar; cdecl;
var
  dn: string;
begin
  GetMem(Result, 254);
  if (Index < GHost.DriverList.Count) and (Index >= 0) then
    dn := GHost.DriverList[Index]
  else
    dn := '';
  StrCopy(Result, PAnsiChar(AnsiString(dn)))
end;

function ASIOGetDriverNames(Names: PAnsiChar;
  lmaxDriverCount, lDriverNumber: Integer): Integer; cdecl;
var
  Current: string;
begin
  try
    if (lDriverNumber < GHost.DriverList.Count) and (lDriverNumber >= 0) then
    begin
      Current := GHost.DriverList[lDriverNumber];

{$IFDEF Debug}
      LogMessage('Available Driver (' + IntToStr(lDriverNumber) + '): ' +
        Current);
{$ENDIF}
      StrCopy(Names, PAnsiChar(AnsiString(Current)));
    end;
    Result := 0;
  except
    Result := 1;
  end;
end;

function ASIOGetBufferSize(minSize, maxSize, preferredSize,
  granularity: PInteger): Integer; cdecl;
begin
  if Assigned(GHost) then
  begin
    minSize^ := GHost.FMin;
    maxSize^ := GHost.FMax;
    preferredSize^ := GHost.FPref;
    granularity^ := GHost.FGranularity;
    Result := 0;
  end
  else
    Result := 1;
end;

function ASIOGetChannels(InputChannels, OutputChannels: PInteger)
  : Integer; cdecl;
begin
  if Assigned(GHost) then
  begin
    InputChannels^ := GHost.InputChannels;
    OutputChannels^ := GHost.OutputChannels;
    Result := 0;
  end
  else
    Result := 1;
end;

function ASIOOutputType(Index: Integer): Integer; cdecl;
begin
  case Index of
    0:
      GHost.PreFillOutBuffer := bpfNone;
    1:
      GHost.PreFillOutBuffer := bpfNoise;
    2:
      GHost.PreFillOutBuffer := bpfSine;
  else
    GHost.PreFillOutBuffer := bpfZero;
  end;
  Result := 0;
end;

function ASIODriverStart: Integer; cdecl;
begin
{$IFDEF Debug}
  LogMessage('Start Audio (before)');
{$ENDIF}
  if Assigned(GHost) then
    GHost.Active := True;
  Result := Integer(not GHost.Active);
{$IFDEF Debug}
  LogMessage('Start Audio (after)');
{$ENDIF}
end;

function ASIODriverStop: Integer; cdecl;
begin
{$IFDEF Debug}
  LogMessage('Stop Audio (before)');
{$ENDIF}
  if Assigned(GHost) then
    GHost.Active := False;
  Result := Integer(GHost.Active);
{$IFDEF Debug}
  LogMessage('Stop Audio (after)');
{$ENDIF}
end;

function ASIOCanSampleRate(SampleRate: Double): Integer; cdecl;
begin
  Result := Integer(GHost.CanSampleRate(SampleRate) <> 0);
end;

function ASIOSetSampleRate(SampleRate: Double): Integer; cdecl;
begin
  GHost.SampleRate := SampleRate;
  Result := Integer(GHost.SampleRate = SampleRate);
{$IFDEF Debug}
  LogMessage(Format('Extra Buffer set to: %f Hz', [SampleRate]));
{$ENDIF}
end;

function ASIOControlPanel: Integer; cdecl;
begin
{$IFDEF Debug}
  LogMessage('Control Panel');
{$ENDIF}

  if Assigned(GHost) then
    Result := GHost.ControlPanel
  else
    Result := 1;
end;

{ TLabviewASIO }

function findDrvPath(const clsidstr: string; var dllpath: string): Integer;
var
  reg: TRegistry;
  success: Boolean;
  buf: array [0 .. 1024] of char;
  s: string;
  temps: string;
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
  Count: Integer;
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
    Count := 0;
    for i := 0 to keys.Count - 1 do
    begin
      success := r.OpenKeyReadOnly(ASIO_PATH + '\' + keys[i]);
      if success then
      begin
        id := r.ReadString(COM_CLSID);
        if findDrvPath(id, dllpath) = 0 then // check if the dll exists
        begin
          SetLength(List, Count + 1);
          List[Count].id := StringToGUID(id);
          StrPLCopy(List[Count].name, keys[i], 512);
          StrPLCopy(List[Count].path, dllpath, 512);
          Inc(Count);
        end;
        r.CloseKey;
      end;
    end;
  finally
    keys.Free;
    r.Free;
  end;
end;

constructor TASIOTimeSub.Create;
begin
  FBufferTime.timeInfo.Speed := 1;
  FBufferTime.timeInfo.SampleRate := 44100;
  FBufferTime.timeInfo.SamplePosition := Int64ToASIOSamples(0);
  Flags := [atSystemTimeValid, atSamplePositionValid, atSampleRateValid,
    atSpeedValid];
end;

procedure TASIOTimeSub.Change;
begin
  if Assigned(FOnChange) then
    FOnChange(Self);
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

function ChannelTypeToString(vType: TASIOSampleType): string;
begin
  Result := '';
  case vType of
    CASIOSTInt16MSB:
      Result := 'Int16MSB';
    CASIOSTInt24MSB:
      Result := 'Int24MSB';
    CASIOSTInt32MSB:
      Result := 'Int32MSB';
    CASIOSTFloat32MSB:
      Result := 'Float32MSB';
    CASIOSTFloat64MSB:
      Result := 'Float64MSB';

    // these are used for 32 bit data buffer, with different alignment of the data inside
    // 32 bit PCI bus systems can be more easily used with these
    CASIOSTInt32MSB16:
      Result := 'Int32MSB16';
    CASIOSTInt32MSB18:
      Result := 'Int32MSB18';
    CASIOSTInt32MSB20:
      Result := 'Int32MSB20';
    CASIOSTInt32MSB24:
      Result := 'Int32MSB24';

    CASIOSTInt16LSB:
      Result := 'Int16LSB';
    CASIOSTInt24LSB:
      Result := 'Int24LSB';
    CASIOSTInt32LSB:
      Result := 'Int32LSB';
    CASIOSTFloat32LSB:
      Result := 'Float32LSB';
    CASIOSTFloat64LSB:
      Result := 'Float64LSB';

    // these are used for 32 bit data buffer, with different alignment of the data inside
    // 32 bit PCI bus systems can more easily used with these
    CASIOSTInt32LSB16:
      Result := 'Int32LSB16';
    CASIOSTInt32LSB18:
      Result := 'Int32LSB18';
    CASIOSTInt32LSB20:
      Result := 'Int32LSB20';
    CASIOSTInt32LSB24:
      Result := 'Int32LSB24';
  end;
end;

procedure ASIOBufferSwitch(doubleBufferIndex: Integer;
  directProcess: TASIOBool); cdecl;
begin
  directProcess := CASIOFalse;
  case directProcess of
    CASIOFalse:
      begin
        PMBufSwitch.WParam := AM_BufferSwitch;
        PMBufSwitch.LParam := doubleBufferIndex;
        GHost.Dispatch(PMBufSwitch);
      end;
    CASIOTrue:
      GHost.BufferSwitch(doubleBufferIndex);
  end;
end;

function ASIOBufferSwitchTimeInfo(var params: TASIOTime;
  doubleBufferIndex: Integer; directProcess: TASIOBool): PASIOTime; cdecl;
begin
  directProcess := CASIOFalse;
  case directProcess of
    CASIOFalse:
      begin
        GHost.ASIOTime.FBufferTime := params;
        PMBufSwitchTimeInfo.WParam := AM_BufferSwitchTimeInfo;
        PMBufSwitchTimeInfo.LParam := doubleBufferIndex;
        GHost.Dispatch(PMBufSwitchTimeInfo);
      end;
    CASIOTrue:
      GHost.BufferSwitchTimeInfo(doubleBufferIndex, params);
  end;
  Result := nil;
end;

procedure ASIOSampleRateDidChange(sRate: TASIOSampleRate); cdecl;
begin
  if Assigned(GHost.FOnSampleRateChanged) then
    GHost.FOnSampleRateChanged(GHost);
end;

function ASIOMessage(selector, Value: Integer; Message: Pointer; opt: PDouble)
  : Integer; cdecl;
begin
  Result := 0;
  case selector of
    kASIOSelectorSupported: // return 1 if a selector is supported
      begin
        case Value of
          kASIOEngineVersion:
            Result := 1;
          kASIOResetRequest:
            Result := 1;
          kASIOBufferSizeChange:
            Result := 0;
          kASIOResyncRequest:
            Result := 1;
          kASIOLatenciesChanged:
            Result := 1;
          kASIOSupportsTimeInfo:
            Result := 1;
          kASIOSupportsTimeCode:
            Result := 1;
          kASIOSupportsInputMonitor:
            Result := 0;
        end;
      end;
    kASIOEngineVersion:
      Result := 2; // ASIO 2 is supported
    kASIOResetRequest:
      begin
        PMReset.Msg := PM_ASIO;
        PMReset.WParam := AM_ResetRequest;
        PMReset.LParam := 0;
        GHost.Dispatch(PMReset);
        Result := 1;
      end;
    kASIOBufferSizeChange:
      begin
        PMReset.Msg := PM_ASIO;
        PMReset.WParam := AM_ResetRequest;
        PMReset.LParam := 0;
        GHost.Dispatch(PMReset);
        Result := 1;
      end;
    kASIOResyncRequest:
      ;
    kASIOLatenciesChanged:
      begin
        PMReset.Msg := PM_ASIO;
        PMReset.WParam := AM_LatencyChanged;
        PMReset.LParam := 0;
        GHost.Dispatch(PMReset);
        Result := 1;
      end;
    kASIOSupportsTimeInfo:
      Result := 1;
    kASIOSupportsTimeCode:
      Result := 0;
    kASIOSupportsInputMonitor:
      Result := 0;
  end;
end;

constructor TLabviewASIO.Create;
begin
{$IFDEF Debug}
  LogMessage('Initialize Host');
{$ENDIF}
  FClipPrevent := ClipDigital.cb32;
  FXBSize := 0;
  FLoopCounts := 0;
  FBufferUnderruns := 0;
  GHost := Self;
  FCalcMeters := True;
  FUnAlignedBuffer := nil;
  FInputBuffer := nil;
  FOutputBuffer := nil;
  ASIOTime := TASIOTimeSub.Create;
  FDriverList := GetDriverList;
  FConvertOptimizations := [coSSE, co3DNow];

  // set the FCallbacks record fields
  FCallbacks.BufferSwitch := {$IFDEF FPC}@{$ENDIF}ASIOBufferSwitch;
  FCallbacks.sampleRateDidChange := {$IFDEF FPC}@{$ENDIF}ASIOSampleRateDidChange;
  FCallbacks.ASIOMessage := {$IFDEF FPC}@{$ENDIF}ASIOMessage;
  FCallbacks.BufferSwitchTimeInfo :=
  {$IFDEF FPC}@{$ENDIF}ASIOBufferSwitchTimeInfo;

  // set the FDriver itself to nil for now
  FDriver := nil;
  FBuffersCreated := False;

  // and make sure all controls are enabled or disabled
  FDriverIndex := -1;
  FInputMonitor := imDisabled;
  ResetPositions;

  inherited;
end;

destructor TLabviewASIO.Destroy;
begin
{$IFDEF Debug}
  LogMessage('Destroy!');
{$ENDIF}

  FCallbacks.BufferSwitchTimeInfo := nil;

  if Active then
    Active := False;

  CloseDriver;

{$IFDEF Debug}
  LogMessage('After CloseDriver');
{$ENDIF}

  SetLength(FAsioDriverList, 0);
  SetLength(FInConverters, 0);
  SetLength(FOutConverters, 0);
  SetLength(FOutputVolume, 0);
  SetLength(FInMeter, 0);
  SetLength(FOutMeter, 0);
  SetLength(FSineFrequencies, 0);
  SetLength(FSineStarts, 0);
  SetLength(FSineStarts, 0);

{$IFDEF Debug}
  LogMessage('After array resize');
{$ENDIF}

  FDriverList.Free;
  ASIOTime.Free;

{$IFDEF Debug}
  LogMessage('Before inherited');
{$ENDIF}

  inherited;
{$IFDEF Debug}
  LogMessage('Host closed');
{$ENDIF}
end;

/// /////////////////////////////////////////////////////////////////////////////

procedure TLabviewASIO.ResetPositions;
begin
  FReadPosition := 0;
  if FBufferSize < FXBSize then
    FReadPosition := FXBSize - FBufferSize
  else
    FReadPosition := 0;
  FWritePosition := 0;
end;

function TLabviewASIO.GetDriverList: TStrings;
var
  i: Integer;
begin
  Result := TStringList.Create;
  SetLength(FAsioDriverList, 0);
  ListAsioDrivers(FAsioDriverList);
  for i := Low(FAsioDriverList) to High(FAsioDriverList) do
    Result.Add(FAsioDriverList[i].name);
end;

procedure TLabviewASIO.SetDriverName(const s: String);
begin
  if FDriverList.IndexOf(s) > -1 then
    DriverIndex := FDriverList.IndexOf(s);
end;

procedure TLabviewASIO.SetInputChannelOffset(const w: Word);
begin
  if (w <> FInputChannelOffset) and (w < FInputChannels) then
    FInputChannelOffset := w;
end;

procedure TLabviewASIO.SetOutputChannelOffset(const w: Word);
begin
  if (w <> FOutputChannelOffset) and (w < FOutputChannels) then
    FOutputChannelOffset := w;
end;

procedure TLabviewASIO.SetConvertOptimizations(const co: TConvertOptimizations);
begin
  Use_FPU;
  case ProcessorType of
    ptFPU:
      if coSSE in co then
        Use_SSE;
    pt3DNow:
      if co3DNow in co then
        Use_3DNow;
  end;
  FConvertOptimizations := co;
end;

procedure TLabviewASIO.SetDriverIndex(Value: Integer);
var
  DriverName: array [0 .. 255] of AnsiChar;
  tmpActive: Boolean;
begin
  if (Value <> FDriverIndex) then
  begin
    tmpActive := Active;
    Active := False;
    if Value < -1 then
      FDriverIndex := -1
    else if Value >= FDriverList.Count then
      FDriverIndex := FDriverList.Count - 1
    else
      FDriverIndex := Value;
    if FDriverIndex = -1 then
    begin
      FDriverName := '';
      FInputLatency := 0;
      FOutputLatency := 0;
      FInputChannels := 0;
      FOutputChannels := 0;
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
        FDriver.GetDriverName(DriverName);
        FDriverVersion := FDriver.GetDriverVersion;
      end;
    end;
    if Assigned(FOnDriverChanged) then
      OnDriverChanged(Self);
    Active := tmpActive;
  end;
end;

procedure TLabviewASIO.SetPreventClipping(v: TPreventClipping);
begin
  FPreventClipping := v;
  case FPreventClipping of
    pcDigital:
      FClipPrevent := ClipDigital.cb32;
    pcAnalog:
      FClipPrevent := ClipAnalog.cb32;
  end;
end;

procedure TLabviewASIO.SetXBSize(const Value: Integer);
var
  i: Integer;

  function Max(const A, B: Integer): Integer;
  begin
    if A > B then
      Result := A
    else
      Result := B;
  end;

begin
  if FXBSize <> Value then
    try
      FBufferUnderruns := 0;
      ResetPositions;
      FXBSize := Max(Value, BufferSize);
      for i := 0 to FInputChannels - 1 do
        SetLength(FInBuf[i], FXBSize);
      for i := 0 to FOutputChannels - 1 do
        SetLength(FOutBuf[i], FXBSize);
      FXBSizeH := FXBSize div 2;
      FXBSizeV := FXBSize div 4;
    finally
      ResetPositions;
    end;
end;

function TLabviewASIO.GetInConverter(ConverterType: TAsioSampleType): TInConverter;
begin
 case ConverterType of
  CAsioSTInt16MSB   : Result := FromInt16MSB;
  CAsioSTInt24MSB   : Result := FromInt24MSB;
  CAsioSTInt32MSB   : Result := FromInt32MSB;
  CAsioSTFloat32MSB : Result := FromSingleMSB;
  CAsioSTFloat64MSB : Result := FromDoubleMSB;
  CAsioSTInt32MSB16 : Result := FromInt32MSB16;
  CAsioSTInt32MSB18 : Result := FromInt32MSB18;
  CAsioSTInt32MSB20 : Result := FromInt32MSB20;
  CAsioSTInt32MSB24 : Result := FromInt32MSB24;
  CAsioSTInt16LSB   : Result := FromInt16LSB;
  CAsioSTInt24LSB   : Result := FromInt24LSB;
  CAsioSTInt32LSB   : Result := FromInt32LSB;
  CAsioSTFloat32LSB : Result := FromSingleLSB;
  CAsioSTFloat64LSB : Result := FromDoubleLSB;
  CAsioSTInt32LSB16 : Result := FromInt32LSB16;
  CAsioSTInt32LSB18 : Result := FromInt32LSB18;
  CAsioSTInt32LSB20 : Result := FromInt32LSB20;
  CAsioSTInt32LSB24 : Result := FromInt32LSB24;
  else
    raise Exception.Create(RStrConverterTypeUnknown);
 end;
end;

function TLabviewASIO.GetOutConverter(ConverterType: TAsioSampleType): TOutConverter;
begin
 case ConverterType of
  CAsioSTInt16MSB   : Result := ToInt16MSB;
  CAsioSTInt24MSB   : Result := ToInt24MSB;
  CAsioSTInt32MSB   : Result := ToInt32MSB;
  CAsioSTFloat32MSB : Result := ToSingleMSB;
  CAsioSTFloat64MSB : Result := ToDoubleMSB;
  CAsioSTInt32MSB16 : Result := ToInt32MSB16;
  CAsioSTInt32MSB18 : Result := ToInt32MSB18;
  CAsioSTInt32MSB20 : Result := ToInt32MSB20;
  CAsioSTInt32MSB24 : Result := ToInt32MSB24;
  CAsioSTInt16LSB   : Result := ToInt16LSB;
  CAsioSTInt24LSB   : Result := ToInt24LSB;
  CAsioSTInt32LSB   : Result := ToInt32LSB;
  CAsioSTFloat32LSB : Result := ToSingleLSB;
  CAsioSTFloat64LSB : Result := ToDoubleLSB;
  CAsioSTInt32LSB16 : Result := ToInt32LSB16;
  CAsioSTInt32LSB18 : Result := ToInt32LSB18;
  CAsioSTInt32LSB20 : Result := ToInt32LSB20;
  CAsioSTInt32LSB24 : Result := ToInt32LSB24;
  else raise Exception.Create(RStrConverterTypeUnknown);
 end;
end;

function TLabviewASIO.CreateBuffers: Boolean;
var
  i: Integer;
  CurrentBuffer: PASIOBufferInfo;
begin
  if FDriver = nil then
  begin
    Result := False;
    exit;
  end;
  if FBuffersCreated then
    DestroyBuffers;

  FDriver.GetBufferSize(FMin, FMax, FPref, FGranularity);
  if FMin = FMax then
    FPref := FMin;
  FBufferSize := FPref;
  FDriver.GetSampleRate(FSampleRate);
  if FSampleRate < 0 then
    FSampleRate := 44100;
  SetSampleRate(FSampleRate);
  FDriver.GetChannels(FInputChannels, FOutputChannels);

  SetLength(FOutputVolume, FOutputChannels);
  SetLength(FOutMeter, FOutputChannels);
  SetLength(FSineFrequencies, FOutputChannels);
  SetLength(FSineStates, FOutputChannels);
  SetLength(FSineStarts, FOutputChannels);
  SetLength(FLastSamples, FOutputChannels);
  SetLength(FOutBuf, FOutputChannels);
  SineFrequency[0] := 1000;

  for i := 0 to FOutputChannels - 1 do
  begin
    FOutputVolume[i] := 1;
    FSineStates[i].Re := 0;
    FSineStates[i].Im := 1;
  end;

  GetMem(FUnAlignedBuffer, SizeOf(TAsioBufferInfo) *
    (FInputChannels + FOutputChannels) + $F);
  FInputBuffer := PAsioBufferInfo(NativeUInt(FUnAlignedBuffer) and (not $F));

  SetLength(FInputChannelInfos, FInputChannels);
  SetLength(FSingleInBuffer, FInputChannels);
  SetLength(FInConverters, FInputChannels);
  SetLength(FInMeter, FInputChannels);
  SetLength(FInBuf, FInputChannels);
  CurrentBuffer := FInputBuffer;
  for i := 0 to FInputChannels - 1 do
  begin
    FInputChannelInfos[i].Channel := i;
    FInputChannelInfos[i].isInput := CASIOTrue;

    FDriver.GetChannelInfo(FInputChannelInfos[i]);
    FInConverters[i] := GetInConverter(FInputChannelInfos[i].SampleType);

    SetLength(FSingleInBuffer[i], BufferSize);
    FillChar(FSingleInBuffer[i, 0], BufferSize * SizeOf(Single), 0);
    CurrentBuffer^.isInput := CASIOTrue;
    CurrentBuffer^.channelNum := i;
    CurrentBuffer^.buffers[0] := nil;
    CurrentBuffer^.buffers[1] := nil;
    Inc(CurrentBuffer);
  end;

  FOutputBuffer := CurrentBuffer;
  SetLength(FOutputChannelInfos, FOutputChannels);
  SetLength(FSingleOutBuffer, FOutputChannels);
  SetLength(FOutConverters, FOutputChannels);
  for i := 0 to FOutputChannels - 1 do
  begin
    FOutputChannelInfos[i].Channel := i;
    FOutputChannelInfos[i].isInput := CASIOFalse; // output

    FDriver.GetChannelInfo(FOutputChannelInfos[i]);
    FOutConverters[i] := GetOutConverter(FOutputChannelInfos[i].SampleType);

    SetLength(FSingleOutBuffer[i], BufferSize);
    FillChar(FSingleOutBuffer[i, 0], BufferSize * SizeOf(Single), 0);
    CurrentBuffer^.isInput := CASIOFalse; // create an output buffer
    CurrentBuffer^.channelNum := i;
    CurrentBuffer^.buffers[0] := nil;
    CurrentBuffer^.buffers[1] := nil;
    Inc(CurrentBuffer);
  end;

{$IFDEF Debug}
  LogMessage('Before CreateBuffers');
{$ENDIF}

  Result := FDriver.CreateBuffers(PASIOBufferInfos(FInputBuffer),
    FInputChannels + FOutputChannels, FPref, FCallbacks) = ASE_OK;
  FDriver.GetLatencies(FInputLatency, FOutputLatency);
  if Assigned(FOnLatencyChanged) then
    FOnLatencyChanged(Self);
  Randomize;
  ExtraBufferSize := 8192;
end;

procedure TLabviewASIO.DestroyBuffers;
begin
  if (FDriver = nil) then
    Exit;

{$IFDEF Debug}
  LogMessage('Destroy buffers');
{$ENDIF}

  if FBuffersCreated then
  begin
{$IFDEF Debug}
    LogMessage('Before DisposeBuffers');
{$ENDIF}
    try
      if FDriver.DisposeBuffers <> ASE_OK then
      begin
{$IFDEF Debug}
        LogMessage('Error DisposingBuffers');
{$ENDIF}
      end;
    except
    end;

{$IFDEF Debug}
    LogMessage('After DisposeBuffers');
{$ENDIF}

    if Assigned(FUnAlignedBuffer) then
      FreeMem(FUnAlignedBuffer);

    FUnAlignedBuffer := nil;
    FInputBuffer := nil;
    FOutputBuffer := nil;

    FBuffersCreated := False;
    FSingleInBuffer := nil;
    FSingleOutBuffer := nil;
    SetLength(FInputChannelInfos, 0);
    SetLength(FOutputChannelInfos, 0);
  end;
end;

procedure TLabviewASIO.OpenDriver;
var
  tmpActive: Boolean;
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
  begin
    try
      if CreateStdCallASIO(FAsioDriverList[FDriverIndex].id, FDriver) then
        if not Succeeded(FDriver.Init(LongWord(ASIODriverInfo.SysRef))) then
          FDriver := nil;
    except
      FDriver := nil;
    end;
  end;
  // if FDriver = nil then raise Exception.Create('ASIO Driver Failed!');
  FBuffersCreated := CreateBuffers;
  if tmpActive then
    Active := True;
end;

procedure TLabviewASIO.CloseDriver;
begin
{$IFDEF Debug}
  LogMessage('CloseDriver');
{$ENDIF}

  if Assigned(FDriver) then
  begin
    if FBuffersCreated then
      DestroyBuffers;
    FDriver := nil; // RELEASE;
  end;

{$IFDEF Debug}
  LogMessage('Clear properties');
{$ENDIF}
  FInputLatency := 0;
  FOutputLatency := 0;
  FInputChannels := 0;
  FOutputChannels := 0;
  FSampleRate := 44100;
end;

function TLabviewASIO.ControlPanel: Integer;
begin
  if Assigned(FDriver) then
    Result := FDriver.ControlPanel
  else
    Result := 1;
end;

procedure TLabviewASIO.Reset;
begin
  OpenDriver; // restart the FDriver
  if Assigned(FOnReset) then
    FOnReset(Self);
end;

procedure TLabviewASIO.PMASIO(var Message: TMessage);
var
  inp, outp: Integer;
begin
  if FDriver = nil then
    exit;
  case Message.WParam of
    AM_ResetRequest:
      begin
        OpenDriver; // restart the FDriver
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

procedure TLabviewASIO.PMUpdateSamplePos(var Message: TMessage);
var
  Samples: TASIOSamples;
begin
  Samples.Hi := Message.WParam;
  Samples.Lo := Message.LParam;
  if Assigned(FOnUpdateSamplePos) then
    FOnUpdateSamplePos(Self, ASIOSamplesToInt64(Samples));
end;

procedure TLabviewASIO.BufferSwitch(Index: Integer);
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

procedure TLabviewASIO.BufferSwitchTimeInfo(Index: Integer;
  const params: TASIOTime);
var
  i, j, n: Integer;
  CurrentBuffer: PASIOBufferInfo;
  PChannelArray: Pointer;
  tmp: Single;
begin
  if FDriver = nil then
    exit;
  PMUpdSamplePos.WParam := params.timeInfo.SamplePosition.Hi;
  PMUpdSamplePos.LParam := params.timeInfo.SamplePosition.Lo;
  Dispatch(PMUpdSamplePos);

  CurrentBuffer := FInputBuffer;
  for j := 0 to FInputChannels - 1 do
  begin
    if FInBufferPreFill = bpfZero then
      FillChar(FSingleInBuffer[j, 0], BufferSize * SizeOf(Single), 0)
    else if FInBufferPreFill = bpfNoise then
      for i := 0 to BufferSize - 1 do
        FSingleInBuffer[j, i] := 2 * Random - 1
    else
    begin
      PChannelArray := CurrentBuffer^.buffers[Index];
      if Assigned(PChannelArray) then
        FInConverters[j].ic32(PChannelArray, PSingle(FSingleInBuffer[j]), BufferSize);
      Inc(CurrentBuffer);
    end;
  end;

  if FPreventClipping <> pcNone then
    for j := 0 to FInputChannels - 1 do
      FClipPrevent(@FSingleInBuffer[j, 0], BufferSize);

  (*
    {$IFDEF Debug}
    LogMessage('inside');
    {$ENDIF}
  *)

  if FOutBufferPreFill = bpfZero then
    for j := 0 to FOutputChannels - 1 do
      FillChar(FSingleOutBuffer[j, 0], BufferSize * SizeOf(Single), 0)
  else if FOutBufferPreFill = bpfSine then
    for j := 0 to FOutputChannels - 1 do
      for i := 0 to BufferSize - 1 do
      begin
        FSingleOutBuffer[j, i] :=
          (FSineStarts[j].Re * FSineStates[j].Re - FSineStarts[j].Im *
          FSineStates[j].Im);
        FSineStates[j].Im := FSineStates[j].Im * FSineStarts[j].Re + FSineStates
          [j].Re * FSineStarts[j].Im;
        FSineStates[j].Re := FSingleOutBuffer[j, i];
        FSingleOutBuffer[j, i] := FOutputVolume[j] * FSingleOutBuffer[j, i];
        FLastSamples[j] := FSingleOutBuffer[j, i];
      end
  else if FOutBufferPreFill = bpfNoise then
    for j := 0 to FOutputChannels - 1 do
      for i := 0 to BufferSize - 1 do
        FSingleOutBuffer[j, i] := FOutputVolume[j] * (2 * Random - 1)
  else if (FLoopCounts > 0) and (FBufferUnderruns >= FLoopCounts) then
    for j := 0 to FOutputChannels - 1 do
      FillChar(FSingleOutBuffer[j, 0], BufferSize * SizeOf(Single), 0)
  else
    try
      if FXBSize - FWritePosition >= FBufferSize then
      begin
        for n := 0 to FOutputChannels - 1 do
        begin
          Move(FOutBuf[n, FWritePosition], FSingleOutBuffer[n, 0],
            FBufferSize * SizeOf(Single));
          Move(FSingleInBuffer[n, 0], FInBuf[n, FWritePosition],
            FBufferSize * SizeOf(Single));
        end;
        if (FWritePosition < FReadPosition) then
          if (FWritePosition + FBufferSize > FReadPosition) then
            Inc(FBufferUnderruns);
        FWritePosition := FWritePosition + FBufferSize;
        if FWritePosition >= FXBSize then
          FWritePosition := 0;
      end
      else
      begin
        for n := 0 to FOutputChannels - 1 do
        begin
          Move(FOutBuf[n, FWritePosition], FSingleOutBuffer[n, 0],
            (FXBSize - FWritePosition) * SizeOf(Single));
          Move(FSingleInBuffer[n, 0], FInBuf[n, FWritePosition],
            (FXBSize - FWritePosition) * SizeOf(Single));
        end;
        if (FWritePosition < FReadPosition) then
          Inc(FBufferUnderruns);
        FWritePosition := FBufferSize - (FXBSize - FWritePosition);
        if (FLoopCounts > 0) and (FBufferUnderruns < FLoopCounts) then
          for n := 0 to FOutputChannels - 1 do
          begin
            Move(FOutBuf[n, 0], FSingleOutBuffer[n, 0],
              FWritePosition * SizeOf(Single));
            Move(FSingleInBuffer[n, 0], FInBuf[n, 0],
              FWritePosition * SizeOf(Single));
          end;
      end;
    except
    end;

  if FCalcMeters then
  begin
    for j := 0 to FOutputChannels - 1 do
      for i := 0 to BufferSize - 1 do
      begin
        tmp := Abs(FSingleOutBuffer[j, i]);
        if tmp > FOutMeter[j] then
          FOutMeter[j] := tmp
        else
          FOutMeter[j] := 0.9999 * FOutMeter[j];
      end;

    for j := 0 to FInputChannels - 1 do
      for i := 0 to BufferSize - 1 do
      begin
        tmp := Abs(FSingleInBuffer[j, i]);
        if tmp > FInMeter[j] then
          FInMeter[j] := tmp
        else
          FInMeter[j] := 0.9999 * FInMeter[j];
      end;
  end;

  if FPreventClipping <> pcNone then
    for j := 0 to FOutputChannels - 1 do
      FClipPrevent(@FSingleOutBuffer[j, 0], BufferSize);

  CurrentBuffer := FOutputBuffer;
  for j := 0 to FOutputChannels - 1 do
  begin
    PChannelArray := CurrentBuffer^.buffers[Index];
    if Assigned(PChannelArray) then
      FOutConverters[j].oc32(PSingle(FSingleOutBuffer[j]), PChannelArray, BufferSize);
    Inc(CurrentBuffer);
  end;
  if Assigned(FDriver) then
    FDriver.OutputReady;
  FWatchDog := True;
end;

procedure TLabviewASIO.SetSampleRate(const Value: Double);
begin
  FSampleRate := Value;
  ASIOTime.SampleRate := Value;
  if Assigned(FDriver) then
    FDriver.SetSampleRate(Value);
end;

procedure TLabviewASIO.SetActive(Value: Boolean);
var
  CurrentBuffer: PASIOBufferInfo;
  i: Integer;
begin
  if FDriver = nil then
    Exit;
  if FActive = Value then
    Exit;

{$IFDEF Debug}
  LogMessage('SetActive');
{$ENDIF}

  if Value = True then
  begin
    try
{$IFDEF Debug}
      LogMessage('Start Audio (internal)');
{$ENDIF}
      FActive := (FDriver.Start = ASE_OK);
      FBufferUnderruns := 0;
      ResetPositions;
    except
      FBufferSize := 2048;
      FSampleRate := 44100;
    end;
    if FActive = False then
    begin
{$IFDEF Debug}
      LogMessage('Stop Audio (internal)');
{$ENDIF}
      FDriver.Stop;
    end;
  end
  else
  begin
    FActive := False;
    try
{$IFDEF Debug}
      LogMessage('Stop Audio (internal)');
{$ENDIF}
      FDriver.Stop;
    except
    end;
    if FBuffersCreated and False then
      try
        CurrentBuffer := FOutputBuffer;
        for i := 0 to FOutputChannels - 1 do
        begin
          FillChar(CurrentBuffer^.buffers[0]^, BufferSize * SizeOf(Single), 0);
          FillChar(CurrentBuffer^.buffers[1]^, BufferSize * SizeOf(Single), 0);
          Inc(CurrentBuffer);
        end;
        CurrentBuffer := FInputBuffer;
        for i := 0 to FInputChannels - 1 do
        begin
          FillChar(CurrentBuffer^.buffers[0]^, BufferSize * SizeOf(Single), 0);
          FillChar(CurrentBuffer^.buffers[1]^, BufferSize * SizeOf(Single), 0);
          Inc(CurrentBuffer);
        end;
      except
      end;
  end;
end;

function TLabviewASIO.GetNumDrivers: Integer;
begin
  Result := Length(FAsioDriverList);
end;

function TLabviewASIO.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  if Assigned(FDriver) then
    Result := FDriver.CanSampleRate(SampleRate)
  else
    Result := ASE_NotPresent;
end;

procedure TLabviewASIO.PMBufferSwitch(var Message: TMessage);
begin
  BufferSwitch(Message.LParam);
end;

procedure TLabviewASIO.PMBufferSwitchTimeInfo(var Message: TMessage);
begin
  BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);
end;

function TLabviewASIO.GetBufferSize: Cardinal;
begin
  if (FBufferSize < 1) or (FBufferSize > 65530) then
    FBufferSize := 4096;
  Result := FBufferSize;
end;

function TLabviewASIO.GetSampleRate: Double;
begin
  if (FSampleRate < 1) or (FSampleRate > 1048575) then
    FSampleRate := 44100;
  Result := FSampleRate;
end;

function TLabviewASIO.GetSineFrequency(Index: Integer): Single;
begin
  Result := FSineFrequencies[index];
end;

procedure TLabviewASIO.SetSineFrequency(Index: Integer; const Value: Single);
var
  d: Double;
  i: Integer;
begin
  if index = 0 then
  begin
    d := (2 * Value * PI) / FSampleRate;
    for i := 0 to OutputChannels - 1 do
    begin
      FSineStarts[i].Re := cos(d);
      FSineStarts[i].Im := sin(d);
      FSineFrequencies[i] := Value;
    end;
  end
  else
  begin
    d := (2 * Value * PI) / FSampleRate;
    FSineStarts[index - 1].Re := cos(d);
    FSineStarts[index - 1].Im := sin(d);
    FSineFrequencies[index - 1] := Value;
  end;
end;

procedure InitializeLibrary;
begin
  PMUpdSamplePos.Msg := PM_UpdateSamplePos;
  PMBufSwitch.Msg := PM_BufferSwitch;
  PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
  {$IFDEF DEBUG}
  GLog := TStringList.Create;
  {$ENDIF}
  GHost := TLabviewASIO.Create;

  LogMessage('Library initialized');
end;

procedure FinalizeLibrary;
begin
  LogMessage('Finalizing Library');

  if Assigned(GHost) then
    try
      GHost.PreFillOutBuffer := bpfNone;
      Sleep(20);
      GHost.Active := False;
      Sleep(20);
      GHost.Free;
      GHost := nil;
    except
    end;

{$IFDEF DEBUG}
  GLog.Free;
{$ENDIF}
end;

initialization
  InitializeLibrary;

finalization
  FinalizeLibrary;

end.
