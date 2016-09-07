unit DAV_AsioVi;

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

{$IFDEF Debug}
  {-$DEFINE DebugLog}
{$ENDIF}

uses
  Windows, Messages, {$IFDEF FPC} LCLIntf, {$ENDIF} Classes,
  DAV_ASIO, DAV_AsioInterface, DAV_AsioConvert, DAV_LabView;

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

  TOutputSource = (osCustom, osZero, osNoise, osSine);
  TPreventClipping = (pcNone, pcDigital, pcAnalog);
  TInputMonitor = (imDisabled, imMono, imStereo, imAll);

  TASIOTimeSub = class(TObject)
  type
    TATFlags = set of (atSystemTimeValid, atSamplePositionValid,
      atSampleRateValid, atSpeedValid, atSampleRateChanged,
      atClockSourceChanged);
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
    property SampleRate: Double Index 1 read GetATdouble write SetATdouble;
    property Flags: TATFlags read GetATflags Write SetATflags;
  end;

type
  TInputChannel = record
    CurrentData: TSingleArray;
    Buffer: TSingleArray;
    Converter: TInConverter;
    Meter: Single;
  end;

  TOutputChannel = record
    CurrentData: TSingleArray;
    Buffer: TSingleArray;
    Converter: TOutConverter;
    Meter: Single;
    Volume: Single;
    SineFrequency: Single;
    SineStart, SineState: TComplex;
  end;

  TAsioCanDos = set of (acdInputMonitor, acdTimeInfo, acdTimeCode, acdTransport,
    acdInputGain, acdInputMeter, acdOutputGain, acdOutputMeter, acdSetIoFormat,
    acdGetIoFormat, acdCanDoIoFormat);

  TLabviewASIO = class(TObject)
  private
    FDriver: IStdCallAsio;
    FASIOTime: TASIOTimeSub;
    FAsioDriverList: TAsioDriverList;
    FAsioCanDos: TAsioCanDos;
    FClockSources: array of TAsioClockSource;

    FActive: Boolean;
    FPreventClipping: TPreventClipping;
    FOutputSource: TOutputSource;

    FDriverIndex: Integer;
    FDriverList: TStrings;
    FDriverName: string;
    FDriverVersion: Integer;

    FInputLatency: Integer;
    FOutputLatency: Integer;
    FInputChannelCount: Integer;
    FOutputChannelCount: Integer;
    FSampleRate: Double;
    FBufferSize: Cardinal;
    FHandle: HWND;

    FMin, FMax, FPref, FGranularity: Integer;

    FInputChannel: array of TInputChannel;
    FOutputChannel: array of TOutputChannel;

    FCallbacks: TASIOCallbacks;

    FUnalignedBuffer: Pointer;
    FInputBuffer: PASIOBufferInfo;
    FOutputBuffer: PASIOBufferInfo;

    FLoopCounts, FBufferUnderruns: Integer;
    FReadPosition, FWritePosition: Integer;

    FInputMonitor: TInputMonitor;
    FConvertOptimizations: TConvertOptimizations;
    FClipPrevent: TClipPreventer;
    FXBSize, FXBSizeH, FXBSizeQ: Integer;
    FCalcMeters: Boolean;
    FWatchDog: Boolean;
    FUserEvent: TLVUserEventRef;

    function GetBufferSize: Cardinal;
    function GetSampleRate: Double;
    function GetSineFrequency(Index: Integer): Single;
    function GetInConverter(ConverterType: TAsioSampleType): TInConverter;
    function GetOutConverter(ConverterType: TAsioSampleType): TOutConverter;

    procedure CreateBuffers;
    procedure DestroyBuffers;

    procedure AquireCanDos;
    procedure AquireClockSources;
    procedure AquireDriverName;
    procedure AquireDriverVersion;
    procedure AquireSampleRate;
    procedure DetermineBuffersize;

    procedure BufferSwitch(Index: Integer);
    procedure BufferSwitchTimeInfo(Index: Integer; const params: TASIOTime);
    procedure ResetPositions;

    procedure LatencyChanged;
    procedure SampleRateChanged;

    procedure InitializeDriver(ID: TGUID);

    procedure SetActive(Value: Boolean);
    procedure SetConvertOptimizations(const co: TConvertOptimizations);
    procedure SetDriverIndex(Value: Integer);
    procedure SetDriverName(const s: String);
    procedure SetPreventClipping(v: TPreventClipping);
    procedure SetSampleRate(Value: Double);
    procedure SetSineFrequency(Index: Integer; const Value: Single);
    procedure SetXBSize(const Value: Integer);
  protected
    procedure WndProc(var Msg: TMessage);
    procedure PMASIO(var Message: TMessage); message PM_ASIO;
    procedure PMUpdateSamplePos(var Message: TMessage); message PM_UpdateSamplePos;
    procedure PMBufferSwitch(var Message: TMessage); message PM_BufferSwitch;
    procedure PMBufferSwitchTimeInfo(var Message: TMessage); message PM_BufferSwitchTimeInfo;
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
    property InputChannelCount: Integer read FInputChannelCount default 0;
    property InputLatency: Integer read FInputLatency default 0;
    property InputMonitor: TInputMonitor read FInputMonitor write FInputMonitor default imDisabled;
    property OutputChannelCount: Integer read FOutputChannelCount default 0;
    property OutputLatency: Integer read FOutputLatency default 0;
    property OutputSource: TOutputSource read FOutputSource write FOutputSource;
    property PreventClipping: TPreventClipping read FPreventClipping write SetPreventClipping;
    property SampleRate: Double read GetSampleRate write SetSampleRate;
    property SineFrequency[index: Integer]: Single read GetSineFrequency write SetSineFrequency;
  end;

procedure ASIOInitialize; cdecl;
procedure ASIOTerminate; cdecl;
function ASIOInitDriver(DriverName: PAnsiChar): Integer; cdecl;
function ASIOInitDriverIndex(Index: Integer): Integer; cdecl;
function ASIOGetNumDevices: Integer; cdecl;
function ASIOSetDriverIndex(Index: Integer): Integer; cdecl;
function ASIOGetDriverName(Index: Integer): PAnsiChar; cdecl;
function ASIOGetDriverNames(Names: PAnsiChar; lmaxDriverCount, lDriverNumber: Integer): Integer; cdecl;
function ASIODriverStart: Integer; cdecl;
function ASIODriverStop: Integer; cdecl;
function ASIOGetBufferSize(minSize, maxSize, preferredSize, granularity: PInteger): Integer; cdecl;
function ASIOControlPanel: Integer; cdecl;
function ASIOCanSampleRate(SampleRate: Double): Integer; cdecl;
function ASIOSetSampleRate(SampleRate: Double): Integer; cdecl;
function ASIOGetChannels(InputChannelCount, OutputChannelCount: PInteger): Integer; cdecl;
function ASIOOutputType(Index: Integer): Integer; cdecl;
function ASIOSetOutputVolume(Channel: Integer; Volume: Single): Integer; cdecl;
function ASIOSetOutputVolumedB(Channel: Integer; Volume: Single): Integer; cdecl;
function ASIOSetSineFrequency(Channel: Integer; Frequency: Single): Integer; cdecl;
function ASIOGetInputLevel(Channel: Integer): Single; cdecl;
function ASIOGetOutputLevel(Channel: Integer): Single; cdecl;
function ASIOReadWriteSize: Integer; cdecl;
function ASIOReadWriteSizeFixed: Integer; cdecl;
function ASIOReadWrite(Buffer: PDouble; Length, Channel: Integer): Integer; cdecl;
function ASIOReadWriteX(Buffer: Pointer; Length: Integer): Integer; cdecl;
function ASIOAutopilot(Buffer: Pointer; Length: Integer): Integer; cdecl;
function ASIOSetExtraBufferSize(Size: Integer): Integer; cdecl;
function ASIOBufferUnderrun: Integer; cdecl;
procedure ASIOResetBufferUnderruns; cdecl;
function ASIOGetLoopCounts: Integer; cdecl;
procedure ASIOSetLoopCounts(Loops: Integer); cdecl;
function ASIOSetClipFunction(ClipFunction: Integer): Integer; cdecl;
procedure ASIOCalcMeters(CalcMeters: Integer); cdecl;

function ASIORegisterCallback(InstanceData: PInstanceData): TManagerError; cdecl;
function ASIOUnegisterCallback(InstanceData: PInstanceData): TManagerError; cdecl;
function ASIOAbortCallback(InstanceData: PInstanceData): TManagerError; cdecl;
procedure ASIOSetUserEventRef(UserEventRef: PLVUserEventRef); cdecl;

procedure InitializeLibrary;
procedure FinalizeLibrary;

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
  RCStrDriverNotPresent = 'Driver not present';
  RCStrHardwareMalfunction = 'Hardware malfunctioning';
  RCStrInputParameterInvalid = 'Input parameter invalid';
  RCStrInvalidMode = 'Hardware is in a bad mode or used in a bad mode';
  RCStrSPNotAdvancing = 'Hardware is not running when sample position is inquired';
  RCStrNoClock = 'Sample clock or rate cannot be determined or is not present';
  RCStrNoMemory = 'Not enough memory for completing the request';
  RCStrPreferedBufferSize = 'Prefered buffer size invalid!';
  RCStrWrongSamplerate = 'Wrong Samplerate!';

var
  GHost: TLabviewASIO;
  PMUpdSamplePos: TMessage;
  PMBufSwitch: TMessage;
  PMBufSwitchTimeInfo: TMessage;
  PMReset: TMessage;
  RandSeed: Cardinal = $DEADBEAF;

{$IFDEF DebugLog}
  GLog: TStringList;

procedure LogMessage(Text: string); inline;
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
    Exit;
  end
  else
    Result := 1;

  for i := 0 to GHost.DriverList.Count - 1 do
  begin
    if GHost.DriverList[i] = s then
    begin
      GHost.DriverIndex := i;
      Result := 0;

      {$IFDEF DebugLog}
      LogMessage(Format('InitDriver (%s)', [s]));
      {$ENDIF}

      Break;
    end;
  end;
end;

function ASIOInitDriverIndex(Index: Integer): Integer; cdecl;
begin
  {$IFDEF DebugLog}
  LogMessage(Format('InitDriverIndex (%d)', [Index]));
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
  {$IFDEF DebugLog}
  LogMessage('GetNumDevices');
  {$ENDIF}

  if Assigned(GHost) then
    Result := GHost.DriverList.Count
  else
    Result := 0;
end;

function ASIOBufferUnderrun: Integer; cdecl;
begin
  {$IFDEF DebugLog}
  LogMessage('BufferUnderrun');
  {$ENDIF}

  Result := GHost.FBufferUnderruns;
end;

procedure ASIOResetBufferUnderruns; cdecl;
begin
  {$IFDEF DebugLog}
  LogMessage('BResetufferUnderrun');
  {$ENDIF}

  GHost.FBufferUnderruns := 0;
end;

function ASIOGetLoopCounts: Integer; cdecl;
begin
  {$IFDEF DebugLog}
  LogMessage('GetLoopCounts');
  {$ENDIF}

  Result := GHost.FLoopCounts;
end;

procedure ASIOSetLoopCounts(Loops: Integer); cdecl;
begin
  GHost.FLoopCounts := Loops;
{$IFDEF DebugLog}
  LogMessage('Set Loopcounts to: ' + IntToStr(Loops));
{$ENDIF}
end;

function ASIOSetOutputVolume(Channel: Integer; Volume: Single): Integer; cdecl;
var
  ChannelIndex: Integer;
begin
  Result := 0;
  if Assigned(GHost) then
    if (Channel > 0) and (Channel <= GHost.FOutputChannelCount) then
      GHost.FOutputChannel[Channel - 1].Volume := Volume
    else
      for ChannelIndex := 0 to GHost.FOutputChannelCount - 1 do
        GHost.FOutputChannel[ChannelIndex].Volume := Volume
  else
    Result := 1;
{$IFDEF DebugLog}
  LogMessage(Format('Set Output Volume on Channel %d to: %g', [Channel, Volume]));
{$ENDIF}
end;

// Convert a value in dB's to a linear amplitude
function dB_to_Amp(g: Double): Double; inline;
begin
  if (g > -150.0) then
    Result := Exp(g * 0.11512925465)
  else
    Result := 0;
end;

function ASIOSetOutputVolumedB(Channel: Integer; Volume: Single): Integer; cdecl;
var
  ChannelIndex: Integer;
begin
  Volume := dB_to_Amp(Volume);
  Result := 0;
  if Assigned(GHost) then
    if (Channel > 0) and (Channel <= GHost.FOutputChannelCount) then
      GHost.FOutputChannel[Channel - 1].Volume := Volume
    else
      for ChannelIndex := 0 to GHost.FOutputChannelCount - 1 do
        GHost.FOutputChannel[ChannelIndex].Volume := Volume
  else
    Result := 1;
{$IFDEF DebugLog}
  LogMessage(Format('Set Output Volume on Channel %d to: %g dB', [Channel,
    Volume]));
{$ENDIF}
end;

function ASIOSetSineFrequency(Channel: Integer; Frequency: Single): Integer; cdecl;
begin
{$IFDEF DebugLog}
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
{$IFDEF DebugLog}
  LogMessage('GetInputLevel');
{$ENDIF}

  if (Channel > 0) and (Channel <= GHost.FInputChannelCount) then
    Result := GHost.FInputChannel[Channel - 1].Meter
  else
    Result := 0;
end;

function ASIOGetOutputLevel(Channel: Integer): Single; cdecl;
begin
{$IFDEF DebugLog}
  LogMessage('GetOutputLevel');
{$ENDIF}

  if (Channel > 0) and (Channel <= GHost.FOutputChannelCount) then
    Result := GHost.FOutputChannel[Channel - 1].Meter
  else
    Result := 0;
end;

procedure ASIOCalcMeters(CalcMeters: Integer); cdecl;
begin
{$IFDEF DebugLog}
  LogMessage('CalcMeters');
{$ENDIF}

  try
    GHost.FCalcMeters := CalcMeters > 0;
  except
  end;
end;

function ASIOReadWriteSize: Integer; cdecl;
begin
{$IFDEF DebugLog}
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
    if Result > FXBSizeQ then
      Result := FXBSizeQ
    else
      Result := 0;
  end;
end;

function ASIOSetExtraBufferSize(Size: Integer): Integer; cdecl;
begin
{$IFDEF DebugLog}
  LogMessage(Format('Extra Buffer set to: %d samples', [Size]));
{$ENDIF}
  GHost.ExtraBufferSize := Size;
  Result := GHost.ExtraBufferSize;
end;

function ASIOReadWrite(Buffer: PDouble; Length, Channel: Integer)
  : Integer; cdecl;
var
  tmp: TDoubleArray absolute Buffer;
  ChannelIndex: Integer;
begin
  Result := 0;
  with GHost do
    if (Channel > 0) then
      if (Channel <= OutputChannelCount) and (Channel <= InputChannelCount) then
        try
          while Result < Length do
          begin
            GHost.FOutputChannel[Channel - 1].Buffer[FReadPosition] := tmp[Result];
            tmp[Result] := GHost.FInputChannel[Channel - 1].Buffer[FReadPosition];
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
            for ChannelIndex := 0 to GHost.FOutputChannelCount - 1 do
              GHost.FOutputChannel[ChannelIndex].Buffer[FReadPosition] := tmp[Result];
            for ChannelIndex := 0 to GHost.FInputChannelCount - 1 do
              tmp[Result] := GHost.FInputChannel[ChannelIndex].Buffer[FReadPosition];
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
  i, ChannelIndex: Integer;
  ChannelCount: array [0..1] of Integer;
begin
  try
    Result := Length;
    Buffer := Pointer(Buffer^);

    i := PInteger(Buffer)^;
    if i = Length then
      ChannelCount[0] := 1
    else
    begin
      ChannelCount[0] := i;
      Inc(PInteger(Buffer), 1);  // was Inc(Integer(Buffer), 4);
      i := PInteger(Buffer)^;
      if i <> Length then
        raise Exception.Create('ASIO: Length doesn''t fit');
      Inc(PInteger(Buffer), 1);  // was Inc(Integer(Buffer), 4);
    end;
    ChannelCount[1] := ChannelCount[0];

    with GHost do
    begin
      if ChannelCount[0] > InputChannelCount then
        ChannelCount[0] := InputChannelCount;
      if ChannelCount[1] > OutputChannelCount then
        ChannelCount[1] := OutputChannelCount;

      if FReadPosition + Length > FXBSize then
      begin
        // copy output data to output buffers
        for ChannelIndex := 0 to ChannelCount[1] - 1 do
          for i := 0 to FXBSize - FReadPosition - 1 do
            FOutputChannel[ChannelIndex].Buffer[FReadPosition + i] := PLVDoubleArray(Buffer)^[ChannelIndex * Length + i];

        // copy input data from input buffers
        for ChannelIndex := 0 to ChannelCount[0] - 1 do
          for i := 0 to FXBSize - FReadPosition - 1 do
            PLVDoubleArray(Buffer)^[ChannelIndex * Length + i] := FInputChannel[ChannelIndex].Buffer[FReadPosition + i];

        FReadPosition := Length - (FXBSize - FReadPosition);

        // copy output data to output buffers
        for ChannelIndex := 0 to ChannelCount[1] - 1 do
          for i := 0 to FReadPosition - 1 do
            FOutputChannel[ChannelIndex].Buffer[i] := PLVDoubleArray(Buffer)^[ChannelIndex * Length + i];

        // copy input data from input buffers
        for ChannelIndex := 0 to ChannelCount[0] - 1 do
          for i := 0 to FReadPosition - 1 do
            PLVDoubleArray(Buffer)^[ChannelIndex * Length + i] := FInputChannel[ChannelIndex].Buffer[i];
      end
      else
      begin
        // copy output data to output buffers
        for ChannelIndex := 0 to ChannelCount[1] - 1 do
          for i := 0 to Length - 1 do
            FOutputChannel[ChannelIndex].Buffer[FReadPosition + i] := PLVDoubleArray(Buffer)^[ChannelIndex * Length + i];

        // copy input data from input buffers
        for ChannelIndex := 0 to ChannelCount[0] - 1 do
          for i := 0 to Length - 1 do
            PLVDoubleArray(Buffer)^[ChannelIndex * Length + i] := FInputChannel[ChannelIndex].Buffer[FReadPosition + i];

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
{$IFDEF DebugLog}
      LogMessage('Autopilot started');
{$ENDIF}
      PreventClipping := pcDigital;
      Active := False;
      oldXBufSz := ExtraBufferSize;
      ExtraBufferSize := Length + FInputLatency + FOutputLatency + Integer(BufferSize);
      FReadPosition := 0;
      FWritePosition := 0;
      Result := ExtraBufferSize;
      if Channels > OutputChannelCount then
        Channels := OutputChannelCount;

      for j := 0 to Channels - 1 do
        try
          FillChar(FOutputChannel[j].Buffer[0], System.Length(FOutputChannel[j].Buffer) * SizeOf(Single), 0);
          for i := 0 to Length - 1 do
            FOutputChannel[j].Buffer[i] := TLVDoubleArray(PLVDoubleArray(Buffer)^)[j * Length + i];
        except
        end;

{$IFDEF DebugLog}
      LogMessage('Autopilot before playing/recording');
{$ENDIF}
      FWatchDog := False;
      FLoopCounts := 1;
      Active := True;

      if Active then
      begin
        Sleep(Round(2008 / SampleRate * (FInputLatency + FOutputLatency +
          Integer(BufferSize))));
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
{$IFDEF DebugLog}
      LogMessage('Autopilot before copying');
{$ENDIF}
      for j := 0 to Channels - 1 do
        try
          for i := 0 to Length - 1 do
            PLVDoubleArray(Buffer)^[j * Length + i] :=
              FInputChannel[j].Buffer[i + FInputLatency + FOutputLatency];
        except
        end;
      ExtraBufferSize := oldXBufSz;
{$IFDEF DebugLog}
      LogMessage('Autopilot finished');
{$ENDIF}
    end;
  except
    Result := 0;
{$IFDEF DebugLog}
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
{$IFDEF DebugLog}
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

function ASIOGetDriverNames(Names: PAnsiChar; lmaxDriverCount,
  lDriverNumber: Integer): Integer; cdecl;
var
  Current: string;
begin
  try
    if (lDriverNumber < GHost.DriverList.Count) and (lDriverNumber >= 0) then
    begin
      Current := GHost.DriverList[lDriverNumber];

{$IFDEF DebugLog}
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

procedure ASIOInitialize; cdecl;
begin
{$IFDEF DebugLog}
  if not Assigned(GLog) then
    GLog := TStringList.Create;
  LogMessage('ASIOInitialize');
{$ENDIF}
  if not Assigned(GHost) then
    GHost := TLabviewASIO.Create;
end;

procedure ASIOTerminate; cdecl;
begin
  FreeAndNil(GHost);
{$IFDEF DebugLog}
  FreeAndNil(GLog);
{$ENDIF}
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

function ASIOGetChannels(InputChannelCount, OutputChannelCount: PInteger): Integer; cdecl;
begin
  if Assigned(GHost) then
  begin
    InputChannelCount^ := GHost.InputChannelCount;
    OutputChannelCount^ := GHost.OutputChannelCount;
    Result := 0;
  end
  else
    Result := 1;
end;

function ASIOOutputType(Index: Integer): Integer; cdecl;
begin
  case Index of
    0:
      GHost.OutputSource := osCustom;
    1:
      GHost.OutputSource := osNoise;
    2:
      GHost.OutputSource := osSine;
  else
    GHost.OutputSource := osZero;
  end;
  Result := 0;
end;

function ASIODriverStart: Integer; cdecl;
begin
{$IFDEF DebugLog}
  LogMessage('Start Audio (before)');
{$ENDIF}
  if Assigned(GHost) then
    GHost.Active := True;
  Result := Integer(not GHost.Active);
{$IFDEF DebugLog}
  LogMessage('Start Audio (after)');
{$ENDIF}
end;

function ASIODriverStop: Integer; cdecl;
begin
{$IFDEF DebugLog}
  LogMessage('Stop Audio (before)');
{$ENDIF}
  if Assigned(GHost) then
    GHost.Active := False;
  Result := Integer(GHost.Active);
{$IFDEF DebugLog}
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
{$IFDEF DebugLog}
  LogMessage(Format('Extra Buffer set to: %f Hz', [SampleRate]));
{$ENDIF}
end;

function ASIOControlPanel: Integer; cdecl;
begin
{$IFDEF DebugLog}
  LogMessage('Control Panel');
{$ENDIF}

  if Assigned(GHost) then
    Result := GHost.ControlPanel
  else
    Result := 1;
end;

function ASIORegisterCallback(InstanceData: PInstanceData): TManagerError; cdecl;
begin

  Result := meNoError;
end;

function ASIOUnegisterCallback(InstanceData: PInstanceData): TManagerError; cdecl;
begin

  Result := meNoError;
end;

function ASIOAbortCallback(InstanceData: PInstanceData): TManagerError; cdecl;
begin

  Result := meNoError;
end;

procedure ASIOSetUserEventRef(UserEventRef: PLVUserEventRef); cdecl;
begin
  GHost.FUserEvent := UserEventRef^;
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

procedure ASIOSampleRateDidChange(SampleRate: TASIOSampleRate); cdecl;
begin
  GHost.SampleRate := SampleRate;
end;

function ASIOMessage(selector, Value: Integer; Message: Pointer;
  opt: PDouble): Integer; cdecl;
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

function FindDrvPath(const clsidstr: string; var dllpath: string): Integer;
var
  reg: TRegistry;
  success: Boolean;
  buf: array [0 .. 1024] of Char;
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


{ TASIOTimeSub }

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

{ TLabviewASIO }

constructor TLabviewASIO.Create;
begin
  GHost := Self;

  // allocate window handle
  FHandle := AllocateHWnd(WndProc);

{$IFDEF DebugLog}
  LogMessage('Initialize Host');
{$ENDIF}
  FClipPrevent := ClipDigital.cb32;
  FXBSize := 0;
  FLoopCounts := 0;
  FBufferUnderruns := 0;
  FCalcMeters := True;
  FUnalignedBuffer := nil;
  FInputBuffer := nil;
  FOutputBuffer := nil;
  FASIOTime := TASIOTimeSub.Create;
  FDriverList := GetDriverList;
  FConvertOptimizations := [coSSE, co3DNow];

  // set the FCallbacks record fields
  FCallbacks.BufferSwitch := {$IFDEF FPC}@{$ENDIF}ASIOBufferSwitch;
  FCallbacks.SampleRateDidChange := {$IFDEF FPC}@{$ENDIF}ASIOSampleRateDidChange;
  FCallbacks.ASIOMessage := {$IFDEF FPC}@{$ENDIF}ASIOMessage;
  FCallbacks.BufferSwitchTimeInfo := {$IFDEF FPC}@{$ENDIF}ASIOBufferSwitchTimeInfo;

  // set the FDriver itself to nil for now
  FDriver := nil;
  FUnalignedBuffer := nil;

  // and make sure all controls are enabled or disabled
  FDriverIndex := -1;
  FInputMonitor := imDisabled;
  ResetPositions;

  inherited;
end;

destructor TLabviewASIO.Destroy;
begin
{$IFDEF DebugLog}
  LogMessage('Destroy');
{$ENDIF}

  FCallbacks.BufferSwitch := nil;
  FCallbacks.BufferSwitchTimeInfo := nil;

  OutputSource := osZero;
  if Active then
    Active := False;
  CloseDriver;

{$IFDEF DebugLog}
  LogMessage('After CloseDriver');
{$ENDIF}

  SetLength(FAsioDriverList, 0);
  FDriverList.Free;
  ASIOTime.Free;

  DeallocateHWnd(FHandle);

  inherited;
{$IFDEF DebugLog}
  LogMessage('Host closed');
{$ENDIF}
end;

/// /////////////////////////////////////////////////////////////////////////////

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
  WasActive: Boolean;
begin
  // ensure input paramter range
  if Value < -1 then
    Value := -1
  else if Value >= FDriverList.Count then
    Value := FDriverList.Count - 1;

  // only continue on change
  if (Value = FDriverIndex) then
    Exit;

  WasActive := Active;
  Active := False;
  FDriverIndex := Value;
  if FDriverIndex = -1 then
  begin
    FDriverName := '';
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
      Exit;
    end;
  end;
  Active := WasActive;
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
      for i := 0 to FInputChannelCount - 1 do
        SetLength(FInputChannel[i].Buffer, FXBSize);
      for i := 0 to FOutputChannelCount - 1 do
        SetLength(FOutputChannel[i].Buffer, FXBSize);
      FXBSizeH := FXBSize div 2;
      FXBSizeQ := FXBSize div 4;
    finally
      ResetPositions;
    end;
end;

procedure TLabviewASIO.ResetPositions;
begin
  FReadPosition := 0;
  if Integer(FBufferSize) < FXBSize then
    FReadPosition := FXBSize - Integer(FBufferSize)
  else
    FReadPosition := 0;
  FWritePosition := 0;
end;

procedure TLabviewASIO.WndProc(var Msg: TMessage);
begin
  Dispatch(Msg);
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

procedure TLabviewASIO.DetermineBuffersize;
begin
  // query buffer sizes
  FDriver.GetBufferSize(FMin, FMax, FPref, FGranularity);

  if FMin = FMax then
    FPref := FMin;

  // check prefered buffersize is valid
  if FPref <= 0 then
    raise Exception.Create(RCStrPreferedBufferSize);

  FBufferSize := FPref;
end;

procedure TLabviewASIO.CreateBuffers;
var
  ChannelIndex: Integer;
  ChannelInfo: TASIOChannelInfo;
  CurrentBuffer: PASIOBufferInfo;
  ChannelsBufferSize: Integer;
begin
  // ensure a driver is present
  if FDriver = nil then
    Exit;

  // destroy buffers if already present
  if Assigned(FUnalignedBuffer) then
    DestroyBuffers;

  // get defaults
  DetermineBuffersize;
  AquireSampleRate;

  FDriver.GetChannels(FInputChannelCount, FOutputChannelCount);

  ChannelsBufferSize := SizeOf(TAsioBufferInfo) * (FInputChannelCount + FOutputChannelCount);
  FUnalignedBuffer := AllocMem(ChannelsBufferSize + $F);
  FInputBuffer := PAsioBufferInfo((NativeUInt(FUnalignedBuffer) + $F) and (not $F));

  SetLength(FInputChannel, FInputChannelCount);
  CurrentBuffer := FInputBuffer;

  for ChannelIndex := 0 to FInputChannelCount - 1 do
  begin
    // channel info management
    ChannelInfo.Channel := ChannelIndex;
    ChannelInfo.isInput := CASIOTrue;

    FDriver.GetChannelInfo(ChannelInfo);
    FInputChannel[ChannelIndex].Converter := GetInConverter(ChannelInfo.SampleType);

    SetLength(FInputChannel[ChannelIndex].CurrentData, BufferSize);
    FillChar(FInputChannel[ChannelIndex].CurrentData[0], BufferSize * SizeOf(Single), 0);

    FillChar(CurrentBuffer^, SizeOf(TAsioBufferInfo), 0);
    CurrentBuffer^.isInput := CASIOTrue;
    CurrentBuffer^.channelNum := ChannelIndex;
    Inc(CurrentBuffer);
  end;

  FOutputBuffer := CurrentBuffer;
  SetLength(FOutputChannel, FOutputChannelCount);

  for ChannelIndex := 0 to FOutputChannelCount - 1 do
  begin
    // channel info management
    ChannelInfo.Channel := ChannelIndex;
    ChannelInfo.isInput := CASIOFalse; // output

    FDriver.GetChannelInfo(ChannelInfo);
    FOutputChannel[ChannelIndex].Converter := GetOutConverter(ChannelInfo.SampleType);

    FOutputChannel[ChannelIndex].Volume := 1;
    FOutputChannel[ChannelIndex].SineFrequency := 1000;
    FOutputChannel[ChannelIndex].SineStart.Re := cos((2000 * PI) / FSampleRate);
    FOutputChannel[ChannelIndex].SineStart.Im := sin((2000 * PI) / FSampleRate);
    FOutputChannel[ChannelIndex].SineState.Re := 0;
    FOutputChannel[ChannelIndex].SineState.Im := 1;

    SetLength(FOutputChannel[ChannelIndex].CurrentData, BufferSize);
    FillChar(FOutputChannel[ChannelIndex].CurrentData[0], BufferSize * SizeOf(Single), 0);

    CurrentBuffer^.isInput := CASIOFalse; // create an output buffer
    CurrentBuffer^.channelNum := ChannelIndex;
    CurrentBuffer^.buffers[0] := nil;
    CurrentBuffer^.buffers[1] := nil;
    Inc(CurrentBuffer);
  end;

{$IFDEF DebugLog}
  LogMessage('Before CreateBuffers');
{$ENDIF}

  if FDriver.CreateBuffers(PASIOBufferInfos(FInputBuffer),
    FInputChannelCount + FOutputChannelCount, FPref, FCallbacks) <> ASE_OK then
  begin
    FreeMem(FUnalignedBuffer);
    FUnalignedBuffer := nil;
    Exit;
  end;

  FDriver.GetLatencies(FInputLatency, FOutputLatency);

  Randomize;
  ExtraBufferSize := 8192;
end;

procedure TLabviewASIO.DestroyBuffers;
begin
  if (FDriver = nil) or not Assigned(FUnalignedBuffer) then
    Exit;

{$IFDEF DebugLog}
  LogMessage('DestroyBuffers');
{$ENDIF}

  if FDriver.DisposeBuffers <> ASE_OK then
  begin
{$IFDEF DebugLog}
    LogMessage('Error DisposingBuffers');
{$ENDIF}
  end;

{$IFDEF DebugLog}
  LogMessage('After DisposeBuffers');
{$ENDIF}

  if Assigned(FUnalignedBuffer) then
    FreeMem(FUnalignedBuffer);

  FUnalignedBuffer := nil;
  FInputBuffer := nil;
  FOutputBuffer := nil;
end;

procedure TLabviewASIO.InitializeDriver(ID: TGUID);
var
  ErrorMessage: PAnsiChar;
begin
  if CreateStdCallASIO(ID, FDriver) then
    case FDriver.Init(FHandle) of
      0:
        begin
          // equals false
          GetMem(ErrorMessage, 128);
          try
            FDriver.GetErrorMessage(ErrorMessage);
            raise Exception.Create(string(ErrorMessage));
          finally
            Dispose(ErrorMessage);
          end;
        end;

      // the below codes are here due to incompatibility of some soundcards
      ASE_NotPresent:
        raise Exception.Create(RCStrDriverNotPresent);
      ASE_HWMalfunction:
        raise Exception.Create(RCStrHardwareMalfunction);
      ASE_InvalidParameter:
        raise Exception.Create(RCStrInputParameterInvalid);
      ASE_InvalidMode:
        raise Exception.Create(RCStrInvalidMode);
      ASE_SPNotAdvancing:
        raise Exception.Create(RCStrSPNotAdvancing);
      ASE_NoClock:
        raise Exception.Create(RCStrNoClock);
      ASE_NoMemory:
        raise Exception.Create(RCStrNoMemory);
    end;

  // aquire driver name
  AquireDriverName;

  // aquire driver version
  AquireDriverVersion;

  // aquire can dos
  AquireCanDos;

  // aquire clock sources
  AquireClockSources;
end;

procedure TLabviewASIO.OpenDriver;
var
  WasActive: Boolean;
begin
  WasActive := False;
  if Assigned(FDriver) then
  begin
    try
      WasActive := Active;
      Active := False;
      CloseDriver;
    except
    end;
  end;

  if FDriverIndex >= 0 then
  begin
    try
      InitializeDriver(FAsioDriverList[FDriverIndex].id);
    except
      FDriver := nil;
    end;
  end;

  // aquire can dos
  AquireCanDos;

  CreateBuffers;
  if WasActive then
    Active := True;
end;

procedure TLabviewASIO.CloseDriver;
begin
{$IFDEF DebugLog}
  LogMessage('CloseDriver');
{$ENDIF}

  if Assigned(FDriver) then
  begin
    DestroyBuffers;
    try
      FDriver := nil; // RELEASE;
    except
    end;
  end;

{$IFDEF DebugLog}
  LogMessage('Clear properties');
{$ENDIF}
  FInputLatency := 0;
  FOutputLatency := 0;
  FInputChannelCount := 0;
  FOutputChannelCount := 0;
  FSampleRate := 44100;
end;

procedure TLabviewASIO.AquireClockSources;
var
  ClockSources: PAsioClockSources;
  ClockSourceCount: Integer;
begin
  ClockSourceCount := 8;
  GetMem(ClockSources, ClockSourceCount * SizeOf(TAsioClockSource));
  try
    FillChar(ClockSources^, ClockSourceCount * SizeOf(TAsioClockSource), 0);
    if FDriver.GetClockSources(ClockSources, ClockSourceCount) = ASE_OK then
    begin
      SetLength(FClockSources, ClockSourceCount);
      Move(ClockSources^, FClockSources[0], ClockSourceCount *
        SizeOf(TAsioClockSource));
    end;
  finally
    Dispose(ClockSources);
  end;
end;

procedure TLabviewASIO.AquireDriverName;
var
  DriverName: array [0 .. 255] of AnsiChar;
begin
  FDriver.GetDriverName(DriverName);
  if DriverName <> '' then
    FDriverName := string(DriverName);
end;

procedure TLabviewASIO.AquireDriverVersion;
begin
  FDriverVersion := FDriver.GetDriverVersion;
end;

procedure TLabviewASIO.AquireSampleRate;
begin
  FDriver.GetSampleRate(FSampleRate);
  SampleRateChanged;
end;

procedure TLabviewASIO.AquireCanDos;
begin
  // check whether driver is has been assigned
  if FDriver = nil then
  begin
    FAsioCanDos := [];
    Exit;
  end;

  // test can do 'Time Info'
  if FDriver.Future(kAsioCanTimeInfo, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdTimeInfo)
  else
    Exclude(FAsioCanDos, acdTimeInfo);

  // test can do 'time code'
  if FDriver.Future(kAsioCanTimeCode, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdTimeCode)
  else
    Exclude(FAsioCanDos, acdTimeCode);

  // test can do 'transport'
  if FDriver.Future(kAsioCanTransport, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdTransport)
  else
    Exclude(FAsioCanDos, acdTransport);

  // test can do 'input gain'
  if FDriver.Future(kAsioCanInputGain, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdInputGain)
  else
    Exclude(FAsioCanDos, acdInputGain);

  // test can do 'input meter'
  if FDriver.Future(kAsioCanInputMeter, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdInputMeter)
  else
    Exclude(FAsioCanDos, acdInputMeter);

  // test can do 'output gain'
  if FDriver.Future(kAsioCanOutputGain, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdOutputGain)
  else
    Exclude(FAsioCanDos, acdOutputGain);

  // test can do 'output meter'
  if FDriver.Future(kAsioCanOutputMeter, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdOutputMeter)
  else
    Exclude(FAsioCanDos, acdOutputMeter);

  // test can do 'set I/O format'
  if FDriver.Future(kAsioSetIoFormat, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdSetIoFormat)
  else
    Exclude(FAsioCanDos, acdSetIoFormat);

  // test can do 'get I/O format'
  if FDriver.Future(kAsioSetIoFormat, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdGetIoFormat)
  else
    Exclude(FAsioCanDos, acdGetIoFormat);

  // test can do 'can do I/O format'
  if FDriver.Future(kAsioSetIoFormat, nil) = ASE_SUCCESS then
    Include(FAsioCanDos, acdCanDoIoFormat)
  else
    Exclude(FAsioCanDos, acdCanDoIoFormat);
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
  // restart the driver
  OpenDriver;
end;

procedure TLabviewASIO.LatencyChanged;
begin
  if Assigned(FDriver) then
    FDriver.GetLatencies(FInputLatency, FOutputLatency);
end;

procedure TLabviewASIO.PMASIO(var Message: TMessage);
begin
  if FDriver = nil then
    Exit;

  case Message.WParam of
    AM_ResetRequest:
      Reset;
    AM_BufferSwitch:
      // process a buffer
      BufferSwitch(Message.LParam);
    AM_BufferSwitchTimeInfo:
      // process a buffer with time
      BufferSwitchTimeInfo(Message.LParam, ASIOTime.FBufferTime);
    AM_LatencyChanged:
      LatencyChanged;
  end;
end;

procedure TLabviewASIO.PMUpdateSamplePos(var Message: TMessage);
var
  Samples: TASIOSamples;
begin
  Samples.Hi := Message.WParam;
  Samples.Lo := Message.LParam;
end;

procedure TLabviewASIO.BufferSwitch(Index: Integer);
begin
  if FUserEvent <> 0 then
    PostLVUserEvent(FUserEvent, Pointer(FBufferSize));

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
  i, ChannelIndex: Integer;
  CurrentBuffer: PASIOBufferInfo;
  BufferPointer: Pointer;
  tmp: Single;
begin
  if FDriver = nil then
    Exit;
  PMUpdSamplePos.WParam := params.timeInfo.SamplePosition.Hi;
  PMUpdSamplePos.LParam := params.timeInfo.SamplePosition.Lo;
  Dispatch(PMUpdSamplePos);

  CurrentBuffer := FInputBuffer;
  for ChannelIndex := 0 to FInputChannelCount - 1 do
  begin
    BufferPointer := CurrentBuffer^.buffers[Index];
    if Assigned(BufferPointer) then
      with FInputChannel[ChannelIndex] do
        Converter.ic32(BufferPointer, @CurrentData[0], BufferSize);
    Inc(CurrentBuffer);
  end;

  if FPreventClipping <> pcNone then
    for ChannelIndex := 0 to FInputChannelCount - 1 do
      FClipPrevent(@FInputChannel[ChannelIndex].CurrentData[0], BufferSize);

  (*
    {$IFDEF DebugLog}
    LogMessage('inside');
    {$ENDIF}
  *)

  if FOutputSource = osZero then
    for ChannelIndex := 0 to FOutputChannelCount - 1 do
      FillChar(FOutputChannel[ChannelIndex].CurrentData[0], BufferSize * SizeOf(Single), 0)
  else if FOutputSource = osSine then
    for ChannelIndex := 0 to FOutputChannelCount - 1 do
      with FOutputChannel[ChannelIndex] do
        for i := 0 to BufferSize - 1 do
        begin
          CurrentData[i] := (SineStart.Re * SineState.Re - SineStart.Im * SineState.Im);
          SineState.Im := SineState.Im * SineStart.Re + SineState.Re * SineStart.Im;
          SineState.Re := CurrentData[i];
          CurrentData[i] := Volume * CurrentData[i];
        end
  else if FOutputSource = osNoise then
    for ChannelIndex := 0 to FOutputChannelCount - 1 do
      with FOutputChannel[ChannelIndex] do
        for i := 0 to BufferSize - 1 do
          CurrentData[i] := Volume * (2 * Random - 1)
  else if (FLoopCounts > 0) and (FBufferUnderruns >= FLoopCounts) then
    for ChannelIndex := 0 to FOutputChannelCount - 1 do
      FillChar(FOutputChannel[ChannelIndex].CurrentData[0], BufferSize * SizeOf(Single), 0)
  else
    try
      if FXBSize - FWritePosition >= Integer(FBufferSize) then
      begin
        // copy output
        for ChannelIndex := 0 to FOutputChannelCount - 1 do
          with FOutputChannel[ChannelIndex] do
            Move(Buffer[FWritePosition], CurrentData[0], FBufferSize * SizeOf(Single));

        // copy input
        for ChannelIndex := 0 to FInputChannelCount - 1 do
          with FInputChannel[ChannelIndex] do
            Move(CurrentData[0], Buffer[FWritePosition], FBufferSize * SizeOf(Single));

        if (FWritePosition < FReadPosition) then
          if (FWritePosition + Integer(FBufferSize) > FReadPosition) then
            Inc(FBufferUnderruns);
        FWritePosition := FWritePosition + Integer(FBufferSize);
        if FWritePosition >= FXBSize then
          FWritePosition := 0;
      end
      else
      begin
        // copy output
        for ChannelIndex := 0 to FOutputChannelCount - 1 do
          with FOutputChannel[ChannelIndex] do
            Move(Buffer[FWritePosition], CurrentData[0], (FXBSize - FWritePosition) * SizeOf(Single));

        // copy input
        for ChannelIndex := 0 to FInputChannelCount - 1 do
          with FInputChannel[ChannelIndex] do
            Move(CurrentData[0], Buffer[FWritePosition], (FXBSize - FWritePosition) * SizeOf(Single));

        if (FWritePosition < FReadPosition) then
          Inc(FBufferUnderruns);

        FWritePosition := Integer(FBufferSize) - (FXBSize - FWritePosition);
        if (FLoopCounts > 0) and (FBufferUnderruns < FLoopCounts) then
        begin
          for ChannelIndex := 0 to FOutputChannelCount - 1 do
            with FOutputChannel[ChannelIndex] do
              Move(Buffer[0], CurrentData[0], FWritePosition * SizeOf(Single));

          for ChannelIndex := 0 to FInputChannelCount - 1 do
            with FInputChannel[ChannelIndex] do
              Move(CurrentData[0], Buffer[0], FWritePosition * SizeOf(Single));
        end;
      end;
    except
    end;

  // calculate meters
  if FCalcMeters then
  begin
    // input meters
    for ChannelIndex := 0 to FOutputChannelCount - 1 do
      with FOutputChannel[ChannelIndex] do
        for i := 0 to BufferSize - 1 do
        begin
          tmp := Abs(CurrentData[i]);
          Meter := 0.9999 * Meter;
          if tmp > Meter then
            Meter := tmp
        end;

    // output meters
    for ChannelIndex := 0 to FInputChannelCount - 1 do
      with FInputChannel[ChannelIndex] do
        for i := 0 to BufferSize - 1 do
        begin
          tmp := Abs(CurrentData[i]);
          Meter := 0.9999 * Meter;
          if tmp > Meter then
            Meter := tmp
        end;
  end;

  // eventually prevent cliping
  if FPreventClipping <> pcNone then
    for ChannelIndex := 0 to FOutputChannelCount - 1 do
      FClipPrevent(@FOutputChannel[ChannelIndex].CurrentData[0], BufferSize);

  // convert data to output buffers
  CurrentBuffer := FOutputBuffer;
  for ChannelIndex := 0 to FOutputChannelCount - 1 do
  begin
    BufferPointer := CurrentBuffer^.buffers[Index];
    if Assigned(BufferPointer) then
      with FOutputChannel[ChannelIndex] do
        Converter.oc32(@CurrentData[0], BufferPointer, BufferSize);
    Inc(CurrentBuffer);
  end;

  if Assigned(FDriver) then
    FDriver.OutputReady;

  FWatchDog := True;
end;

procedure TLabviewASIO.SetSampleRate(Value: Double);
begin
  // check for a valid samplerate
  Value := Abs(Value);
  if (Value = 0) then
    raise Exception.Create(RCStrWrongSamplerate);

  // check if samplerate is supported
  if Assigned(FDriver) then
    if FDriver.CanSampleRate(Value) <> ASE_OK then
      Exit;

  if FSampleRate <> Value then
    if FDriver.SetSampleRate(Value) = ASE_OK then
    begin
      FSampleRate := Value;
      SampleRateChanged;
    end;
end;

procedure TLabviewASIO.SampleRateChanged;
begin
  // synchronize asio time with current samplerate
  AsioTime.SampleRate := FSampleRate;
end;

procedure TLabviewASIO.SetActive(Value: Boolean);
begin
  if (FDriver = nil) or (FActive = Value) then
    Exit;

{$IFDEF DebugLog}
  LogMessage('SetActive');
{$ENDIF}

  if Value = True then
  begin
    try
{$IFDEF DebugLog}
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
{$IFDEF DebugLog}
      LogMessage('Stop Audio (internal)');
{$ENDIF}
      FDriver.Stop;
    end;
  end
  else
  begin
    FActive := False;
    try
{$IFDEF DebugLog}
      LogMessage('Stop Audio (internal)');
{$ENDIF}
      FDriver.Stop;
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
  Result := FOutputChannel[index].SineFrequency;
end;

procedure TLabviewASIO.SetSineFrequency(Index: Integer; const Value: Single);
var
  Freq, Re, Im: Double;
  ChannelIndex: Integer;
begin
  if Index = 0 then
  begin
    Freq := (2 * Value * PI) / FSampleRate;
    Re := cos(Freq);
    Im := sin(Freq);
    for ChannelIndex := 0 to OutputChannelCount - 1 do
    begin
      FOutputChannel[ChannelIndex].SineStart.Re := Re;
      FOutputChannel[ChannelIndex].SineStart.Im := Im;
      FOutputChannel[ChannelIndex].SineFrequency := Value;
    end;
  end
  else
  begin
    Freq := (2 * Value * PI) / FSampleRate;
    FOutputChannel[Index - 1].SineStart.Re := cos(Freq);
    FOutputChannel[Index - 1].SineStart.Im := sin(Freq);
    FOutputChannel[Index - 1].SineFrequency := Value;
  end;
end;

procedure InitializeLibrary;
begin
{$IFDEF DebugLog}
  if not Assigned(GLog) then
    GLog := TStringList.Create;
  LogMessage('InitializeLibrary');
{$ENDIF}
  PMUpdSamplePos.Msg := PM_UpdateSamplePos;
  PMBufSwitch.Msg := PM_BufferSwitch;
  PMBufSwitchTimeInfo.Msg := PM_BufferSwitchTimeInfo;
  if not Assigned(GHost) then
    GHost := TLabviewASIO.Create;
end;

procedure FinalizeLibrary;
begin
{$IFDEF DebugLog}
  LogMessage('FinalizeLibrary');
{$ENDIF}
  FreeAndNil(GHost);
{$IFDEF DebugLog}
  FreeAndNil(GLog);
{$ENDIF}
end;

initialization
  InitializeLibrary;

finalization
  FinalizeLibrary;

end.
