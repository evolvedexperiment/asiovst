// BeRoASIO - A ASIO interface wrapper for Delphi & FreePascal
// Copyright (C) 2005-2006, Benjamin Rosseaux ( http://bero.0ok.de/ )
unit BeRoASIO;

{$I ASIOVST.inc}

interface

{$ifDEF WIN32}
uses
  Windows,ActiveX,ASIO;

type
  IBeRoASIO = interface(IUnknown)
    function Init(SysHandle: HWND): TASIOBool; stdcall;
    procedure GetDriverName(Name: PCHAR); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString:PCHAR); stdcall;
    function Start:TASIOError; stdcall;
    function Stop:TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumoutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, outputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity:LONGINT):TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate):TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate):TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource;out NumSources: LongInt):TASIOError; stdcall;
    function SetClockSource(Reference: LONGINT): HResult; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples;out TimeStamp: TASIOTimeStamp):TASIOError; stdcall;
    function GetChannelInfo(out Info: TASIOChannelInfo):TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt;const Callbacks:TASIOCallbacks):TASIOError; stdcall;
    function DisposeBuffers:TASIOError; stdcall;
    function ControlPanel:TASIOError; stdcall;
    function Future(Selector:LongInt; Opt:pointer):TASIOError; stdcall;
    function outputReady:TASIOError; stdcall;
  end;

  TBeRoASIO = class(TInterfacedObject, IBeRoASIO)
  private
    ASIODriverInterface:IBeRoASIO;
  public
    constructor Create(AsioCLSID:TClsID;var Okay:boolean);
    destructor Destroy; override;
    function Init(SysHandle:HWND):TASIOBool; stdcall;
    procedure GetDriverName(Name:PCHAR); stdcall;
    function GetDriverVersion:longint; stdcall;
    procedure GetErrorMessage(ErrorString:PCHAR); stdcall;
    function Start:TASIOError; stdcall;
    function Stop:TASIOError; stdcall;
    function GetChannels(out NumInputChannels,NumoutputChannels:longint):TASIOError; stdcall;
    function GetLatencies(out InputLatency,outputLatency:longint):TASIOError; stdcall;
    function GetBufferSize(out MinSize,MaxSize,PreferredSize,Granularity:longint):TASIOError; stdcall;
    function CanSampleRate(SampleRate:TASIOSampleRate):TASIOError; stdcall;
    function GetSampleRate(out SampleRate:TASIOSampleRate):TASIOError; stdcall;
    function SetSampleRate(SampleRate:TASIOSampleRate):TASIOError; stdcall;
    function GetClockSources(Clocks:PASIOClockSource;out NumSources:longint):TASIOError; stdcall;
    function SetClockSource(Reference:longint):HResult; stdcall;
    function GetSamplePosition(out SamplePosition:TASIOSamples;out TimeStamp:TASIOTimeStamp):TASIOError; stdcall;
    function GetChannelInfo(out Info:TASIOChannelInfo):TASIOError; stdcall;
    function CreateBuffers(BufferInfos:PASIOBufferInfo;NumChannels,BufferSize:longint;const Callbacks:TASIOCallbacks):TASIOError; stdcall;
    function DisposeBuffers:TASIOError; stdcall;
    function ControlPanel:TASIOError; stdcall;
    function Future(Selector:longint;Opt:pointer):TASIOError; stdcall;
    function outputReady:TASIOError; stdcall;
  end;

function CreateBeRoASIO(const AsioCLSID:TClsId;var ASIODriver:IBeRoASIO):boolean; overload;
function CreateBeRoASIO(const AsioCLSID:TClsId;var ASIODriver:TBeRoASIO):boolean; overload;
{$endif}

IMPLEMENTATION

{$ifDEF WIN32}
const baQueryInterface=0;
      baAddRef=1;
      baRelease=2;
      baInit=12;
      baGetDriverName=16;
      baGetDriverVersion=20;
      baGetErrorMessage=24;
      baStart=28;
      baStop=32;
      baGetChannels=36;
      baGetLatencies=40;
      baGetBufferSize=44;
      baCanSampleRate=48;
      baGetSampleRate=52;
      baSetSampleRate=56;
      baGetClockSources=60;
      baSetClockSource=64;
      baGetSamplePosition=68;
      baGetChannelInfo=72;
      baCreateBuffers=76;
      baDisposeBuffers=80;
      baControlPanel=84;
      baFuture=88;
      baoutputReady=92;

constRUCTOR TBeRoASIO.Create(AsioCLSID:TClsID;var Okay:boolean);
begin
 INHERITED Create;
 CoInitialize(NIL);
 CoCreateInstance(AsioCLSID,NIL,CLSCTX_INPROC_SERVER,AsioCLSID,ASIODriverInterface);
 Okay:=ASSIGNED(ASIODriverInterface);
end;

DESTRUCTOR TBeRoASIO.Destroy;
begin
 if ASSIGNED(ASIODriverInterface) then ASIODriverInterface:=NIL;
 CoUninitialize;
 INHERITED Destroy;
end;

function TBeRoASIO.Init(SysHandle:HWND):TASIOBool; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR SysHandle
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baInit]
end;
{$ELSE}
asm
 PUSH DWORD PTR SysHandle
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baInit]
end;
{$endif}

procedure TBeRoASIO.GetDriverName(Name:PCHAR); assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR Name
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverName]
end;
{$ELSE}
asm
 PUSH DWORD PTR Name
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverName]
end;
{$endif}

function TBeRoASIO.GetDriverVersion:longint; assembler;
{$ifDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverVersion]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverVersion]
end;
{$endif}

procedure TBeRoASIO.GetErrorMessage(ErrorString:PCHAR); assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR ErrorString
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetErrorMessage]
end;
{$ELSE}
asm
 PUSH DWORD PTR ErrorString
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetErrorMessage]
end;
{$endif}

function TBeRoASIO.Start:TASIOError; assembler;
{$ifDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStart]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStart]
end;
{$endif}

function TBeRoASIO.Stop:TASIOError; assembler;
{$ifDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStop]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStop]
end;
{$endif}

function TBeRoASIO.GetChannels(out NumInputChannels,NumoutputChannels:longint):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR NumoutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannels]
end;
{$ELSE}
asm
 PUSH DWORD PTR NumoutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannels]
end;
{$endif}

function TBeRoASIO.GetLatencies(out InputLatency,outputLatency:longint):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetLatencies]
end;
{$ELSE}
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetLatencies]
end;
{$endif}

function TBeRoASIO.GetBufferSize(out MinSize,MaxSize,PreferredSize,Granularity:longint):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetBufferSize]
end;
{$ELSE}
asm
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetBufferSize]
end;
{$endif}

function TBeRoASIO.CanSampleRate(SampleRate:TASIOSampleRate):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCanSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCanSampleRate]
end;
{$endif}

function TBeRoASIO.GetSampleRate(out SampleRate:TASIOSampleRate):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR SampleRate
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSampleRate]
end;
{$endif}

function TBeRoASIO.SetSampleRate(SampleRate:TASIOSampleRate):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetSampleRate]
end;
{$endif}

function TBeRoASIO.GetClockSources(Clocks:PASIOClockSource;out NumSources:longint):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetClockSources]
end;
{$ELSE}
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetClockSources]
end;
{$endif}

function TBeRoASIO.SetClockSource(Reference:longint):HResult; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR Reference
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetClockSource]
end;
{$ELSE}
asm
 PUSH DWORD PTR Reference
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetClockSource]
end;
{$endif}

function TBeRoASIO.GetSamplePosition(out SamplePosition:TASIOSamples;out TimeStamp:TASIOTimeStamp):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSamplePosition]
end;
{$ELSE}
asm
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSamplePosition]
end;
{$endif}

function TBeRoASIO.GetChannelInfo(out Info:TASIOChannelInfo):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannelInfo]
end;
{$ELSE}
asm
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannelInfo]
end;
{$endif}

function TBeRoASIO.CreateBuffers(BufferInfos:PASIOBufferInfo;NumChannels,BufferSize:longint;const Callbacks:TASIOCallbacks):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCreateBuffers]
end;
{$ELSE}
asm
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCreateBuffers]
end;
{$endif}

function TBeRoASIO.DisposeBuffers:TASIOError; assembler;
{$ifDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baDisposeBuffers]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baDisposeBuffers]
end;
{$endif}

function TBeRoASIO.ControlPanel:TASIOError; assembler;
{$ifDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baControlPanel]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baControlPanel]
end;
{$endif}

function TBeRoASIO.Future(Selector:longint;Opt:pointer):TASIOError; assembler;
{$ifDEF FPC}
asm
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baFuture]
end;
{$ELSE}
asm
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baFuture]
end;
{$endif}

function TBeRoASIO.OutputReady:TASIOError; assembler;
{$ifDEF FPC}
asm
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baOutputReady]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baOutputReady]
end;
{$endif}

function CreateBeRoASIO(const AsioCLSID:TClsId;var ASIODriver:IBeRoASIO): boolean; overload;
var BeRoASIO:TBeRoASIO;
begin
 BeRoASIO:=TBeRoASIO.Create(AsioCLSID,RESULT);
 if RESULT then begin
  ASIODriver:=BeRoASIO;
 end else begin
  ASIODriver := nil;
 end;
 RESULT:=ASSIGNED(ASIODriver);
end;

function CreateBeRoASIO(const AsioCLSID:TClsId;var ASIODriver:TBeRoASIO): boolean; overload;
begin
 ASIODriver := TBeRoASIO.Create(AsioCLSID, RESULT);
 if not RESULT then begin
  ASIODriver.Destroy;
  ASIODriver := nil;
 end;
 RESULT:=ASSIGNED(ASIODriver);
end;

initialization
finalization
{$endif}
end.


