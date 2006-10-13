// BeRoASIO - A ASIO interface wrapper for Delphi & FreePascal
// Copyright (C) 2005-2006, Benjamin Rosseaux ( http://bero.0ok.de/ )
UNIT BeRoASIO;
{$IFDEF FPC}
 {$MODE DELPHI}
 {$WARNINGS OFF}
 {$HINTS OFF}
 {$OVERFLOWCHECKS OFF}
 {$RANGECHECKS OFF}
 {$IFDEF CPUI386}
  {$DEFINE CPU386}
  {$ASMMODE INTEL}
 {$ENDIF}
 {$IFDEF FPC_LITTLE_ENDIAN}
  {$DEFINE LITTLE_ENDIAN}
 {$ELSE}
  {$IFDEF FPC_BIG_ENDIAN}
   {$DEFINE BIG_ENDIAN}
  {$ENDIF}
 {$ENDIF}
{$ELSE}
 {$DEFINE LITTLE_ENDIAN}
 {$IFNDEF CPU64}
  {$DEFINE CPU32}
 {$ENDIF}
 {$OPTIMIZATION ON}
{$ENDIF}
  
INTERFACE

{$IFDEF WIN32}
USES Windows,ActiveX,ASIO;

TYPE IBeRoASIO=INTERFACE(IUnknown)
      FUNCTION Init(SysHandle:HWND):TASIOBool; STDCALL;
      PROCEDURE GetDriverName(Name:PCHAR); STDCALL;
      FUNCTION GetDriverVersion:LONGINT; STDCALL;
      PROCEDURE GetErrorMessage(ErrorString:PCHAR); STDCALL;
      FUNCTION Start:TASIOError; STDCALL;
      FUNCTION Stop:TASIOError; STDCALL;
      FUNCTION GetChannels(OUT NumInputChannels,NumOutputChannels:LONGINT):TASIOError; STDCALL;
      FUNCTION GetLatencies(OUT InputLatency,OutputLatency:LONGINT):TASIOError; STDCALL;
      FUNCTION GetBufferSize(OUT MinSize,MaxSize,PreferredSize,Granularity:LONGINT):TASIOError; STDCALL;
      FUNCTION CanSampleRate(SampleRate:TASIOSampleRate):TASIOError; STDCALL;
      FUNCTION GetSampleRate(OUT SampleRate:TASIOSampleRate):TASIOError; STDCALL;
      FUNCTION SetSampleRate(SampleRate:TASIOSampleRate):TASIOError; STDCALL;
      FUNCTION GetClockSources(Clocks:PASIOClockSource;OUT NumSources:LONGINT):TASIOError; STDCALL;
      FUNCTION SetClockSource(Reference:LONGINT):HResult; STDCALL;
      FUNCTION GetSamplePosition(OUT SamplePosition:TASIOSamples;OUT TimeStamp:TASIOTimeStamp):TASIOError; STDCALL;
      FUNCTION GetChannelInfo(OUT Info:TASIOChannelInfo):TASIOError; STDCALL;
      FUNCTION CreateBuffers(BufferInfos:PASIOBufferInfo;NumChannels,BufferSize:LONGINT;CONST Callbacks:TASIOCallbacks):TASIOError; STDCALL;
      FUNCTION DisposeBuffers:TASIOError; STDCALL;
      FUNCTION ControlPanel:TASIOError; STDCALL;
      FUNCTION Future(Selector:LONGINT;Opt:POINTER):TASIOError; STDCALL;
      FUNCTION OutputReady:TASIOError; STDCALL;
     END;

     TBeRoASIO=CLASS(TInterfacedObject,IBeRoASIO)
      PRIVATE
       ASIODriverInterface:IBeRoASIO;
      PUBLIC
       CONSTRUCTOR Create(AsioCLSID:TClsID;VAR Okay:BOOLEAN);
       DESTRUCTOR Destroy; OVERRIDE;
       FUNCTION Init(SysHandle:HWND):TASIOBool; STDCALL;
       PROCEDURE GetDriverName(Name:PCHAR); STDCALL;
       FUNCTION GetDriverVersion:LONGINT; STDCALL;
       PROCEDURE GetErrorMessage(ErrorString:PCHAR); STDCALL;
       FUNCTION Start:TASIOError; STDCALL;
       FUNCTION Stop:TASIOError; STDCALL;
       FUNCTION GetChannels(OUT NumInputChannels,NumOutputChannels:LONGINT):TASIOError; STDCALL;
       FUNCTION GetLatencies(OUT InputLatency,OutputLatency:LONGINT):TASIOError; STDCALL;
       FUNCTION GetBufferSize(OUT MinSize,MaxSize,PreferredSize,Granularity:LONGINT):TASIOError; STDCALL;
       FUNCTION CanSampleRate(SampleRate:TASIOSampleRate):TASIOError; STDCALL;
       FUNCTION GetSampleRate(OUT SampleRate:TASIOSampleRate):TASIOError; STDCALL;
       FUNCTION SetSampleRate(SampleRate:TASIOSampleRate):TASIOError; STDCALL;
       FUNCTION GetClockSources(Clocks:PASIOClockSource;OUT NumSources:LONGINT):TASIOError; STDCALL;
       FUNCTION SetClockSource(Reference:LONGINT):HResult; STDCALL;
       FUNCTION GetSamplePosition(OUT SamplePosition:TASIOSamples;OUT TimeStamp:TASIOTimeStamp):TASIOError; STDCALL;
       FUNCTION GetChannelInfo(OUT Info:TASIOChannelInfo):TASIOError; STDCALL;
       FUNCTION CreateBuffers(BufferInfos:PASIOBufferInfo;NumChannels,BufferSize:LONGINT;CONST Callbacks:TASIOCallbacks):TASIOError; STDCALL;
       FUNCTION DisposeBuffers:TASIOError; STDCALL;
       FUNCTION ControlPanel:TASIOError; STDCALL;
       FUNCTION Future(Selector:LONGINT;Opt:POINTER):TASIOError; STDCALL;
       FUNCTION OutputReady:TASIOError; STDCALL;
     END;

FUNCTION CreateBeRoASIO(CONST AsioCLSID:TClsId;VAR ASIODriver:IBeRoASIO):BOOLEAN; OVERLOAD;
FUNCTION CreateBeRoASIO(CONST AsioCLSID:TClsId;VAR ASIODriver:TBeRoASIO):BOOLEAN; OVERLOAD;
{$ENDIF}

IMPLEMENTATION

{$IFDEF WIN32}
CONST baQueryInterface=0;
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
      baOutputReady=92;

CONSTRUCTOR TBeRoASIO.Create(AsioCLSID:TClsID;VAR Okay:BOOLEAN);
BEGIN
 INHERITED Create;
 CoInitialize(NIL);
 CoCreateInstance(AsioCLSID,NIL,CLSCTX_INPROC_SERVER,AsioCLSID,ASIODriverInterface);
 Okay:=ASSIGNED(ASIODriverInterface);
END;

DESTRUCTOR TBeRoASIO.Destroy;
BEGIN
 IF ASSIGNED(ASIODriverInterface) THEN ASIODriverInterface:=NIL;
 CoUninitialize;
 INHERITED Destroy;
END;

FUNCTION TBeRoASIO.Init(SysHandle:HWND):TASIOBool; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR SysHandle
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baInit]
END;
{$ELSE}
ASM
 PUSH DWORD PTR SysHandle
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baInit]
END;
{$ENDIF}

PROCEDURE TBeRoASIO.GetDriverName(Name:PCHAR); ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR Name
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverName]
END;
{$ELSE}
ASM
 PUSH DWORD PTR Name
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverName]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetDriverVersion:LONGINT; ASSEMBLER;
{$IFDEF FPC}
ASM
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverVersion]
END;
{$ELSE}
ASM
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverVersion]
END;
{$ENDIF}

PROCEDURE TBeRoASIO.GetErrorMessage(ErrorString:PCHAR); ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR ErrorString
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetErrorMessage]
END;
{$ELSE}
ASM
 PUSH DWORD PTR ErrorString
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetErrorMessage]
END;
{$ENDIF}

FUNCTION TBeRoASIO.Start:TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStart]
END;
{$ELSE}
ASM
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStart]
END;
{$ENDIF}

FUNCTION TBeRoASIO.Stop:TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStop]
END;
{$ELSE}
ASM
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStop]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetChannels(OUT NumInputChannels,NumOutputChannels:LONGINT):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR NumOutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannels]
END;
{$ELSE}
ASM
 PUSH DWORD PTR NumOutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannels]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetLatencies(OUT InputLatency,OutputLatency:LONGINT):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR OutputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetLatencies]
END;
{$ELSE}
ASM
 PUSH DWORD PTR OutputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetLatencies]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetBufferSize(OUT MinSize,MaxSize,PreferredSize,Granularity:LONGINT):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetBufferSize]
END;
{$ELSE}
ASM
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetBufferSize]
END;
{$ENDIF}

FUNCTION TBeRoASIO.CanSampleRate(SampleRate:TASIOSampleRate):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCanSampleRate]
END;
{$ELSE}
ASM
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCanSampleRate]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetSampleRate(OUT SampleRate:TASIOSampleRate):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR SampleRate
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSampleRate]
END;
{$ELSE}
ASM
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSampleRate]
END;
{$ENDIF}

FUNCTION TBeRoASIO.SetSampleRate(SampleRate:TASIOSampleRate):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetSampleRate]
END;
{$ELSE}
ASM
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetSampleRate]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetClockSources(Clocks:PASIOClockSource;OUT NumSources:LONGINT):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetClockSources]
END;
{$ELSE}
ASM
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetClockSources]
END;
{$ENDIF}

FUNCTION TBeRoASIO.SetClockSource(Reference:LONGINT):HResult; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR Reference
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetClockSource]
END;
{$ELSE}
ASM
 PUSH DWORD PTR Reference
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetClockSource]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetSamplePosition(OUT SamplePosition:TASIOSamples;OUT TimeStamp:TASIOTimeStamp):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSamplePosition]
END;
{$ELSE}
ASM
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSamplePosition]
END;
{$ENDIF}

FUNCTION TBeRoASIO.GetChannelInfo(OUT Info:TASIOChannelInfo):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannelInfo]
END;
{$ELSE}
ASM
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannelInfo]
END;
{$ENDIF}

FUNCTION TBeRoASIO.CreateBuffers(BufferInfos:PASIOBufferInfo;NumChannels,BufferSize:LONGINT;CONST Callbacks:TASIOCallbacks):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCreateBuffers]
END;
{$ELSE}
ASM
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCreateBuffers]
END;
{$ENDIF}

FUNCTION TBeRoASIO.DisposeBuffers:TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baDisposeBuffers]
END;
{$ELSE}
ASM
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baDisposeBuffers]
END;
{$ENDIF}

FUNCTION TBeRoASIO.ControlPanel:TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baControlPanel]
END;
{$ELSE}
ASM
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baControlPanel]
END;
{$ENDIF}

FUNCTION TBeRoASIO.Future(Selector:LONGINT;Opt:POINTER):TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baFuture]
END;
{$ELSE}
ASM
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baFuture]
END;
{$ENDIF}

FUNCTION TBeRoASIO.OutputReady:TASIOError; ASSEMBLER;
{$IFDEF FPC}
ASM
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baOutputReady]
END;
{$ELSE}
ASM
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baOutputReady]
END;
{$ENDIF}

FUNCTION CreateBeRoASIO(CONST AsioCLSID:TClsId;VAR ASIODriver:IBeRoASIO):BOOLEAN; OVERLOAD;
VAR BeRoASIO:TBeRoASIO;
BEGIN
 BeRoASIO:=TBeRoASIO.Create(AsioCLSID,RESULT);
 IF RESULT THEN BEGIN
  ASIODriver:=BeRoASIO;
 END ELSE BEGIN
  ASIODriver:=NIL;
 END;
 RESULT:=ASSIGNED(ASIODriver);
END;

FUNCTION CreateBeRoASIO(CONST AsioCLSID:TClsId;VAR ASIODriver:TBeRoASIO):BOOLEAN; OVERLOAD;
BEGIN
 ASIODriver:=TBeRoASIO.Create(AsioCLSID,RESULT);
 IF NOT RESULT THEN BEGIN
  ASIODriver.Destroy;
  ASIODriver:=NIL;
 END;
 RESULT:=ASSIGNED(ASIODriver);
END;

INITIALIZATION
FINALIZATION
{$ENDIF}
END.


