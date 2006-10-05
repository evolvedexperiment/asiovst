// BeRoASIO - A ASIO interface wrapper for Delphi & FreePascal
// Copyright (C) 2005, Benjamin Rosseaux ( http://bero.0ok.de/ )
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
ASM
 PUSH DWORD PTR SysHandle
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baInit]
END;

PROCEDURE TBeRoASIO.GetDriverName(Name:PCHAR); ASSEMBLER;
ASM
 PUSH DWORD PTR Name
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverName]
END;

FUNCTION TBeRoASIO.GetDriverVersion:LONGINT; ASSEMBLER;
ASM
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetDriverVersion]
END;

PROCEDURE TBeRoASIO.GetErrorMessage(ErrorString:PCHAR); ASSEMBLER;
ASM
 PUSH DWORD PTR ErrorString
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetErrorMessage]
END;

FUNCTION TBeRoASIO.Start:TASIOError; ASSEMBLER;
ASM
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStart]
END;

FUNCTION TBeRoASIO.Stop:TASIOError; ASSEMBLER;
ASM
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baStop]
END;

FUNCTION TBeRoASIO.GetChannels(OUT NumInputChannels,NumOutputChannels:LONGINT):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR NumOutputChannels
 PUSH DWORD PTR NumInputChannels
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannels]
END;

FUNCTION TBeRoASIO.GetLatencies(OUT InputLatency,OutputLatency:LONGINT):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR OutputLatency
 PUSH DWORD PTR InputLatency
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetLatencies]
END;

FUNCTION TBeRoASIO.GetBufferSize(OUT MinSize,MaxSize,PreferredSize,Granularity:LONGINT):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetBufferSize]
END;

FUNCTION TBeRoASIO.CanSampleRate(SampleRate:TASIOSampleRate):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCanSampleRate]
END;

FUNCTION TBeRoASIO.GetSampleRate(OUT SampleRate:TASIOSampleRate):TASIOError;
ASM
 PUSH DWORD PTR SampleRate
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSampleRate]
END;

FUNCTION TBeRoASIO.SetSampleRate(SampleRate:TASIOSampleRate):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR [SampleRate+4]
 PUSH DWORD PTR SampleRate
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetSampleRate]
END;

FUNCTION TBeRoASIO.GetClockSources(Clocks:PASIOClockSource;OUT NumSources:LONGINT):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetClockSources]
END;

FUNCTION TBeRoASIO.SetClockSource(Reference:LONGINT):HResult; ASSEMBLER;
ASM
 PUSH DWORD PTR Reference
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baSetClockSource]
END;

FUNCTION TBeRoASIO.GetSamplePosition(OUT SamplePosition:TASIOSamples;OUT TimeStamp:TASIOTimeStamp):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetSamplePosition]
END;

FUNCTION TBeRoASIO.GetChannelInfo(OUT Info:TASIOChannelInfo):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR Info
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baGetChannelInfo]
END;

FUNCTION TBeRoASIO.CreateBuffers(BufferInfos:PASIOBufferInfo;NumChannels,BufferSize:LONGINT;CONST Callbacks:TASIOCallbacks):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baCreateBuffers]
END;

FUNCTION TBeRoASIO.DisposeBuffers:TASIOError; ASSEMBLER;
ASM
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baDisposeBuffers]
END;

FUNCTION TBeRoASIO.ControlPanel:TASIOError; ASSEMBLER;
ASM
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baControlPanel]
END;

FUNCTION TBeRoASIO.Future(Selector:LONGINT;Opt:POINTER):TASIOError; ASSEMBLER;
ASM
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baFuture]
END;

FUNCTION TBeRoASIO.OutputReady:TASIOError; ASSEMBLER;
ASM
 {$IFDEF FPC}
 MOV ECX,DWORD PTR [EAX]
 {$ELSE}
 MOV ECX,DWORD PTR [SELF]
 {$ENDIF}
 MOV ECX,DWORD PTR [ECX+ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX+baOutputReady]
END;

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


