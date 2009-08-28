unit DAV_AsioInterface;

// Several different ASIO interface wrappers are contained in this unit
// The IStdCallAsio has been written and contributed as IBEROASIO interface
// as an ASIO interface wrapper for Delphi & FreePascal by Benjamin Rosseaux
// see http://bero.0ok.de/ Copyright (C) 2005-2006,
// The IStdCallAsio is basically identical to this (except for the name).
//
// Furthermore an IDelphiAsio interface has been developed to simplify the
// interface for an ASIO driver written in Delphi

interface

{$I ..\DAV_Compiler.inc}

{$IFDEF WIN32}
uses
  Windows, ActiveX, DAV_ASIO;

type
  IStdCallAsio = interface(IUnknown)
    // never ever change the order of the functions!!!
    function Init(SysHandle: HWND): TASIOBool; stdcall;
    procedure GetDriverName(Name: PAnsiChar); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): TASIOError; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
  end;

  IDelphiAsio = interface(IUnknown)
    // never ever change the order of the functions!!!
    function Init(SysHandle: HWND): TASIOBool;
    procedure GetDriverName(Name: PAnsiChar);
    function GetDriverVersion: LongInt;
    procedure GetErrorMessage(ErrorString: PAnsiChar);
    function Start: TASIOError;
    function Stop: TASIOError;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
    function SetClockSource(Reference: LongInt): TASIOError;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError;
    function ControlPanel: TASIOError;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError;
    function OutputReady: TASIOError; 
  end;

  TStdCallAsio = class(TInterfacedObject, IStdCallAsio)
  private
    ASIODriverInterface: IStdCallAsio;
  public
    constructor Create(AsioCLSID: TClsID; var Okay: Boolean);
    destructor Destroy; override;
    function Init(SysHandle: HWND): TASIOBool; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; stdcall;
    function GetDriverVersion: LongInt; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    procedure GetDriverName(Name: PAnsiChar); stdcall;
    procedure GetErrorMessage(ErrorString: PAnsiChar); stdcall;
  end;

function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: IStdCallAsio): Boolean; overload;
function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: TStdCallAsio): Boolean; overload;
{$ENDIF}

IMPLEMENTATION

{$IFDEF WIN32}
const
  baQueryInterface    = 0;
  baAddRef            = 1;
  baRelease           = 2;
  baInit              = 12;
  baGetDriverName     = 16;
  baGetDriverVersion  = 20;
  baGetErrorMessage   = 24;
  baStart             = 28;
  baStop              = 32;
  baGetChannels       = 36;
  baGetLatencies      = 40;
  baGetBufferSize     = 44;
  baCanSampleRate     = 48;
  baGetSampleRate     = 52;
  baSetSampleRate     = 56;
  baGetClockSources   = 60;
  baSetClockSource    = 64;
  baGetSamplePosition = 68;
  baGetChannelInfo    = 72;
  baCreateBuffers     = 76;
  baDisposeBuffers    = 80;
  baControlPanel      = 84;
  baFuture            = 88;
  baOutputReady       = 92;

constructor TStdCallAsio.Create(AsioCLSID: TClsID; var Okay: Boolean);
begin
 inherited Create;
 CoInitialize(nil);
 CoCreateInstance(AsioCLSID, nil, CLSCTX_INPROC_SERVER, AsioCLSID, ASIODriverInterface);
 Okay := assigned(ASIODriverInterface);
end;

destructor TStdCallAsio.Destroy;
begin
 if assigned(ASIODriverInterface) then ASIODriverInterface := nil;
 CoUninitialize;
 inherited Destroy;
end;

function TStdCallAsio.Init(SysHandle: HWND): TASIOBool; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR SysHandle
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baInit]
end;
{$ELSE}
asm
 PUSH DWORD PTR SysHandle
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baInit]
end;
{$ENDIF}

procedure TStdCallAsio.GetDriverName(Name: PAnsiChar); assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Name
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetDriverName]
end;
{$ELSE}
asm
 PUSH DWORD PTR Name
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetDriverName]
end;
{$ENDIF}

function TStdCallAsio.GetDriverVersion: LongInt; assembler;
{$IFDEF FPC}
asm
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetDriverVersion]
end;
{$ELSE}
asm
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetDriverVersion]
end;
{$ENDIF}

procedure TStdCallAsio.GetErrorMessage(ErrorString: PAnsiChar); assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR ErrorString
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetErrorMessage]
end;
{$ELSE}
asm
 PUSH DWORD PTR ErrorString
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetErrorMessage]
end;
{$ENDIF}

function TStdCallAsio.Start: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStart]
end;
{$ELSE}
asm
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStart]
end;
{$ENDIF}

function TStdCallAsio.Stop: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStop]
end;
{$ELSE}
asm
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStop]
end;
{$ENDIF}

function TStdCallAsio.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR NumOutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannels]
end;
{$ELSE}
asm
 PUSH DWORD PTR NumOutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannels]
end;
{$ENDIF}

function TStdCallAsio.GetLatencies(out InputLatency, OutputLatency:LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetLatencies]
end;
{$ELSE}
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetLatencies]
end;
{$ENDIF}

function TStdCallAsio.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetBufferSize]
end;
{$ELSE}
asm
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetBufferSize]
end;
{$ENDIF}

function TStdCallAsio.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCanSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCanSampleRate]
end;
{$ENDIF}

function TStdCallAsio.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR SampleRate
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR SampleRate
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSampleRate]
end;
{$ENDIF}

function TStdCallAsio.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX, DWORD PTR SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetSampleRate]
end;
{$ENDIF}

function TStdCallAsio.GetClockSources(Clocks: PASIOClockSource;
  out NumSources: LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX, DWORD PTR SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetClockSources]
end;
{$ELSE}
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetClockSources]
end;
{$ENDIF}

function TStdCallAsio.SetClockSource(Reference: LongInt): TAsioError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Reference
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetClockSource]
end;
{$ELSE}
asm
 PUSH DWORD PTR Reference
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetClockSource]
end;
{$ENDIF}

function TStdCallAsio.GetSamplePosition(out SamplePosition: TASIOSamples;
  out TimeStamp: TASIOTimeStamp): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSamplePosition]
end;
{$ELSE}
asm
 PUSH DWORD PTR TimeStamp
 PUSH DWORD PTR SamplePosition
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSamplePosition]
end;
{$ENDIF}

function TStdCallAsio.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannelInfo]
end;
{$ELSE}
asm
 PUSH DWORD PTR Info
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannelInfo]
end;
{$ENDIF}

function TStdCallAsio.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels,
  BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCreateBuffers]
end;
{$ELSE}
asm
 PUSH DWORD PTR Callbacks
 PUSH DWORD PTR BufferSize
 PUSH DWORD PTR NumChannels
 PUSH DWORD PTR BufferInfos
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCreateBuffers]
end;
{$ENDIF}

function TStdCallAsio.DisposeBuffers: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baDisposeBuffers]
end;
{$ELSE}
asm
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baDisposeBuffers]
end;
{$ENDIF}

function TStdCallAsio.ControlPanel: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baControlPanel]
end;
{$ELSE}
asm
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baControlPanel]
end;
{$ENDIF}

function TStdCallAsio.Future(Selector: LongInt; Opt: Pointer): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baFuture]
end;
{$ELSE}
asm
 PUSH DWORD PTR Opt
 PUSH DWORD PTR Selector
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baFuture]
end;
{$ENDIF}

function TStdCallAsio.OutputReady: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX, DWORD PTR SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baOutputReady]
end;
{$ELSE}
asm
 MOV ECX, DWORD PTR [SELF]
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baOutputReady]
end;
{$ENDIF}

function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: IStdCallAsio): Boolean; overload;
var
  StdCallASIO: TStdCallAsio;
begin
 try
  StdCallASIO := TStdCallAsio.Create(AsioCLSID, Result);
  if Result
   then ASIODriver := StdCallASIO
   else ASIODriver := nil;
  Result := Assigned(ASIODriver);
 except
  Result := False;
 end;
end;

function CreateStdCallASIO(const AsioCLSID: TClsId; var ASIODriver: TStdCallAsio): Boolean; overload;
begin
 try
  ASIODriver := TStdCallAsio.Create(AsioCLSID, Result);
  if not Result then
   begin
    ASIODriver.Destroy;
    ASIODriver := nil;
   end;
  Result := Assigned(ASIODriver);
 except
  Result := False;
 end;
end;

initialization

finalization
{$ENDIF}

end.
