// BeRoASIO - A ASIO interface wrapper for Delphi & FreePascal
// Copyright (C) 2005-2006, Benjamin Rosseaux ( http://bero.0ok.de/ )
unit DAV_BeRoASIO;

{$I ASIOVST.inc}

interface

{$IFDEF WIN32}
uses
  Windows, ActiveX, DAV_ASIO;

type
  IBeRoASIO = interface(IUnknown)
    function Init(SysHandle: HWND): TASIOError; stdcall;
    procedure GetDriverName(Name: PCHAR); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString: PCHAR); stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumoutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource;out NumSources: LongInt): TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): Hresult; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples;out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
  end;

  TBeRoASIO = class(TInterfacedObject, IBeRoASIO)
  private
    ASIODriverInterface: IBeRoASIO;
  public
    constructor Create(AsioCLSID: TClsID; var Okay: Boolean);
    destructor Destroy; override;
    function Init(SysHandle: HWND): TASIOError; stdcall;
    procedure GetDriverName(Name: PCHAR); stdcall;
    function GetDriverVersion: LongInt; stdcall;
    procedure GetErrorMessage(ErrorString: PCHAR); stdcall;
    function Start: TASIOError; stdcall;
    function Stop: TASIOError; stdcall;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; stdcall;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; stdcall;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; stdcall;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; stdcall;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; stdcall;
    function SetClockSource(Reference: LongInt): Hresult; stdcall;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; stdcall;
    function GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; stdcall;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; stdcall;
    function DisposeBuffers: TASIOError; stdcall;
    function ControlPanel: TASIOError; stdcall;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; stdcall;
    function OutputReady: TASIOError; stdcall;
  end;

function CreateBeRoASIO(const AsioCLSID: TClsId;var ASIODriver: IBeRoASIO): Boolean; overload;
function CreateBeRoASIO(const AsioCLSID: TClsId;var ASIODriver: TBeRoASIO): Boolean; overload;
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

constructor TBeRoASIO.Create(AsioCLSID: TClsID; var Okay: Boolean);
begin
 inherited Create;
 CoInitialize(nil);
 CoCreateInstance(AsioCLSID, nil, CLSCTX_INPROC_SERVER, AsioCLSID, ASIODriverInterface);
 Okay := assigned(ASIODriverInterface);
end;

destructor TBeRoASIO.Destroy;
begin
 if assigned(ASIODriverInterface) then ASIODriverInterface := nil;
 CoUninitialize;
 inherited Destroy;
end;

function TBeRoASIO.Init(SysHandle: HWND): TASIOError; assembler;
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

procedure TBeRoASIO.GetDriverName(Name: PCHAR); assembler;
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

function TBeRoASIO.GetDriverVersion: LongInt; assembler;
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

procedure TBeRoASIO.GetErrorMessage(ErrorString: PCHAR); assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR ErrorString
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetErrorMessage]
end;
{$ELSE}
asm
 PUSH DWORD PTR ErrorString
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetErrorMessage]
end;
{$ENDIF}

function TBeRoASIO.Start: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStart]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStart]
end;
{$ENDIF}

function TBeRoASIO.Stop: TASIOError; assembler;
{$IFDEF FPC}
asm
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStop]
end;
{$ELSE}
asm
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baStop]
end;
{$ENDIF}

function TBeRoASIO.GetChannels(out NumInputChannels, NumoutputChannels: LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR NumoutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX, SELF
 MOV ECX, DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX, DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannels]
end;
{$ELSE}
asm
 PUSH DWORD PTR NumoutputChannels
 PUSH DWORD PTR NumInputChannels
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetChannels]
end;
{$ENDIF}

function TBeRoASIO.GetLatencies(out InputLatency, outputLatency:LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetLatencies]
end;
{$ELSE}
asm
 PUSH DWORD PTR outputLatency
 PUSH DWORD PTR InputLatency
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetLatencies]
end;
{$ENDIF}

function TBeRoASIO.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR Granularity
 PUSH DWORD PTR PreferredSize
 PUSH DWORD PTR MaxSize
 PUSH DWORD PTR MinSize
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetBufferSize]
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
{$ENDIF}

function TBeRoASIO.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCanSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baCanSampleRate]
end;
{$ENDIF}

function TBeRoASIO.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR SampleRate
 MOV ECX,SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetSampleRate]
end;
{$ENDIF}

function TBeRoASIO.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetSampleRate]
end;
{$ELSE}
asm
 PUSH DWORD PTR [SampleRate + 4]
 PUSH DWORD PTR SampleRate
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baSetSampleRate]
end;
{$ENDIF}

function TBeRoASIO.GetClockSources(Clocks: PASIOClockSource;
  out NumSources: LongInt): TASIOError; assembler;
{$IFDEF FPC}
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR SELF
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetClockSources]
end;
{$ELSE}
asm
 PUSH DWORD PTR NumSources
 PUSH DWORD PTR Clocks
 MOV ECX,DWORD PTR [SELF]
 MOV ECX,DWORD PTR [ECX + ASIODriverInterface]
 MOV EAX,DWORD PTR [ECX]
 CALL DWORD PTR [EAX + baGetClockSources]
end;
{$ENDIF}

function TBeRoASIO.SetClockSource(Reference: LongInt): Hresult; assembler;
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

function TBeRoASIO.GetSamplePosition(out SamplePosition: TASIOSamples;
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

function TBeRoASIO.GetChannelInfo(out Info: TASIOChannelInfo): TASIOError; assembler;
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

function TBeRoASIO.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels,
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

function TBeRoASIO.DisposeBuffers: TASIOError; assembler;
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

function TBeRoASIO.ControlPanel: TASIOError; assembler;
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

function TBeRoASIO.Future(Selector: LongInt; Opt: Pointer): TASIOError; assembler;
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

function TBeRoASIO.OutputReady: TASIOError; assembler;
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

function CreateBeRoASIO(const AsioCLSID: TClsId; var ASIODriver: IBeRoASIO): Boolean; overload;
var
  BeRoASIO: TBeRoASIO;
begin
 try
  BeRoASIO := TBeRoASIO.Create(AsioCLSID, result);
  if result
   then ASIODriver := BeRoASIO
   else ASIODriver := nil;
  result := assigned(ASIODriver);
 except
  result := False;
 end;
end;

function CreateBeRoASIO(const AsioCLSID: TClsId; var ASIODriver: TBeRoASIO): Boolean; overload;
begin
 try
  ASIODriver := TBeRoASIO.Create(AsioCLSID, result);
  if not result then
   begin
    ASIODriver.Destroy;
    ASIODriver := nil;
   end;
  result := assigned(ASIODriver);
 except
  result := False;
 end;
end;

initialization

finalization
{$ENDIF}

end.
