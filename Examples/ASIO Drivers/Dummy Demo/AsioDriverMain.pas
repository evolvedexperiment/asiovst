unit AsioDriverMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, ComObj, DAV_Common, DAV_ASIO, DAV_BeroAsio;

{$DEFINE TESTWAVES}

const
  CBlockFrames = 256;
  CNumInputs = 16;
  CNumOutputs = 16;
  CClass_AsioDriver: TGUID = '{A8DD45FD-34CC-4996-9695-CDD2AE483B47}';

type
  TContextAsioDriverFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  IDavAsio = interface(IBeroASIO)
  ['{A8DD45FD-34CC-4996-9695-CDD2AE483B47}']
  end;

  TAsioDriver = class;

  TAsioProcessingThread = class(TThread)
  private
    FDriver : TAsioDriver;
  protected
    procedure Execute; override;
  end;

  TAsioDriver = class(TComObject, IDavAsio)
  private
    FSamplePosition : Double;
    FSampleRate     : Double;
    FCallbacks      : PASIOCallbacks;
    FAsioTime       : TASIOTime;
    FSystemTime     : TASIOTimeStamp;
    FInMap          : array of LongInt;
    FOutMap         : array of LongInt;
    FBlockFrames    : LongInt;
    FInputLatency   : LongInt;
    FOutputLatency  : LongInt;
    FActiveInputs   : LongInt;
    FActiveOutputs  : LongInt;
    FToggle         : LongInt;
    FMilliSeconds   : LongInt;
    FActive         : Boolean;
    FStarted        : Boolean;
    FTimeCodeRead   : Boolean;
    FTimeInfoMode   : Boolean;
    FDriverVersion  : Integer;
    FSystemHandle   : HWND;
    FAsioProcessing : TAsioProcessingThread;
    FErrorMessage   : array [0..127] of Char;
    FInputBuffers   : TDAVArrayOfSingleFixedArray;
    FOutputBuffers  : TDAVArrayOfSingleFixedArray;

    {$IFDEF TESTWAVES}
    FSineWave       : PDAVSingleFixedArray;
    FSawTooth       : PDAVSingleFixedArray;
    {$ENDIF}

    function InputOpen: Boolean;
    procedure InputClose;
    procedure Input;

    function OutputOpen: Boolean;
    procedure OutputClose;
    procedure Output;

    procedure TimerOn;
    procedure TimerOff;
    procedure BufferSwitchX;
    function SetInternalClockSource(Reference: Integer): TASIOError;
    {$IFDEF TESTWAVES}
    procedure MakeSine(const Data: PDAVSingleFixedArray);
    procedure MakeSaw(const Data: PDAVSingleFixedArray);
    {$ENDIF}
  protected
    procedure BufferSwitch;

    function GetInternalBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError;
    function GetInternalChannelInfo(var Info: TASIOChannelInfo): TASIOError;
    function GetInternalChannels(out NumInputChannels, NumOutputChannels: Integer): TASIOError;
    function GetInternalClockSources(Clocks: PASIOClockSource; out NumSources: Integer): TASIOError;
    function GetInternalDriverVersion: LongInt;
    function GetInternalLatencies(out InputLatency, OutputLatency: Integer): TASIOError;
    function GetInternalSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
    function GetInternalSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
    function InternalControlPanel: TASIOError;
    function InternalCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks): TASIOError;
    function InternalDisposeBuffers: TASIOError;
    function InternalFuture(Selector: LongInt; Opt: Pointer): TASIOError;
    function InternalInit(SysHandle: HWND): TASIOBool;
    function InternalOutputReady: TASIOError;
    function InternalStart: TASIOError;
    function InternalStop: TASIOError;
    function SetInternalSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    procedure GetInternalDriverName(Name: PAnsiChar);
    procedure GetInternalErrorMessage(ErrorString: PAnsiChar);
  public
    destructor Destroy; override;

    procedure Initialize; override;
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
    function GetMilliSeconds: LongInt;
  end;

// extern
procedure GetNanoSeconds(var Time: TASIOTimeStamp);

// local
function AsioSamples2Double(const Samples: TASIOSamples): Double;

implementation

uses
  ComServ, MMSystem, Registry;

const
  CTwoRaisedTo32 : Double = 4294967296;
  CTwoRaisedTo32Reciprocal : Double = 1 / 4294967296;

{$IFDEF TESTWAVES}
  CStupidOffset = $194; // = InstanceSize
{$ELSE}
  CStupidOffset = $18C; // = InstanceSize
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////

function AsioSamples2Double(const Samples: TASIOSamples): Double;
begin
 Result := Samples.Lo;
 if Samples.Hi <> 0
  then Result := Result + Samples.Hi * CTwoRaisedTo32;
end;

procedure GetNanoSeconds(var Time: TASIOTimeStamp);
var
  NanoSeconds : Double;
begin
 NanoSeconds := 0; // (double)((unsigned long)timeGetTime ()) * 1000000.;
 Time.Hi := round(NanoSeconds / CTwoRaisedTo32);
 Time.Lo := round(NanoSeconds - (Time.Hi * CTwoRaisedTo32));
end;


////////////////////////////////////////////////////////////////////////////////

{ TContextAsioDriverFactory }

procedure TContextAsioDriverFactory.UpdateRegistry(Register: Boolean);
begin
 inherited UpdateRegistry(Register);

 // stdcall our global context menu handler
 if Register then
  begin
   CreateRegKey('CLSID\' + GUIDToString(ClassID) + '\' + ComServer.ServerKey, 'ThreadingModel', 'Apartment');
//   ComServer.
   CreateRegKey('SOFTWARE\ASIO\' + Description, 'CLSID', GUIDToString(ClassID), HKEY_LOCAL_MACHINE);
   CreateRegKey('SOFTWARE\ASIO\' + Description, 'Description', Description, HKEY_LOCAL_MACHINE);
  end
 else DeleteRegKey('SOFTWARE\ASIO\' + Description, HKEY_LOCAL_MACHINE);
end;

////////////////////////////////////////////////////////////////////////////////

{ TAsioProcessingThread }

procedure TAsioProcessingThread.Execute;
begin
 while not Terminated do
  begin
   if assigned(FDriver) then
    with TAsioDriver(FDriver) do
     begin
      BufferSwitch;
      Sleep(round(1000 * CBlockFrames / FSampleRate));
     end
   else Sleep(round(1000 * CBlockFrames / 44100));
  end;
end;

////////////////////////////////////////////////////////////////////////////////

{ TAsioDriver }

procedure TAsioDriver.Initialize;
var
  Channel : Integer;
begin
 inherited;
 Assert(CStupidOffset = InstanceSize - 4);
 FBlockFrames    := CBlockFrames;
 FInputLatency   := FBlockFrames;    // typically
 FOutputLatency  := FBlockFrames * 2;

 // typically blockFrames * 2; try to get 1 by offering direct buffer
 // access, and using asioPostOutput for lower latency
 FSamplePosition := 0;
 FSampleRate     := 44100;
 FMilliSeconds   := Round((CBlockFrames * 1000) / FSampleRate);
 FActive         := False;
 FStarted        := False;
 FTimeInfoMode   := False;
 FTimeCodeRead   := False;
 FDriverVersion  := 1;

 // input channels
 SetLength(FInputBuffers, CNumInputs);
 SetLength(FInMap, CNumInputs);
 for Channel := 0 to CNumInputs - 1 do
  begin
   FInputBuffers[Channel] := nil;
   FInMap[Channel] := 0;
  end;

 {$IFDEF TESTWAVES}
 FSawTooth := nil;
 FSineWave := nil;
 {$ENDIF}

 // output channels
 SetLength(FOutputBuffers, CNumOutputs);
 SetLength(FOutMap, CNumOutputs);
 for Channel := 0 to CNumOutputs - 1 do
  begin
   FOutputBuffers[Channel] := nil;
   FOutMap[Channel] := 0;
  end;

 FCallbacks := nil;
 FActiveInputs := 0;
 FActiveOutputs := 0;
 FToggle := 0;
end;

////////////////////////////////////////////////////////////////////////////////

destructor TAsioDriver.Destroy;
begin
 InternalStop;
 OutputClose;
 InputClose;
 InternalDisposeBuffers;
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.Init(SysHandle: HWND): TASIOBool;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL InternalInit
end;

function TAsioDriver.InternalInit(SysHandle: HWND): TASIOBool;
begin
 Result := 1;
 FSystemHandle := SysHandle;
 if FActive then Exit;

 StrCopy(FErrorMessage, 'ASIO Driver open Failure not ');
 if InputOpen and OutputOpen
  then FActive := True
  else
   begin
    // de-activate 'hardware'
    TimerOff;

    OutputClose;
    InputClose;
    Result := 0;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioDriver.GetDriverName(Name: PAnsiChar);
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalDriverName
end;

procedure TAsioDriver.GetInternalDriverName(Name: PAnsiChar);
begin
 StrCopy(Name, 'DAV ASIO Driver');
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetDriverVersion: LongInt;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX

 // stupid
 SUB EAX, CStupidOffset
 ADD [EBP + $8], CStupidOffset

 CALL GetInternalDriverVersion
end;

function TAsioDriver.GetInternalDriverVersion: LongInt;
begin
 Result := FDriverVersion;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioDriver.GetErrorMessage(ErrorString: PAnsiChar);
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalErrorMessage
end;

procedure TAsioDriver.GetInternalErrorMessage(ErrorString: PAnsiChar);
begin
 StrCopy(ErrorString, FErrorMessage);
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.Start: TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX

 // stupid
 SUB EAX, CStupidOffset
 ADD [EBP + $8], CStupidOffset

 CALL InternalStart
end;

function TAsioDriver.InternalStart: TASIOError;
begin
 if assigned(FCallbacks) then
  begin
   FStarted := False;
   FSamplePosition := 0;
   FSystemTime.Lo := 0;
   FSystemTime.Hi := 0;
   FToggle := 0;

   // activate 'hardware'
   TimerOn;
   FStarted := True;

   Result := ASE_OK;
  end
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.Stop: TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX

 // stupid
 SUB EAX, CStupidOffset
 ADD [EBP + $8], CStupidOffset

 CALL InternalStop
end;

function TAsioDriver.InternalStop: TASIOError;
begin
 FStarted := False;
 TimerOff;    // de-activate 'hardware'
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioDriver.TimerOn;
begin
 if Assigned(FAsioProcessing)
  then TimerOff;

 FAsioProcessing := TAsioProcessingThread.Create(True);
 FAsioProcessing.FDriver := Self;
 FAsioProcessing.Resume;
end;

procedure TAsioDriver.TimerOff;
begin
 FStarted := False;
 if assigned(FAsioProcessing) then
  begin
   with FAsioProcessing do
    begin
     if Suspended then Resume;
     Terminate;
     WaitFor;
    end;
   FreeAndNil(FAsioProcessing);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalChannels
end;

function TAsioDriver.GetInternalChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
begin
 NumInputChannels := CNumInputs;
 NumOutputChannels := CNumOutputs;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalLatencies
end;

function TAsioDriver.GetInternalLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
begin
 InputLatency := FInputLatency;
 OutputLatency := FOutputLatency;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetMilliSeconds: LongInt;
begin
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetBufferSize(out MinSize, MaxSize, PreferredSize,
  Granularity: LongInt): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]
 PUSH [EBP + $10]
 PUSH [EBP + $14]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalBufferSize
end;

function TAsioDriver.GetInternalBufferSize(out MinSize, MaxSize, PreferredSize,
  Granularity: Integer): TASIOError;
begin
 // allow one fixed size only
 MinSize := FBlockFrames;
 MaxSize := FBlockFrames;
 PreferredSize := FBlockFrames;
 Granularity := 0;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 // allow these rates only
 if (SampleRate = 44100) or (SampleRate = 48000)
  then Result := ASE_OK
  else Result := ASE_NoClock;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX
 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalSampleRate
end;

function TAsioDriver.GetInternalSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
 SampleRate := FSampleRate;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX
 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL SetInternalSampleRate
end;

function TAsioDriver.SetInternalSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 // allow these rates only
 if (SampleRate <> 44100) and (SampleRate <> 48000)
  then Result := ASE_NoClock
  else
   begin
    if (SampleRate <> FSampleRate) then
    begin
      FSampleRate := SampleRate;
      FAsioTime.TimeInfo.SampleRate := SampleRate;
      FAsioTime.TimeInfo.Flags := FAsioTime.TimeInfo.Flags or kSampleRateChanged;
      FMilliSeconds := round((CBlockFrames * 1000) / FSampleRate);
      if assigned(FCallbacks) and assigned(FCallbacks^.SampleRateDidChange)
       then FCallbacks^.SampleRateDidChange(FSampleRate);
     end;
    Result := ASE_OK;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalClockSources
end;

function TAsioDriver.GetInternalClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
begin
 // internal
 Clocks^.Index := 0;
 Clocks^.AssociatedChannel := -1;
 Clocks^.AssociatedGroup := -1;
 Clocks^.IsCurrentSource := ASIOTrue;
 StrCopy(Clocks^.Name, 'Internal');
 numSources := 1;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.SetClockSource(Reference: LongInt): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX
 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL SetInternalClockSource
end;

function TAsioDriver.SetInternalClockSource(Reference: LongInt): TASIOError;
begin
 if Reference = 0 then
  begin
   FAsioTime.TimeInfo.Flags:= FAsioTime.TimeInfo.Flags or kClockSourceChanged;
   Result := ASE_OK;
  end
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalSamplePosition
end;

function TAsioDriver.GetInternalSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
 TimeStamp.Lo := FSystemTime.Lo;
 TimeStamp.Hi := FSystemTime.Hi;
 if (FSamplePosition >= CTwoRaisedTo32) then 
  begin
   SamplePosition.Hi := round(FSamplePosition * CTwoRaisedTo32Reciprocal);
   SamplePosition.Lo := round(FSamplePosition - (SamplePosition.Hi * CTwoRaisedTo32));
  end
 else
  begin
   SamplePosition.Hi := 0;
   SamplePosition.Lo := round(FSamplePosition);
  end;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL GetInternalChannelInfo
end;

function TAsioDriver.GetInternalChannelInfo(var Info: TASIOChannelInfo): TASIOError;
var
  i : Integer;
begin
 if (Info.Channel < 0) then
  begin
   Result := ASE_InvalidParameter;
   Exit;
  end else
 if Info.IsInput <> 0 then
  if Info.Channel >= CNumInputs then
   begin
    Result := ASE_InvalidParameter;
    Exit;
   end else else
  if Info.Channel >= CNumOutputs then
   begin
    Result := ASE_InvalidParameter;
    Exit;
   end;

 Info.SampleType := ASIOSTFloat32LSB;

 Info.ChannelGroup := 0;
 Info.IsActive := ASIOFalse;

 if Info.IsInput <> 0 then
  begin
   for i := 0 to FActiveInputs - 1 do
    begin
     if FInMap[i] = Info.Channel then
      begin
       Info.IsActive := ASIOTrue;
       Break;
      end;
    end;
  end
 else
  begin
   for i := 0 to FActiveOutputs - 1 do
    begin
     if (FOutMap[i] = Info.Channel) then
      begin
       Info.IsActive := ASIOTrue;
       Break;
      end;
    end;
  end;
 StrPCopy(Info.Name, 'Channel ' + IntToStr(Info.Channel + 1));
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels,
  BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]
 PUSH [EBP + $10]
 PUSH [EBP + $14]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL InternalCreateBuffers
end;

function TAsioDriver.InternalCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels,
  BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
var
  BufferInfo   : PASIOBufferInfo;
  Channel      : Integer;
  NotEnoughMem : Boolean;
begin
 BufferInfo := BufferInfos;
 NotEnoughMem := False;

 FActiveInputs := 0;
 FActiveOutputs := 0;
 FBlockFrames := BufferSize;
 for Channel := 0 to numChannels - 1 do
  begin
   if BufferInfo^.IsInput <> 0 then
    begin
     if (BufferInfo^.ChannelNum < 0) or (BufferInfo^.ChannelNum >= CNumInputs) then
      begin
       InternalDisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
     FInMap[FActiveInputs] := BufferInfo^.ChannelNum;

     // double buffer
     GetMem(FInputBuffers[FActiveInputs], 2 * FBlockFrames * SizeOf(Single));
     if assigned(FInputBuffers[FActiveInputs]) then
      begin
       BufferInfo^.Buffers[0] := @FInputBuffers[FActiveInputs]^[0];
       BufferInfo^.Buffers[1] := @FInputBuffers[FActiveInputs]^[FBlockFrames];
      end
     else
      begin
       BufferInfo^.Buffers[0] := nil;
       BufferInfo^.Buffers[1] := nil;
       NotEnoughMem := True;
      end;

     FActiveInputs:= FActiveInputs + 1;
     if (FActiveInputs > CNumInputs) then
      begin 
       InternalDisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
    end
   else  // output
    begin
     if (BufferInfo^.ChannelNum < 0) or (BufferInfo^.ChannelNum >= CNumOutputs) then
      begin
       InternalDisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
     FOutMap[FActiveOutputs] := BufferInfo^.ChannelNum;

     // double buffer
     GetMem(FOutputBuffers[FActiveOutputs], 2 * FBlockFrames * SizeOf(Single));
     if assigned(FOutputBuffers[FActiveOutputs]) then
      begin
       BufferInfo^.Buffers[0] := @FOutputBuffers[FActiveOutputs]^[0];
       BufferInfo^.Buffers[1] := @FOutputBuffers[FActiveOutputs]^[FBlockFrames];
      end
     else
      begin
       BufferInfo^.Buffers[0] := nil;
       BufferInfo^.Buffers[1] := nil;
       NotEnoughMem := True;
      end;
     FActiveOutputs:= FActiveOutputs + 1;
     if (FActiveOutputs > CNumOutputs) then
      begin
       FActiveOutputs := FActiveOutputs - 1;
       InternalDisposeBuffers;
       Result := ASE_InvalidParameter;
       Exit;
      end;
    end;

   Inc(BufferInfo);
  end;

 if NotEnoughMem then
  begin
   InternalDisposeBuffers;
   Result := ASE_NoMemory;
   Exit;
  end;

 FCallbacks := @Callbacks;
 if (Callbacks.AsioMessage(kAsioSupportsTimeInfo, 0, nil, nil)) <> 0 then
  begin
   FTimeInfoMode := True;
   with FAsioTime.TimeInfo do
    begin
     Speed := 1.;
     SystemTime.Hi := 0;
     SystemTime.Lo := 0;
     SamplePosition.Hi := 0;
     SamplePosition.Lo := 0;
     SampleRate := FSampleRate;
     Flags := kSystemTimeValid or kSamplePositionValid or kSampleRateValid;
    end;

   with FAsioTime.TimeCode do
    begin
    Speed := 1.;
    TimeCodeSamples.Lo := 0;
    TimeCodeSamples.Hi := 0;
    Flags := kTcValid or kTcRunning;
   end;
  end
 else FTimeInfoMode := False;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.DisposeBuffers: TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX

 // stupid
 SUB EAX, CStupidOffset
 ADD [EBP + $8], CStupidOffset

 CALL InternalDisposeBuffers
end;

function TAsioDriver.InternalDisposeBuffers: TASIOError;
var
  Channel : Integer;
begin
 FCallbacks := nil;
 InternalStop;

 for Channel := 0 to FActiveInputs - 1 do Dispose(FInputBuffers[Channel]);
 SetLength(FInputBuffers, 0);
 FActiveInputs := 0;

 for Channel := 0 to FActiveOutputs - 1 do Dispose(FOutputBuffers[Channel]);
 SetLength(FOutputBuffers, 0);
 FActiveOutputs := 0;

 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.ControlPanel: TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX

 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX

 // stupid
 SUB EAX, CStupidOffset
 ADD [EBP + $8], CStupidOffset

 CALL InternalControlPanel
end;

function TAsioDriver.InternalControlPanel: TASIOError;
begin
 Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.Future(Selector: Integer; Opt: Pointer): TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX
 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX
 MOV EDX, [EBP + $8]
 MOV ECX, [EBP + $C]

 // stupid
 SUB EAX, CStupidOffset
 ADD EDX, CStupidOffset

 CALL InternalFuture
end;

function TAsioDriver.InternalFuture(Selector: Integer;
  Opt: Pointer): TASIOError;
begin
 Result := ASE_SUCCESS;
 case Selector of
   kAsioEnableTimeCodeRead : FTimeCodeRead := True;
  kAsioDisableTimeCodeRead : FTimeCodeRead := False;
      kAsioSetInputMonitor : Result := ASE_SUCCESS;  // for testing!!!
      kAsioCanInputMonitor : Result := ASE_SUCCESS;  // for testing!!!
          kAsioCanTimeInfo : Result := ASE_SUCCESS;
          kAsioCanTimeCode : Result := ASE_SUCCESS;
  else Result := ASE_NotPresent;
 end;
end;

////////////////////////////////////////////////////////////////////////////////
// private methods
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
// input
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
function TAsioDriver.InputOpen: Boolean;
begin 
 {$IFDEF TESTWAVES}
 GetMem(FSineWave, FBlockFrames * SizeOf(Single));
 if not Assigned(FSineWave) then
  begin
   StrCopy(FErrorMessage, 'ASIO Driver: Out of Memory');
   Result := False;
   Exit;
  end;
 MakeSine(FSineWave);

 GetMem(FSawTooth, FBlockFrames * SizeOf(Single));
 if not Assigned(FSawTooth) then
  begin
   StrCopy(FErrorMessage, 'ASIO Driver: Out of Memory');
   Result := False;
   Exit;
  end;
 MakeSaw(FSawTooth);
 {$ENDIF}
 Result := True;
end;

{$IFDEF TESTWAVES}
////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.MakeSine(const Data: PDAVSingleFixedArray);
var
  Freq   : Double;
  Sample : Integer;
begin
 Freq := 2 * Pi / FBlockFrames;

 for Sample := 0 to FBlockFrames - 1
  do Data[Sample] := sin(Freq * Sample);
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.MakeSaw(const Data: PDAVSingleFixedArray);
var
  Freq   : Double;
  Sample : Integer;
begin
 Freq := 2 * Pi / FBlockFrames;

 for Sample := 0 to FBlockFrames - 1
  do Data[Sample] := 1 - Freq * Sample;
end;
{$ENDIF}

////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.InputClose;
begin
 {$IFDEF TESTWAVES}
 if Assigned(FSineWave)
  then Dispose(FSineWave);
 FSineWave := nil;

 if Assigned(FSawTooth)
  then Dispose(FSawTooth);
 FSawTooth := nil;
 {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.Input;
{$IFDEF TESTWAVES}
var
  Channel : Integer;
  Offset  : Integer;
  Data    : PDavSingleFixedArray;
{$ENDIF}
begin
 {$IFDEF TESTWAVES}
 for Channel := 0 to FActiveInputs - 1 do
  begin
   Data := @FInputBuffers[Channel]^[0];
   if assigned(Data) then
    begin
     if FToggle = 0
      then Offset := 0
      else Offset := FBlockFrames;

     if Channel mod 2 = 0
      then Move(FSawTooth[0], Data[Offset], Round(FBlockFrames * SizeOf(Single)))
      else Move(FSineWave[0], Data[Offset], Round(FBlockFrames * SizeOf(Single)));
    end;
  end;
 {$ENDIF}
end;

////////////////////////////////////////////////////////////////////////////////
// Output
////////////////////////////////////////////////////////////////////////////////

////////////////////////////////////////////////////////////////////////////////
function TAsioDriver.OutputOpen: Boolean;
begin
 Result := True;
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.OutputClose;
begin
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.Output;
begin
end;

////////////////////////////////////////////////////////////////////////////////
procedure TAsioDriver.BufferSwitch;
begin
 if FStarted and assigned(FCallbacks) then
  begin
   GetNanoSeconds(FSystemTime);      // latch system time
   Input;
   Output;
   FSamplePosition := FSamplePosition + FBlockFrames;
   if FTimeInfoMode
    then BufferSwitchX
    else FCallbacks^.BufferSwitch(FToggle, ASIOFalse);
   FToggle := 1 - FToggle;
  end;
end;

////////////////////////////////////////////////////////////////////////////////
// asio2 buffer switch
procedure TAsioDriver.BufferSwitchX;
var
  Offset : Integer;
begin
 with FAsioTime, TimeInfo do
  begin
   GetInternalSamplePosition(SamplePosition, SystemTime);
   if FToggle = 0
    then Offset := 0
    else Offset := FBlockFrames;

   if FTimeCodeRead then
    with FAsioTime.TimeCode do
     begin
      // Create a fake time code, which is 10 minutes ahead of the card's sample position
      // Please note that for simplicity here time code will wrap after 32 bit are reached
      TimeCodeSamples.Lo := SamplePosition.Lo + Round(600.0 * FSampleRate);
      TimeCodeSamples.Hi := 0;
     end;
   FCallbacks^.BufferSwitchTimeInfo(FAsioTime, FToggle, ASIOFalse);
   Flags := Flags and not (kSampleRateChanged or kClockSourceChanged);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioDriver.OutputReady: TASIOError;
asm
 // shift/store saved ESP
 MOV EAX, [ESP]
 MOV [ESP - 4], EAX

 // shift/store saved EIP (return adress)
 MOV EAX, [ESP + 4]
 MOV [ESP], EAX
 // shift stack pointer to get a valid return address on return
 SUB ESP, 4

 // pass variables
 MOV EAX, ECX

 // stupid
 SUB EAX, CStupidOffset
 ADD [EBP + $8], CStupidOffset

 CALL InternalOutputReady
end;

function TAsioDriver.InternalOutputReady: TASIOError;
begin
 Result := ASE_NotPresent;
end;

initialization
  { Note that we create an instance of TContextMenuFactory here rather
    than TComObjectFactory. This is necessary so that we can add some
    custom registry entries by overriding the UpdateRegistry virtual
    function. }
  TContextAsioDriverFactory.Create(ComServer, TAsioDriver, CClass_AsioDriver,
    'AsioDriver', 'DAV ASIO Driver', ciSingleInstance, tmApartment);

end.
