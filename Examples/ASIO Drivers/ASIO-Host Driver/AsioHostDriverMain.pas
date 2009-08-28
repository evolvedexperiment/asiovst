unit AsioHostDriverMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, Forms, ComObj, DAV_Common, DAV_ASIO,
  DAV_BeroAsio, DAV_AsioHost;

const
  CBlockFrames = 256;
  CNumInputs = 2;
  CNumOutputs = 2;
  CClass_AsioHostDriver: TGUID = '{8F45801A-8D74-4179-9F66-ADD2C3CD4C70}';
  CDriverDescription = 'DAV ASIO-Host Driver';

type
  TContextAsioHostDriverFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

  IAsioHostDriver = interface(IBeroASIO)
  ['{8F45801A-8D74-4179-9F66-ADD2C3CD4C70}']
  end;

  TAsioHostDriver = class(TComObject, IAsioHostDriver)
  private
    FSamplePosition : Double;
    FCallbacks      : PASIOCallbacks;
    FAsioTime       : TASIOTime;
    FSystemTime     : TASIOTimeStamp;
    FInMap          : array of LongInt;
    FOutMap         : array of LongInt;
    FBlockFrames    : LongInt;
    FActiveInputs   : LongInt;
    FActiveOutputs  : LongInt;
    FToggle         : LongInt;
    FActive         : Boolean;
    FStarted        : Boolean;
    FTimeCodeRead   : Boolean;
    FTimeInfoMode   : Boolean;
    FDriverVersion  : Integer;
    FSystemHandle   : HWND;
    FControlPanel   : TForm;
    FAsioHost       : TAsioHost;
    FErrorMessage   : array [0..127] of Char;
    FInputBuffers   : TDAVArrayOfSingleFixedArray;
    FOutputBuffers  : TDAVArrayOfSingleFixedArray;

    function InternalOpen: Boolean;
    procedure InternalClose;

    procedure TimerOn;
    procedure TimerOff;
    procedure BufferSwitchX;
    function SetInternalClockSource(Reference: Integer): TASIOError;
    procedure BufferSwitch32EventHandler(Sender: TObject; const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
  protected
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
    function InternalStart: TASIOError;
    function InternalStop: TASIOError;
    function InternalOutputReady: TASIOError;
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

    property AsioHost: TAsioHost read FAsioHost;
  end;

// extern
procedure GetNanoSeconds(var Time: TASIOTimeStamp);

// local
function AsioSamples2Double(const Samples: TASIOSamples): Double;

implementation

uses
  Math, ComServ, Registry, AsioHostDriverControlPanel;

const
  CTwoRaisedTo32 : Double = 4294967296;
  CTwoRaisedTo32Reciprocal : Double = 1 / 4294967296;
  CStupidOffset = $17C; // = InstanceSize

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

{ TContextAsioHostDriverFactory }

procedure TContextAsioHostDriverFactory.UpdateRegistry(Register: Boolean);
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

{ TAsioHostDriver }

procedure TAsioHostDriver.Initialize;
var
  Channel : Integer;
begin
 inherited;
 Assert(CStupidOffset = InstanceSize - 4);

 // typically blockFrames * 2; try to get 1 by offering direct buffer
 // access, and using asioPostOutput for lower latency
 FSamplePosition := 0;
 FActive         := False;
 FStarted        := False;
 FTimeInfoMode   := False;
 FTimeCodeRead         := False;
 FDriverVersion  := 1;

 // input channels
 SetLength(FInputBuffers, CNumInputs);
 SetLength(FInMap, CNumInputs);
 for Channel := 0 to CNumInputs - 1 do
  begin
   FInputBuffers[Channel] := nil;
   FInMap[Channel] := 0;
  end;

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

 FAsioHost := TAsioHost.Create(nil);
 with FAsioHost do
  begin
   OnBufferSwitch32 := BufferSwitch32EventHandler;
   DriverIndex := AsioHost.DriverList.IndexOf('ASIO4ALL v2');
   FBlockFrames := FAsioHost.BufferSize;
  end;
end;

////////////////////////////////////////////////////////////////////////////////

destructor TAsioHostDriver.Destroy;
begin
 InternalStop;
 InternalClose;
 InternalDisposeBuffers;
 if assigned(FControlPanel)
  then FreeAndNil(FControlPanel);
 FreeAndNil(FAsioHost); 
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Init(SysHandle: HWND): TASIOBool;
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

function TAsioHostDriver.InternalInit(SysHandle: HWND): TASIOBool;
begin
 Result := 1;
 FSystemHandle := SysHandle;
 if FActive then Exit;

 StrCopy(FErrorMessage, 'ASIO Driver open Failure not ');
 if InternalOpen
  then FActive := True
  else
   begin
    // de-activate 'hardware'
    TimerOff;

    InternalClose;
    Result := 0;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetDriverName(Name: PAnsiChar);
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

procedure TAsioHostDriver.GetInternalDriverName(Name: PAnsiChar);
begin
 StrCopy(Name, 'Asio-Host ASIO Driver');
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetDriverVersion: LongInt;
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

function TAsioHostDriver.GetInternalDriverVersion: LongInt;
begin
 Result := FDriverVersion;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetErrorMessage(ErrorString: PAnsiChar);
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

procedure TAsioHostDriver.GetInternalErrorMessage(ErrorString: PAnsiChar);
begin
 StrCopy(ErrorString, FErrorMessage);
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Start: TASIOError;
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

function TAsioHostDriver.InternalStart: TASIOError;
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

function TAsioHostDriver.Stop: TASIOError;
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

function TAsioHostDriver.InternalStop: TASIOError;
begin
 FStarted := False;
 TimerOff;    // de-activate 'hardware'
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.TimerOn;
begin
 FAsioHost.Active := True;
end;

procedure TAsioHostDriver.TimerOff;
begin
 FAsioHost.Active := False;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
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

function TAsioHostDriver.GetInternalChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
begin
 NumInputChannels := CNumInputs;
 NumOutputChannels := CNumOutputs;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
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

function TAsioHostDriver.GetInternalLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
begin
 InputLatency := FAsioHost.InputLatency;
 OutputLatency := FAsioHost.OutputLatency;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetMilliSeconds: LongInt;
begin
 Result := 0;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetBufferSize(out MinSize, MaxSize, PreferredSize,
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

function TAsioHostDriver.GetInternalBufferSize(out MinSize, MaxSize, PreferredSize,
  Granularity: Integer): TASIOError;
begin
 // allow one fixed size only
 if FAsioHost.DriverIndex >= 0 then
  begin
   MinSize := FAsioHost.BufferMinimum;
   MaxSize := FAsioHost.BufferMaximum;
   PreferredSize := FAsioHost.BufferPreferredSize;
   Granularity := FAsioHost.BufferGranularity;
   Result := ASE_OK;
  end
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 if FAsioHost.DriverIndex >= 0
  then Result := FAsioHost.CanSampleRate(SampleRate)
  else Result := ASE_NoClock;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
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

function TAsioHostDriver.GetInternalSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
 SampleRate := FAsioHost.SampleRate;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
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

function TAsioHostDriver.SetInternalSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 if FAsioHost.DriverIndex < 0
  then Result := ASE_NoClock
  else
   begin
    Result := FAsioHost.CanSampleRate(SampleRate);

    if Result = ASE_OK then
     if (SampleRate <> FAsioHost.SampleRate) then
      begin
       FAsioHost.SampleRate := SampleRate;
       FAsioTime.TimeInfo.SampleRate := SampleRate;
       FAsioTime.TimeInfo.Flags:= FAsioTime.TimeInfo.flags or kSampleRateChanged;
       if Assigned(FCallbacks) and assigned(FCallbacks^.SampleRateDidChange)
        then FCallbacks^.SampleRateDidChange(FAsioHost.SampleRate);
      end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
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

function TAsioHostDriver.GetInternalClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
begin
 with Clocks^ do
  begin
   Index := 0;
   AssociatedChannel := -1;
   AssociatedGroup := -1;
   IsCurrentSource := ASIOTrue;
   StrCopy(Name, 'Internal');
  end;
 NumSources := 1;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.SetClockSource(Reference: LongInt): TASIOError;
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

function TAsioHostDriver.SetInternalClockSource(Reference: LongInt): TASIOError;
begin
 if Reference = 0 then
  begin
   FAsioTime.TimeInfo.Flags:= FAsioTime.TimeInfo.Flags or kClockSourceChanged;
   Result := ASE_OK;
  end
 else Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
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

function TAsioHostDriver.GetInternalSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
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
   SamplePosition.Lo := Round(FSamplePosition);
  end;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
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

function TAsioHostDriver.GetInternalChannelInfo(var Info: TASIOChannelInfo): TASIOError;
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

function TAsioHostDriver.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels,
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

function TAsioHostDriver.InternalCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels,
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
  with FAsioTime do
   begin
    FTimeInfoMode := True;
    with TimeInfo do
     begin
      Speed := 1.;
      SystemTime.Hi := 0;
      SystemTime.Lo := 0;
      SamplePosition.Hi := 0;
      SamplePosition.Lo := 0;
      SampleRate := FAsioHost.SampleRate;
      Flags := kSystemTimeValid or kSamplePositionValid or kSampleRateValid;
     end;

    with TimeCode do
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

function TAsioHostDriver.DisposeBuffers: TASIOError;
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

function TAsioHostDriver.InternalDisposeBuffers: TASIOError;
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

function TAsioHostDriver.ControlPanel: TASIOError;
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

function TAsioHostDriver.InternalControlPanel: TASIOError;
begin
 if assigned(FControlPanel) then
  begin
   FControlPanel.Show;
   FControlPanel.BringToFront;
  end
 else
  begin
   FControlPanel := TFmAsioDriverControlPanel.Create(FAsioHost);
//   FControlPanel.ParentWindow := FSystemHandle;
   FControlPanel.Show;
  end;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.Future(Selector: Integer; Opt: Pointer): TASIOError;
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

function TAsioHostDriver.InternalFuture(Selector: Integer;
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

function TAsioHostDriver.InternalOpen: Boolean;
begin
 Result := True;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.InternalClose;
begin
end;

////////////////////////////////////////////////////////////////////////////////
// asio2 buffer switch
////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.BufferSwitchX;
var
  Offset : Integer;
begin
 with FAsioTime, TimeInfo, TimeCode do
  begin
   GetInternalSamplePosition(SamplePosition, SystemTime);
   if FToggle = 0
    then Offset := 0
    else Offset := FBlockFrames;

   if FTimeCodeRead then
    begin
     TimeCodeSamples.Lo := SamplePosition.Lo;
     TimeCodeSamples.Hi := SamplePosition.Hi;
    end;
   FCallbacks^.BufferSwitchTimeInfo(FAsioTime, FToggle, ASIOFalse);
   Flags := Flags and  not (kSampleRateChanged or kClockSourceChanged);
  end;
end;

////////////////////////////////////////////////////////////////////////////////

function TAsioHostDriver.OutputReady: TASIOError;
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

function TAsioHostDriver.InternalOutputReady: TASIOError;
begin
 Result := ASE_NotPresent;
end;

procedure TAsioHostDriver.BufferSwitch32EventHandler(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Offset  : Integer;
begin
 if FStarted and assigned(FCallbacks) then
  begin
   GetNanoSeconds(FSystemTime);      // latch system time

   if FToggle = 0
    then Offset := 0
    else Offset := FBlockFrames;

   for Channel := 0 to Min(Length(FInputBuffers), AsioHost.InputChannelCount) - 1
    do Move(InBuffer[Channel]^[0], FInputBuffers[Channel]^[Offset], AsioHost.BufferSize * SizeOf(Single));

   for Channel := 0 to Min(Length(FOutputBuffers), AsioHost.OutputChannelCount) - 1
    do Move(FOutputBuffers[Channel]^[Offset], OutBuffer[Channel]^[0], AsioHost.BufferSize * SizeOf(Single));

   FSamplePosition := FSamplePosition + FBlockFrames;
   if FTimeInfoMode
    then BufferSwitchX
    else FCallbacks^.BufferSwitch(FToggle, ASIOFalse);
   FToggle := 1 - FToggle;
  end;
end;

initialization
  { Note that we create an instance of TContextMenuFactory here rather
    than TComObjectFactory. This is necessary so that we can add some
    custom registry entries by overriding the UpdateRegistry virtual
    function. }
  TContextAsioHostDriverFactory.Create(ComServer, TAsioHostDriver, CClass_AsioHostDriver,
    'AsioHostDriver', CDriverDescription, ciSingleInstance, tmApartment);

end.
