unit AsioHostDriverMain;

{$I DAV_Compiler.inc}

{$WARN SYMBOL_PLATFORM OFF}

interface

uses
  Windows, SysUtils, Classes, Forms, ComObj, DAV_Common, DAV_ASIO,
  DAV_AsioInterface, DAV_AsioHost;

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

  IAsioHostDriver = interface(IDelphiASIO)
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
    function InternalCanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function InternalControlPanel: TASIOError;
    function InternalCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: Integer; const Callbacks: TASIOCallbacks): TASIOError;
    function InternalDisposeBuffers: TASIOError;
    function InternalFuture(Selector: LongInt; Opt: Pointer): TASIOError;
    function InternalInit(SysHandle: HWND): TASIOBool;
    function InternalStart: TASIOError;
    function InternalStop: TASIOError;
    function InternalOutputReady: TASIOError;
    function SetInternalSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    procedure DriverIndexChangedHandler(Sender: TObject);
    procedure ResetRequestedHandler(Sender: TObject);
    procedure GetInternalDriverName(Name: PAnsiChar);
    procedure GetInternalErrorMessage(ErrorString: PAnsiChar);
  public
    destructor Destroy; override;

    procedure Initialize; override;
   
    procedure Init;
    procedure GetDriverName;
    procedure GetDriverVersion;
    procedure GetErrorMessage;
    procedure Start;
    procedure Stop;
    procedure GetChannels;
    procedure GetLatencies;
    procedure GetBufferSize;
    procedure CanSampleRate;
    procedure GetSampleRate;
    procedure SetSampleRate;
    procedure GetClockSources;
    procedure SetClockSource;
    procedure GetSamplePosition;
    procedure GetChannelInfo;
    procedure CreateBuffers;
    procedure DisposeBuffers;
    procedure ControlPanel;
    procedure Future;
    procedure OutputReady;

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
  CInterfaceOffset = $17C;

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
 Time.Hi := Round(NanoSeconds / CTwoRaisedTo32);
 Time.Lo := Round(NanoSeconds - (Time.Hi * CTwoRaisedTo32));
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
 Assert(CInterfaceOffset = GetInterfaceTable^.Entries[0].IOffset);

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
   OnDriverChanged := DriverIndexChangedHandler;
   OnReset := ResetRequestedHandler;
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
 SetLength(FInputBuffers, 0);
 SetLength(FOutputBuffers, 0);
 SetLength(FInMap, 0);
 SetLength(FOutMap, 0);

 if Assigned(FControlPanel)
  then FreeAndNil(FControlPanel);
 FreeAndNil(FAsioHost);
 inherited;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.Init;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  call InternalInit
end;

function TAsioHostDriver.InternalInit(SysHandle: HWND): TASIOBool;
begin
 Result := 1;
 FSystemHandle := SysHandle;
 if FActive then Exit;

 StrCopy(FErrorMessage, 'ASIO Driver Init Failure');
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

procedure TAsioHostDriver.GetDriverName;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  call GetInternalDriverName
end;

procedure TAsioHostDriver.GetInternalDriverName(Name: PAnsiChar);
begin
 StrCopy(Name, 'Asio-Host ASIO Driver');
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetDriverVersion;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  call GetInternalDriverVersion
end;

function TAsioHostDriver.GetInternalDriverVersion: LongInt;
begin
 Result := FDriverVersion;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetErrorMessage;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  call GetInternalErrorMessage
end;

procedure TAsioHostDriver.GetInternalErrorMessage(ErrorString: PAnsiChar);
begin
 StrCopy(ErrorString, FErrorMessage);
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.Start;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  call InternalStart
end;

function TAsioHostDriver.InternalStart: TASIOError;
begin
 if Assigned(FCallbacks) then
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

procedure TAsioHostDriver.Stop;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  call InternalStop
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

procedure TAsioHostDriver.GetChannels;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  call GetInternalChannels
end;

function TAsioHostDriver.GetInternalChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
begin
 NumInputChannels := CNumInputs;
 NumOutputChannels := CNumOutputs;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetLatencies;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8
  call GetInternalLatencies
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

procedure TAsioHostDriver.GetBufferSize;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8]   // get second parameter

  mov edx, [esp + 16]  // get fourth parameter
  mov [esp + 8], edx   // set fourth parameter

  // move return address on the stack position of the incoming fourth parameter
  mov edx, [esp]
  mov [esp + 16], edx

  mov edx, [esp + 4]   // get first parameter

  // move stack pointer to the new fourth parameter
  add esp, 8
  call GetInternalBufferSize
end;

function TAsioHostDriver.GetInternalBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
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

procedure TAsioHostDriver.CanSampleRate;
asm
  // double uses 2 Words, they come in on the stack,
  // and delphi function calls use the same method
  // so we just move the return address to stack positions up
  // and are done

  mov edx, [esp]   // backup return address

  mov eax, [esp + 4]
  mov [esp], eax
  mov eax, [esp + 8]
  mov [esp + 4], eax

  mov [esp + 8], edx    // set return address

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset

  call InternalCanSampleRate
end;

function TAsioHostDriver.InternalCanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
 if FAsioHost.DriverIndex >= 0
  then Result := FAsioHost.CanSampleRate(SampleRate)
  else Result := ASE_NoClock;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetSampleRate;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset

  // move stack pointer to the return address position
  add esp, 4

  call GetInternalSampleRate
end;

function TAsioHostDriver.GetInternalSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
 SampleRate := FAsioHost.SampleRate;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.SetSampleRate;
asm
  // double uses 2 Words, they come in on the stack,
  // and delphi function calls use the same method
  // so we just move the return address to stack positions up
  // and are done

  mov edx,[esp]   // backup return address

  mov eax,[esp + 4]
  mov [esp], eax
  mov eax,[esp + 8]
  mov [esp+4], eax

  mov [esp+8],edx    // set return address

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset
  call SetInternalSampleRate
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
       with FAsioTime do
        begin
         TimeInfo.SampleRate := SampleRate;
         TimeInfo.Flags:= TimeInfo.flags or kSampleRateChanged;
        end;
       if Assigned(FCallbacks) and Assigned(FCallbacks^.SampleRateDidChange)
        then FCallbacks^.SampleRateDidChange(FAsioHost.SampleRate);
      end;
   end;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.GetClockSources;
asm
 // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  call GetInternalClockSources
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

procedure TAsioHostDriver.SetClockSource;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset    

  // move stack pointer to the return address position
  add esp, 4

  call SetInternalClockSource
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

procedure TAsioHostDriver.GetSamplePosition;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  call GetInternalSamplePosition
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

procedure TAsioHostDriver.GetChannelInfo;
asm
  mov edx, [esp + 4] // get first parameter

  // move return address on the stack position of the incoming parameter
  mov eax, [esp]
  mov [esp + 4], eax

  // generate new "self" pointer for this object in ECX
  mov eax, ecx
  sub eax, CInterfaceOffset    

  // move stack pointer to the return address position
  add esp, 4

  call GetInternalChannelInfo
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

procedure TAsioHostDriver.CreateBuffers;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8]   // get second parameter

  mov edx, [esp + 16]  // get fourth parameter
  mov [esp + 8], edx   // set fourth parameter

  // move return address on the stack position of the incoming fourth parameter
  mov edx, [esp]
  mov [esp + 16], edx

  mov edx, [esp + 4]   // get first parameter

  // move stack pointer to the new fourth parameter
  add esp, 8

  call InternalCreateBuffers
end;

function TAsioHostDriver.InternalCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
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
     if Assigned(FInputBuffers[FActiveInputs]) then
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
     if Assigned(FOutputBuffers[FActiveOutputs]) then
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

procedure TAsioHostDriver.DisposeBuffers;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset
  call InternalDisposeBuffers
end;

function TAsioHostDriver.InternalDisposeBuffers: TASIOError;
var
  Channel : Integer;
begin
 FCallbacks := nil;
 InternalStop;

 for Channel := 0 to FActiveInputs - 1 do
   if assigned(FInputBuffers[Channel]) then
   begin
     Dispose(FInputBuffers[Channel]);
     FInputBuffers[Channel] := nil;
   end;

 FActiveInputs := 0;

 for Channel := 0 to FActiveOutputs - 1 do 
   if assigned(FOutputBuffers[Channel]) then
   begin
     Dispose(FOutputBuffers[Channel]);
     FOutputBuffers[Channel] := nil;
   end;

 FActiveOutputs := 0;

 Result := ASE_OK; 
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.ControlPanel;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  call InternalControlPanel
end;

function TAsioHostDriver.InternalControlPanel: TASIOError;
begin
 if Assigned(FControlPanel) then
  begin
   FControlPanel.Showmodal; // Important, there are Host that push the window back... eg. SynthMaker
  end
 else
  begin
   FControlPanel := TFmAsioDriverControlPanel.Create(FAsioHost);
//   FControlPanel.ParentWindow := FSystemHandle;
   FControlPanel.ShowModal; // Important, there are Host that push the window back... eg. SynthMaker
  end;
 Result := ASE_OK;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.Future;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  mov ecx, [esp + 8] // get second parameter

  // move return address on the stack position of the incoming second parameter
  mov edx, [esp]
  mov [esp + 8], edx

  mov edx, [esp + 4] // get first parameter

  // move stack pointer to the return address position
  add esp, 8

  call InternalFuture
end;

function TAsioHostDriver.InternalFuture(Selector: Integer; Opt: Pointer): TASIOError;
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
begin
 with FAsioTime, TimeInfo, TimeCode do
  begin
   GetInternalSamplePosition(SamplePosition, SystemTime);

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

procedure TAsioHostDriver.OutputReady;
asm
  // generate new "self" pointer for this object
  mov eax, ecx
  sub eax, CInterfaceOffset

  call InternalOutputReady
end;

function TAsioHostDriver.InternalOutputReady: TASIOError;
begin
 Result := ASE_NotPresent;
end;

////////////////////////////////////////////////////////////////////////////////

procedure TAsioHostDriver.DriverIndexChangedHandler(Sender: TObject);
begin
 if Assigned(FCallbacks) then
  if Assigned(FCallbacks.AsioMessage) then
   begin
    FCallbacks.AsioMessage(kAsioResetRequest, 0, nil, nil);
   end;
end;

procedure TAsioHostDriver.ResetRequestedHandler(Sender: TObject);
begin
 if Assigned(FCallbacks) then
  if Assigned(FCallbacks.AsioMessage) then
   begin
    FCallbacks.AsioMessage(kAsioResetRequest, 0, nil, nil);
   end;
end;

////////////////////////////////////////////////////////////////////////////////



procedure TAsioHostDriver.BufferSwitch32EventHandler(Sender: TObject;
  const InBuffer, OutBuffer: TDAVArrayOfSingleFixedArray);
var
  Channel : Integer;
  Offset  : Integer;
begin
 if FStarted and Assigned(FCallbacks) then
  begin
   GetNanoSeconds(FSystemTime);      // latch system time

   if FToggle = 0
    then Offset := 0
    else Offset := FBlockFrames;

   for Channel := 0 to Min(FActiveInputs,AsioHost.InputChannelCount) - 1
    do Move(InBuffer[Channel]^[0], FInputBuffers[Channel]^[Offset], AsioHost.BufferSize * SizeOf(Single));

   for Channel := 0 to Min(FActiveOutputs,AsioHost.OutputChannelCount) - 1
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
    'AsioHostDriver', CDriverDescription, ciMultiInstance, tmApartment);

end.
