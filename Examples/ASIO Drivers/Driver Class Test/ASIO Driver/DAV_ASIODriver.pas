unit DAV_ASIODriver;

interface

uses
  Windows, ActiveX, ComObj, DAV_ASIO;

type
  IDavASIODriverInterface = interface(IUnknown)
    // never ever change the order of the functions!!!
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
  end;

  TDavASIODriver = class;
  TTDavASIODriver = class of TDavASIODriver;

  TDavASIOTCWrapper = class(TComObject)
  private
    FDestinationClass: TDavASIODriver;
  protected
    function GetDriverClass: TTDavASIODriver; virtual; abstract;
  public
    procedure Initialize; override;
    destructor Destroy; override;

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
  end;

  TDavASIODriver = class
  private
    fTCWrapper: TDavASIOTCWrapper;
  public
    constructor create(TCWrapper: TDavASIOTCWrapper);


    function Init(SysHandle: HWND): boolean; virtual;
    function GetDriverName: string; virtual;
    function GetDriverVersion: LongInt; virtual;
    function GetErrorMessage: string; virtual;
    function Start: TASIOError; virtual;
    function Stop: TASIOError; virtual;
    function GetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError; virtual;
    function GetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError; virtual;
    function GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError; virtual;
    function CanSampleRate(SampleRate: TASIOSampleRate): TASIOError; virtual;
    function GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError; virtual;
    function SetSampleRate(SampleRate: TASIOSampleRate): TASIOError; virtual;
    function GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError; virtual;
    function SetClockSource(Reference: LongInt): TASIOError; virtual;
    function GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError; virtual;
    function GetChannelInfo(var Info: TASIOChannelInfo): TASIOError; virtual;
    function CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError; virtual;
    function DisposeBuffers: TASIOError; virtual;
    function ControlPanel: TASIOError; virtual;
    function Future(Selector: LongInt; Opt: Pointer): TASIOError; virtual;
    function OutputReady: TASIOError; virtual;

    function AsioInit(SysHandle: HWND): TASIOBool;
    procedure AsioGetDriverName(Name: PAnsiChar);
    function AsioGetDriverVersion: Longint;  
    procedure AsioGetErrorMessage(Msg: PAnsiChar);  
    function AsioStart: TASIOError;
    function AsioStop: TASIOError;
    function AsioGetChannels(out NumInputChannels, NumOutputChannels: LongInt): TASIOError;
    function AsioGetLatencies(out InputLatency, OutputLatency: LongInt): TASIOError;
    function AsioGetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: LongInt): TASIOError;
    function AsioCanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function AsioGetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
    function AsioSetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
    function AsioGetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
    function AsioSetClockSource(Reference: LongInt): TASIOError;
    function AsioGetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
    function AsioGetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
    function AsioCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
    function AsioDisposeBuffers: TASIOError;
    function AsioControlPanel: TASIOError;
    function AsioFuture(Selector: LongInt; Opt: Pointer): TASIOError;
    function AsioOutputReady: TASIOError;
  end;

  TDavAsioDriverFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

IMPLEMENTATION


uses sysutils;

const DavASIOInterfaceOffset = $24;


{ TDavASIOTCWrapper }

procedure TDavASIOTCWrapper.Initialize;
begin
  inherited;
  Assert(DavASIOInterfaceOffset = GetInterfaceTable^.Entries[0].IOffset);
  FDestinationClass:=GetDriverClass.Create(self);
end;

destructor TDavASIOTCWrapper.Destroy;
begin
  FDestinationClass.Free;
  FDestinationClass:=nil;
  inherited;
end;

procedure TDavASIOTCWrapper.Init;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioInit-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetDriverName;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again
  
  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetDriverName-FDestinationClass
end;  

procedure TDavASIOTCWrapper.GetDriverVersion;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetDriverVersion-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetErrorMessage;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetErrorMessage-FDestinationClass
end;

procedure TDavASIOTCWrapper.Start;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioStart-FDestinationClass
end;

procedure TDavASIOTCWrapper.Stop;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioStop-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetChannels;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  pop ecx   // get 2. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetChannels-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetLatencies;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  pop ecx   // get 2. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetLatencies-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetBufferSize;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter (min)
  pop ecx   // get 2. parameter (max)
  pop edi   // get 3. parameter (pref)
  pop esi   // get 4. parameter (gran)
  push ebx  // push return adress again
  push edi  // push 3. parameter (pref)
  push esi  // push 4. parameter (gran)


  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetBufferSize-FDestinationClass
end;

procedure TDavASIOTCWrapper.CanSampleRate;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioCanSampleRate-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetSampleRate;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetSampleRate-FDestinationClass
end;

procedure TDavASIOTCWrapper.SetSampleRate;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioSetSampleRate-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetClockSources;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  pop ecx   // get 2. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetClockSources-FDestinationClass
end;

procedure TDavASIOTCWrapper.SetClockSource;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioSetClockSource-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetSamplePosition;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  pop ecx   // get 2. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetSamplePosition-FDestinationClass
end;

procedure TDavASIOTCWrapper.GetChannelInfo;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioGetChannelInfo-FDestinationClass
end;

procedure TDavASIOTCWrapper.CreateBuffers;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter (BufferInfos)
  pop ecx   // get 2. parameter (NumChannels)
  pop edi   // get 3. parameter (BufferSize)
  pop esi   // get 4. parameter (Callbacks)
  push ebx  // push return adress again
  push edi  // push 3. parameter (BufferSize)
  push esi  // push 4. parameter (Callbacks)


  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioCreateBuffers-FDestinationClass
end;

procedure TDavASIOTCWrapper.DisposeBuffers;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioDisposeBuffers-FDestinationClass
end;

procedure TDavASIOTCWrapper.ControlPanel;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioControlPanel-FDestinationClass
end;

procedure TDavASIOTCWrapper.Future;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  pop ebx   // pop return address
  pop edx   // get 1. parameter
  pop ecx   // get 2. parameter
  push ebx  // push return adress again

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioFuture-FDestinationClass
end;

procedure TDavASIOTCWrapper.OutputReady;
asm
  // generate new "self" pointer for this object
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset

  // now generate "self" pointer for called class using the "self" pointer of this object
  mov eax,[self.FDestinationClass]
  call FDestinationClass.AsioOutputReady-FDestinationClass
end;


{ TDavAsioDriverFactory }

procedure TDavAsioDriverFactory.UpdateRegistry(Register: Boolean);
begin
 inherited UpdateRegistry(Register);

 if Register then
  begin
   CreateRegKey('CLSID\' + GUIDToString(ClassID) + '\' + ComServer.ServerKey, 'ThreadingModel', 'Apartment');
   CreateRegKey('SOFTWARE\ASIO\' + Description, 'CLSID', GUIDToString(ClassID), HKEY_LOCAL_MACHINE);
   CreateRegKey('SOFTWARE\ASIO\' + Description, 'Description', Description, HKEY_LOCAL_MACHINE);
  end
 else DeleteRegKey('SOFTWARE\ASIO\' + Description, HKEY_LOCAL_MACHINE);
end;



{ TDavASIODriver }

constructor TDavASIODriver.create(TCWrapper: TDavASIOTCWrapper);
begin
  fTCWrapper := TCWrapper;
end;




function TDavASIODriver.Init(SysHandle: HWND): boolean;
begin
  result := true;
end;

function TDavASIODriver.GetDriverName: string;
begin
  result := 'DAV Abstract Driver';
end;

function TDavASIODriver.GetDriverVersion: LongInt;
begin
  result := 0;
end;

function TDavASIODriver.GetErrorMessage: string;
begin
  result := '';
end;

function TDavASIODriver.Start: TASIOError;
begin
  result := ASE_OK;
end;

function TDavASIODriver.Stop: TASIOError;
begin
  result := ASE_OK;
end;

function TDavASIODriver.GetChannels(out NumInputChannels, NumOutputChannels: Integer): TASIOError;
begin
  NumInputChannels  := 1;
  NumOutputChannels := 1;
  result := ASE_OK;
end;

function TDavASIODriver.GetLatencies(out InputLatency, OutputLatency: Integer): TASIOError;
begin
  InputLatency  := 0;
  OutputLatency := 0; 
  result := ASE_OK;
end;

function TDavASIODriver.GetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin
  MinSize       := 256;
  MaxSize       := 1024;
  PreferredSize := 512;
  Granularity   := -1;
  result := ASE_OK;
end;


function TDavASIODriver.CanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  result := ASE_OK;
end;

function TDavASIODriver.GetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
  SampleRate := 44100;
  result := ASE_OK;
end;

function TDavASIODriver.SetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  result := ASE_OK;
end;


function TDavASIODriver.GetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
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
  result := ASE_OK;
end;

function TDavASIODriver.SetClockSource(Reference: LongInt): TASIOError;
begin
  result := ASE_OK;
end;

function TDavASIODriver.GetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  SamplePosition.Hi := 0;
  SamplePosition.Lo := 0;
  TimeStamp.Hi := 0;
  TimeStamp.Lo := 0;
  result := ASE_OK;
end;

function TDavASIODriver.GetChannelInfo(var Info: TASIOChannelInfo): TASIOError;     
begin
  if (Info.Channel <> 0) then
  begin
    Result := ASE_InvalidParameter;
    Exit;
  end;

  Info.SampleType := ASIOSTFloat32LSB;
  Info.ChannelGroup := 0;
  Info.IsActive := ASIOFalse;
  StrPCopy(Info.Name, 'Default channel');

  result := ASE_OK;
end;

function TDavASIODriver.CreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
begin
  result := ASE_NotPresent; // doesn't allocate anything right now
end;


function TDavASIODriver.DisposeBuffers: TASIOError;
begin
  result := ASE_OK;
end;

function TDavASIODriver.ControlPanel: TASIOError;
begin
  result := ASE_OK;
end;

function TDavASIODriver.Future(Selector: LongInt; Opt: Pointer): TASIOError;
begin
  result := ASE_NotPresent;
end;

function TDavASIODriver.OutputReady: TASIOError;
begin
  result := ASE_OK;
end;


// methods used by the Asio-Wrapper, they are forwarded to local
// functions that can get overwritten, don't overwrite this functions
// the wrapper would never call them.

function TDavASIODriver.AsioInit(SysHandle: HWND): TASIOBool;
begin
  result := TASIOBool(Init(SysHandle));
end;

procedure TDavASIODriver.AsioGetDriverName(Name: PAnsiChar);
begin
  strcopy(Name,pchar(GetDriverName));
end;

function TDavASIODriver.AsioGetDriverVersion: Longint;
begin
  result := GetDriverVersion;
end;

procedure TDavASIODriver.AsioGetErrorMessage(Msg: PAnsiChar);
begin
  strcopy(Msg,pchar(GetErrorMessage));
end;

function TDavASIODriver.AsioStart: TASIOError;
begin
  result := Start;
end;

function TDavASIODriver.AsioStop: TASIOError;
begin
  result := Stop;
end;

function TDavASIODriver.AsioGetChannels(out NumInputChannels, NumOutputChannels: Integer): TASIOError;
begin
  result := GetChannels(NumInputChannels, NumOutputChannels);
end;

function TDavASIODriver.AsioGetLatencies(out InputLatency, OutputLatency: Integer): TASIOError;
begin
  result := GetLatencies(InputLatency, OutputLatency);
end;

function TDavASIODriver.AsioGetBufferSize(out MinSize, MaxSize, PreferredSize, Granularity: Integer): TASIOError;
begin
  result := GetBufferSize(MinSize, MaxSize, PreferredSize, Granularity);
end;

function TDavASIODriver.AsioCanSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  result := CanSampleRate(SampleRate);
end;

function TDavASIODriver.AsioGetSampleRate(out SampleRate: TASIOSampleRate): TASIOError;
begin
  result := GetSampleRate(SampleRate);
end;

function TDavASIODriver.AsioSetSampleRate(SampleRate: TASIOSampleRate): TASIOError;
begin
  result := SetSampleRate(SampleRate);
end;

function TDavASIODriver.AsioGetClockSources(Clocks: PASIOClockSource; out NumSources: LongInt): TASIOError;
begin
  result := GetClockSources(Clocks, NumSources);
end;

function TDavASIODriver.AsioSetClockSource(Reference: LongInt): TASIOError;
begin
  result := SetClockSource(Reference);
end;

function TDavASIODriver.AsioGetSamplePosition(out SamplePosition: TASIOSamples; out TimeStamp: TASIOTimeStamp): TASIOError;
begin
  result := GetSamplePosition(SamplePosition, TimeStamp);
end;

function TDavASIODriver.AsioGetChannelInfo(var Info: TASIOChannelInfo): TASIOError;
begin
  result := GetChannelInfo(Info);
end;

function TDavASIODriver.AsioCreateBuffers(BufferInfos: PASIOBufferInfo; NumChannels, BufferSize: LongInt; const Callbacks: TASIOCallbacks): TASIOError;
begin
  result := CreateBuffers(BufferInfos, NumChannels, BufferSize, Callbacks);
end;

function TDavASIODriver.AsioDisposeBuffers: TASIOError;
begin
  result := DisposeBuffers;
end;

function TDavASIODriver.AsioControlPanel: TASIOError;
begin
  result := ControlPanel;
end;

function TDavASIODriver.AsioFuture(Selector: LongInt; Opt: Pointer): TASIOError;
begin
  result := Future(Selector, Opt);
end;

function TDavASIODriver.AsioOutputReady: TASIOError;
begin
  result:=OutputReady;
end;


end.
