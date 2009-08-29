unit DAV_ASIOTCWrapper;

interface

uses
  Windows, ActiveX, ComObj, DAV_ASIO, DAV_ASIODriver;

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
    procedure GetDriverClass2; virtual; abstract;
    procedure GetDriverClass3; virtual; abstract;
    procedure GetDriverClass4; virtual; abstract;
    procedure GetDriverClass5; virtual; abstract;
  end;

  TDavAsioDriverFactory = class(TComObjectFactory)
  public
    procedure UpdateRegistry(Register: Boolean); override;
  end;

IMPLEMENTATION

{ TAsioDriver }

const DavASIOInterfaceOffset = $24;

procedure TDavASIOTCWrapper.Initialize;
begin
  inherited;
  Assert(DavASIOInterfaceOffset = GetInterfaceTable^.Entries[0].IOffset);
  FDestinationClass:=GetDriverClass.Create;
end;

destructor TDavASIOTCWrapper.Destroy;
begin
  FDestinationClass.Free;
  FDestinationClass:=nil;
  inherited;
end;

procedure TDavASIOTCWrapper.Init;
asm
  pop ebx   // pop return address
  pop edx
  push ebx  // push retur adress again

  // generate new "self" pointer for the called class function
  mov eax,ecx
  sub eax,DavASIOInterfaceOffset
  mov eax,[self.FDestinationClass]
  call FDestinationClass.init-FDestinationClass 
end;

procedure TDavASIOTCWrapper.GetDriverName;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetDriverVersion;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetErrorMessage;
asm
  nop
end;

procedure TDavASIOTCWrapper.Start;
asm
  nop
end;

procedure TDavASIOTCWrapper.Stop;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetChannels;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetLatencies;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetBufferSize;
asm
  nop
end;

procedure TDavASIOTCWrapper.CanSampleRate;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetSampleRate;
asm
  nop
end;

procedure TDavASIOTCWrapper.SetSampleRate;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetClockSources;
asm
  nop
end;

procedure TDavASIOTCWrapper.SetClockSource;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetSamplePosition;
asm
  nop
end;

procedure TDavASIOTCWrapper.GetChannelInfo;
asm
  nop
end;

procedure TDavASIOTCWrapper.CreateBuffers;
asm
  nop
end;

procedure TDavASIOTCWrapper.DisposeBuffers;
asm
  nop
end;

procedure TDavASIOTCWrapper.ControlPanel;
asm
  nop
end;

procedure TDavASIOTCWrapper.Future;
asm
  nop
end;

procedure TDavASIOTCWrapper.OutputReady;
asm
  nop
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

end.
