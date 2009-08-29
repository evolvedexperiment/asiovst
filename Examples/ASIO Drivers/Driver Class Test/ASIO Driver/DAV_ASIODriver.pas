unit DAV_ASIODriver;

interface

uses
  Windows,DAV_ASIO;

type
  TDavASIODriver = class
  private
    fTCWrapper: TDavASIOTCWrapper;
  public
    constructor create(TCWrapper: TDavASIOTCWrapper);


    function Init(SysHandle: HWND): boolean; virtual;
    function GetDriverName: string; virtual;

    function AsioInit(SysHandle: HWND): TASIOBool;
    procedure AsioGetDriverName(Name: PAnsiChar);
  end;

  TTDavASIODriver = class of TDavASIODriver;

implementation

{ TDavASIODriver }

uses sysutils,dialogs;

constructor TDavASIODriver.create(TCWrapper: TDavASIOTCWrapper);
begin
  fTCWrapper := TCWrapper;
end;


function TDavASIODriver.GetDriverName: string;
begin
  result := 'DAV Abstract Driver';
end;

function TDavASIODriver.Init(SysHandle: HWND): boolean;
begin
  result := true;
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

end.
