unit DAV_ASIODriver;

interface

uses
  Windows,DAV_ASIO;

type
  TDavASIODriver = class
  public
    ftestvar: integer;
    constructor create;
    function Init(SysHandle: HWND): TASIOBool;
  end;

  TTDavASIODriver = class of TDavASIODriver;

implementation

{ TDavASIODriver }

uses sysutils,dialogs;

constructor TDavASIODriver.create;
begin
  ftestvar:=1234
end;

function TDavASIODriver.Init(SysHandle: HWND): TASIOBool;
begin
  showmessage(inttostr(ftestvar));
  result := ASE_SPNotAdvancing;
end;
end.
