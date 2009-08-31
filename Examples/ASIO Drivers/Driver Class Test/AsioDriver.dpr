library AsioDriver;

uses
  ComServ,
  AsioDriverMain in 'AsioDriverMain.pas',
  DAV_ASIODriver in 'ASIO Driver\DAV_ASIODriver.pas',
  DAV_ASIOExtendedDriver in 'ASIO Driver\DAV_ASIOExtendedDriver.pas',
  AsioDriverMainCPanel in 'AsioDriverMainCPanel.pas' {DriverTestCP};

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
