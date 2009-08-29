library AsioDriver;

uses
  ComServ,
  AsioDriverMain in 'AsioDriverMain.pas',
  DAV_ASIOTCWrapper in 'ASIO Driver\DAV_ASIOTCWrapper.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
