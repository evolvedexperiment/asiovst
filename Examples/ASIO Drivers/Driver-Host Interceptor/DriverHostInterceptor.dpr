library DriverHostInterceptor;

uses
  ComServ,
  DrvrHostIntMain in 'DrvrHostIntMain.pas',
  DAV_ASIODriverInterceptor in 'DAV_ASIODriverInterceptor.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
