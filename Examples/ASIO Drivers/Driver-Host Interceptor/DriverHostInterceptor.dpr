library DriverHostInterceptor;

uses
  ComServ,
  DrvrHostIntMain in 'DrvrHostIntMain.pas',
  DAV_ASIODriverInterceptor in 'DAV_ASIODriverInterceptor.pas',
  DrvrHostIntCPanel in 'DrvrHostIntCPanel.pas';

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

{$R *.RES}

begin
end.
