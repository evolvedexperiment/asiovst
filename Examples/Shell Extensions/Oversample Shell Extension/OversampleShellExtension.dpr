library OversampleShellExtension;

{$R 'OversampleTemplate.res' 'OversampleTemplate.rc'}

uses
  FastMM4,  // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  RTLVCLOptimize, // "
  ComServ,
  SysUtils,
  Classes,
  OversamplePlugin in 'OversamplePlugin.pas';

{$R *.res}

exports
  DllGetClassObject,
  DllCanUnloadNow,
  DllRegisterServer,
  DllUnregisterServer;

begin
end.
