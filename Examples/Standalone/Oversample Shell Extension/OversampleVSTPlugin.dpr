library OversampleVSTPlugin;

{$R 'OversampleTemplate.res' 'OversampleTemplate.rc'}

uses
  FastMM4,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  FastMove,
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
