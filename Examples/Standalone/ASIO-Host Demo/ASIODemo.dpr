program ASIODemo;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$IFDEF UseMadExcept}
  madExcept,
  {$ENDIF }
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  ASIODemoForm in 'ASIODemoForm.pas' {FormASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFormASIO, FormASIO);
  Application.Run;
end.
