program ASIODemo;

{$I DAV_Compiler.inc}

uses
  FastMM4,  // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF}
  Forms,
  AsioDemoForm in 'ASIODemoForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
