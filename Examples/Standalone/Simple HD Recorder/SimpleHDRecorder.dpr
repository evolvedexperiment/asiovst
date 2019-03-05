program SimpleHDRecorder;

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
  SHRmain in 'SHRmain.pas' {FormSimpleHDRecorder},
  SHRSetup in 'SHRSetup.pas' {FormSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple HD Recorder';
  Application.CreateForm(TFormSimpleHDRecorder, FormSimpleHDRecorder);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.Run;
end.
