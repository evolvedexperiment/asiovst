program SimpleHDRecorder;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListModules,
  madListHardware,
  madListProcesses,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
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
