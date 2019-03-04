program SimpleHDRecorder;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download the library or disable the feature
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  SHRmain in 'SHRmain.pas' {FmSimpleHDRecorder},
  SHRSetup in 'SHRSetup.pas' {FmSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple HD Recorder';
  Application.CreateForm(TFmSimpleHDRecorder, FmSimpleHDRecorder);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
