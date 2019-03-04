program Generator;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  GenMain in 'GenMain.pas' {FmGenerator};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Generator';
  Application.CreateForm(TFmGenerator, FmGenerator);
  Application.Run;
end.
