program Generator;

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
  GenMain in 'GenMain.pas' {FormGenerator};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Generator';
  Application.CreateForm(TFormGenerator, FormGenerator);
  Application.Run;
end.
