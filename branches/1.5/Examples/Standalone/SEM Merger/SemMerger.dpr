program SemMerger;

{$I DAV_Compiler.inc}

{$R 'SEMerger.res' 'SEMerger.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  SEmain in 'SEmain.pas' {FormSEModuleExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSEModuleExplorer, FormSEModuleExplorer);
  Application.Run;
end.
