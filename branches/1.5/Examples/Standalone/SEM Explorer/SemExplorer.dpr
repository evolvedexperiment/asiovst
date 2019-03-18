program SemExplorer;

{$I DAV_Compiler.inc}

{$R 'SEMagicHealer.res' 'SEMagicHealer.rc'}

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
  SEmain in 'SEmain.pas' {FormSEModuleExplorer},
  SEabout in 'SEabout.pas' {FormAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSEModuleExplorer, FormSEModuleExplorer);
  Application.CreateForm(TFormAbout, FormAbout);
  Application.Run;
end.
