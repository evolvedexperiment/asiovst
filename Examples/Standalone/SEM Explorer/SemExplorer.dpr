program SemExplorer;

{$I DAV_Compiler.inc}

{$R 'SEMagicHealer.res' 'SEMagicHealer.rc'}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
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
