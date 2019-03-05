program SemMerger;

{$I DAV_Compiler.inc}

{$R 'SEMerger.res' 'SEMerger.rc'}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  SEmain in 'SEmain.pas' {FormSEModuleExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormSEModuleExplorer, FormSEModuleExplorer);
  Application.Run;
end.
