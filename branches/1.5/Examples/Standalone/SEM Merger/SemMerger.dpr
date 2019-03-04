program SemMerger;

{$I DAV_Compiler.inc}

{$R 'SEMerger.res' 'SEMerger.rc'}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  SEmain in 'SEmain.pas' {FmSEModuleExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSEModuleExplorer, FmSEModuleExplorer);
  Application.Run;
end.
