program SemExplorer;

{$I DAV_Compiler.inc}

{$R 'SEMagicHealer.res' 'SEMagicHealer.rc'}

uses
  FastMM4,  // if this line makes trouble, remove or download this library!
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  SEmain in 'SEmain.pas' {FmSEModuleExplorer},
  SEabout in 'SEabout.pas' {FmAbout};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSEModuleExplorer, FmSEModuleExplorer);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.Run;
end.
