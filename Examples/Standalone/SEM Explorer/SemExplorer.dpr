program SemExplorer;

{$R 'SEMagicHealer.res' 'SEMagicHealer.rc'}

uses
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
