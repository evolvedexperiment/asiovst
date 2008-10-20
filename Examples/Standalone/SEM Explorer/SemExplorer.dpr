program SemExplorer;

{$R 'SEMagicHealer.res' 'SEMagicHealer.rc'}

uses
  Forms,
  SEmain in 'SEmain.pas' {FmSEModuleExplorer};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSEModuleExplorer, FmSEModuleExplorer);
  Application.Run;
end.
