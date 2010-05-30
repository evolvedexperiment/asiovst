program VST2Sonogram;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  VSmain in 'VSmain.pas' {FmSonogram};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSonogram, FmSonogram);
  Application.Run;
end.
