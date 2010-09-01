program Generator;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  GenMain in 'GenMain.pas' {FmGenerator};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Generator';
  Application.CreateForm(TFmGenerator, FmGenerator);
  Application.Run;
end.
