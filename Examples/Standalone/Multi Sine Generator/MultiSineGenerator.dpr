program MultiSineGenerator;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  MultiSineGeneratorMain in 'MultiSineGeneratorMain.pas' {FmASIO},
  MultiSineGeneratorFrequency in 'MultiSineGeneratorFrequency.pas' {FmSetFrequency};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
