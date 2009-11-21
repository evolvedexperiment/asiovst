program EqualizedNoiseGenerator;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  EqualizedNoiseGeneratorForm in 'EqualizedNoiseGeneratorForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ASIO Equalized Noise Generator';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
