program BarberpoleTuner;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  BTmain in 'BTmain.pas' {FmBarberpoleTuner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmBarberpoleTuner, FmBarberpoleTuner);
  Application.Run;
end.
