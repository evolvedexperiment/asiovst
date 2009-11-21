program HrtfAverager;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  HAmain in 'HAmain.pas' {FmHrtfAverager};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmHrtfAverager, FmHrtfAverager);
  Application.Run;
end.
