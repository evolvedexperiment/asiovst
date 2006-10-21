program Analyser;

uses
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
