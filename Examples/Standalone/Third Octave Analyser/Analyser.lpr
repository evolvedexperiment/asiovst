program Analyser;

uses
  Interfaces,
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO},
  AnalyserChebyshevFilter in 'AnalyserChebyshevFilter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Analyser';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
