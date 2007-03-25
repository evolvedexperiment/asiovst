program Analyser;

uses
  Interfaces,
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO},
  DChebyshevFilter in 'DChebyshevFilter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Analyser';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
