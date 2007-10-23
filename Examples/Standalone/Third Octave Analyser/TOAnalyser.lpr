program TOAnalyser;

uses
  Interfaces,
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO},
  DDspChebyshevFilter in 'DDspChebyshevFilter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Analyser';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
