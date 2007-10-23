program TOAnalyser;

uses
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO},
  DDspChebyshevFilter in '..\..\..\Source\DDspChebyshevFilter.pas',
  DDspButterworthFilter in '..\..\..\Source\DDspButterworthFilter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Analyser';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
