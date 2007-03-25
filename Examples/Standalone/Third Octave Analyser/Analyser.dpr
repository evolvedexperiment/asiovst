program Analyser;

uses
  Forms,
  AnalyserForm in 'AnalyserForm.pas' {FmASIO},
  DChebyshevFilter in '..\..\..\Source\DChebyshevFilter.pas',
  DButterworthFilter in '..\..\..\Source\DButterworthFilter.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO Analyser';
  Application.CreateForm(TFmAnalyser, FmAnalyser);
  Application.Run;
end.
