program ASIODemoAudioData;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  ASIODemoForm in 'ASIODemoForm.pas' {FormAnalyserGoertzel};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFormAnalyserGoertzel, FormAnalyserGoertzel);
  Application.Run;
end.
