program ASIODemo;

uses
  FastMM4,
  FastMove,
  Forms,
  AsioDemoForm in 'ASIODemoForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
