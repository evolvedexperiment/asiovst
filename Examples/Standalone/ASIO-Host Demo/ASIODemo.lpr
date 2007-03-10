program ASIODemo;

{$MODE Delphi}

uses
  Forms,
  AsioDemoForm in 'ASIODemoForm.pas' {FmASIO}, HostASIOLaz;

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
