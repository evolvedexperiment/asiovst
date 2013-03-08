program ASIODemo;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms,
  AsioDemoForm in 'ASIODemoForm.pas' {FmASIO};

begin
  Application.Initialize;
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
