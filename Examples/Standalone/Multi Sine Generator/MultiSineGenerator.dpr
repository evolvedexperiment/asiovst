program MultiSineGenerator;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules, // either download the library or comment if there is an error here
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  MultiSineGeneratorMain in 'MultiSineGeneratorMain.pas' {FmASIO},
  MultiSineGeneratorFrequency in 'MultiSineGeneratorFrequency.pas' {FmSetFrequency};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
