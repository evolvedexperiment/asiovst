program MultiSineGenerator;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListModules,
  madListHardware,
  madListProcesses,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  Vcl.Themes,
  Vcl.Styles,
  MultiSineGeneratorMain in 'MultiSineGeneratorMain.pas' {FormMultiSineGenerator},
  MultiSineGeneratorFrequency in 'MultiSineGeneratorFrequency.pas' {FormSetFrequency};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Demo application for ASIO-Host';
  Application.CreateForm(TFormMultiSineGenerator, FormMultiSineGenerator);
  Application.Run;
end.
