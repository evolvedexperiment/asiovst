program MultiSineGenerator;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  {$IFDEF UseMadExcept}
  madExcept,
  {$ENDIF }
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
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
