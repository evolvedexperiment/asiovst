program LedTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  LedTestMain in 'LedTestMain.pas' {FmLEDTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmLEDTest, FmLEDTest);
  Application.Run;
end.
