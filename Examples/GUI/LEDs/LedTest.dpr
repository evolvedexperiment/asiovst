program LedTest;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  LedTestMain in 'LedTestMain.pas' {FmLEDTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmLEDTest, FmLEDTest);
  Application.Run;
end.
