program StaticWaveformTest;

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
  StaticWaveformTestMain in 'StaticWaveformTestMain.pas' {FmStaticWaveformTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmStaticWaveformTest, FmStaticWaveformTest);
  Application.Run;
end.
