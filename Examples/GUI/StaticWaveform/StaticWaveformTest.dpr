program StaticWaveformTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  StaticWaveformTestMain in 'StaticWaveformTestMain.pas' {FmStaticWaveformTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmStaticWaveformTest, FmStaticWaveformTest);
  Application.Run;
end.
