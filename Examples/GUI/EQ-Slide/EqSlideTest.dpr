program EqSlideTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  EqSlideTestMain in 'EqSlideTestMain.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmEqSlideTest, FmEqSlideTest);
  Application.Run;
end.
