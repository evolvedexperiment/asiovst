program GraphXyTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  GraphXYtestMain in 'GraphXYtestMain.pas' {FmGraphXY};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGraphXY, FmGraphXY);
  Application.Run;
end.
