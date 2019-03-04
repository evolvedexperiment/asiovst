program PanelTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  PanelTestMain in 'PanelTestMain.pas' {FmPanelTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPanelTest, FmPanelTest);
  Application.Run;
end.
