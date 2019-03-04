program GroupBoxTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  GroupBoxTestMain in 'GroupBoxTestMain.pas' {Form1},
  DAV_GuiGroup in '..\..\..\Source\GUI\DAV_GuiGroup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmGroupBoxTest, FmGroupBoxTest);
  Application.Run;
end.
