program StitchedSwitchTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FmSwitchTest},
  DAV_GuiFileFormatGraphics in '..\..\..\Source\GUI\DAV_GuiFileFormatGraphics.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSwitchTest, FmSwitchTest);
  Application.Run;
end.

