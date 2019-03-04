program DialTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FmSwitchTest},
  DAV_GuiStitchedDial in '..\..\..\Source\GUI\DAV_GuiStitchedDial.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmDialTest, FmDialTest);
  Application.Run;
end.

