program DialTest;

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
  MainUnit in 'MainUnit.pas' {FmSwitchTest},
  DAV_GuiStitchedDial in '..\..\..\Source\GUI\DAV_GuiStitchedDial.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmDialTest, FmDialTest);
  Application.Run;
end.

