program StitchedButtonTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  MainUnit in 'MainUnit.pas' {FmStitchedButtonTest};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmStitchedButtonTest, FmStitchedButtonTest);
  Application.Run;
end.

