program ButtonTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  ButtonTestMain in 'ButtonTestMain.pas' {FmButton};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmButton, FmButton);
  Application.Run;
end.
