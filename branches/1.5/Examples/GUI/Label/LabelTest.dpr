program LabelTest;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  LabelTestMain in 'LabelTestMain.pas' {FmLabelTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmLabelTest, FmLabelTest);
  Application.Run;
end.
