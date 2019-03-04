program SelectBox;

{$I DAV_Compiler.inc}

uses
  FastMM4,
{$IFDEF UseMadExcept}
  madExcept,
  madLinkDisAsm,
{$ENDIF}
  Forms,
  SelectBoxTest in 'SelectBoxTest.pas' {FmSelectBox};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmSelectBox, FmSelectBox);
  Application.Run;
end.
