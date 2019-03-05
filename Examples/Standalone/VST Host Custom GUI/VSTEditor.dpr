program VSTEditor;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  EditorForm in 'EditorForm.pas' {FormVSTEditor},
  EditorSetup in 'EditorSetup.pas' {FormSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'VST Plugin Editor';
  Application.CreateForm(TFormVSTEditor, FormVSTEditor);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.Run;
end.

