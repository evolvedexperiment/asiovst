program VSTEditor;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListModules,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
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

