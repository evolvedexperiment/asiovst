program VSTDebug;

uses
  Forms,
  Debug in 'Debug.pas' {FmVSTDebug},
  uEditor in 'uEditor.pas' {PluginEditorWindow},
  uPlugin in 'uPlugin.pas';

{$R Debug.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmVSTDebug, FmVSTDebug);
  Application.Run;
end.
