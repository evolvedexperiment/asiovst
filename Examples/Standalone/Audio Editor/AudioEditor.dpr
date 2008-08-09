program AudioEditor;

uses
  Forms,
  AEmain in 'AEmain.pas' {FmAudioEditor};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioEditor, FmAudioEditor);
  Application.Run;
end.
