program AudioEditor;

uses
  Forms,
  AEmain in 'AEmain.pas' {FmAudioEditor},
  DAudioChunks in '..\..\..\Source\DAudioChunks.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioEditor, FmAudioEditor);
  Application.Run;
end.
