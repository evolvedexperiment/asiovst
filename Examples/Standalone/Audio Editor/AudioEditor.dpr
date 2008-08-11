program AudioEditor;

uses
  Forms,
  AEmain in 'AEmain.pas' {FmAudioEditor},
  DChunkWaveFile in '..\..\..\Source\DChunkWaveFile.pas',
  DWaveFileTypes in '..\..\..\Source\DWaveFileTypes.pas',
  DChunkClasses in '..\..\..\Source\DChunkClasses.pas',
  DChunkAIFFFile in '..\..\..\Source\DChunkAIFFFile.pas',
  DAudioFileAIFF in '..\..\..\Source\DAudioFileAIFF.pas',
  DAudioFileAU in '..\..\..\Source\DAudioFileAU.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioEditor, FmAudioEditor);
  Application.Run;
end.
