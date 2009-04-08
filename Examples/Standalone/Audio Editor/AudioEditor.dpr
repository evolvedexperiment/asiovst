program AudioEditor;

uses
  Forms,
  AEmain in 'AEmain.pas' {FmAudioEditor},
  DAV_ChunkWaveFile in '..\..\..\Source\DAV_ChunkWaveFile.pas',
  DAV_WaveFileTypes in '..\..\..\Source\DAV_WaveFileTypes.pas',
  DAV_ChunkClasses in '..\..\..\Source\DAV_ChunkClasses.pas',
  DAV_ChunkAIFFFile in '..\..\..\Source\DAV_ChunkAIFFFile.pas',
  DAV_AudioFileAIFF in '..\..\..\Source\DAV_AudioFileAIFF.pas',
  DAV_AudioFileWAV in '..\..\..\Source\DAV_AudioFileWAV.pas',
  DAV_AudioFileAU in '..\..\..\Source\DAV_AudioFileAU.pas',
  DAV_AudioData in '..\..\..\Source\DAV_AudioData.pas',
  AEAsioSetup in 'AEAsioSetup.pas' {FmSetup},
  AEVstSetup in 'AEVstSetup.pas' {FmVSTSetup},
  DAV_GuiAudioDataDisplayCursor in '..\..\..\Source\GUI\DAV_GuiAudioDataDisplayCursor.pas',
  DAV_GuiAudioDataDisplayAxis in '..\..\..\Source\GUI\DAV_GuiAudioDataDisplayAxis.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmAudioEditor, FmAudioEditor);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.CreateForm(TFmVSTSetup, FmVSTSetup);
  Application.Run;
end.
