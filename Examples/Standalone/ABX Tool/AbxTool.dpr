program AbxTool;

{$R 'Standalone.res' 'Standalone.rc'}

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListHardware,
  madListProcesses,
  madListModules,
  Forms,
  AbxMain in 'AbxMain.pas' {FmAbxMain},
  AbxProject in 'AbxProject.pas' {FmProject},
  AbxAbout in 'AbxAbout.pas' {FmAboutBox},
  AbxAudio in 'AbxAudio.pas' {FmAudioSettings},
  AbxProjectSetup in 'AbxProjectSetup.pas' {FmProjectSetup},
  AbxTest in 'AbxTest.pas' {FmAbxTest},
  AbxTestSetup in 'AbxTestSetup.pas' {FmTestSetup},
  AbxChunks in 'AbxChunks.pas',
  AbxResultTableSetup in 'AbxResultTableSetup.pas' {FmResultTableSetup};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TFmAbxMain, FmAbxMain);
  Application.CreateForm(TFmAboutBox, FmAboutBox);
  Application.CreateForm(TFmAudioSettings, FmAudioSettings);
  Application.CreateForm(TFmResultTableSetup, FmResultTableSetup);
  Application.Run;
end.
