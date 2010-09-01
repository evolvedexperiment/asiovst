program AbxAlgorithmTest;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  AbxMain in 'AbxMain.pas' {FmAbxAlgorithmTest},
  AbxTest in 'AbxTest.pas' {FmAbxTest},
  AbxAudio in 'AbxAudio.pas' {FmAudioSettings},
  AbxTestSetup in 'AbxTestSetup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ABX Algorithm Test';
  Application.CreateForm(TFmAbxAlgorithmTest, FmAbxAlgorithmTest);
  Application.CreateForm(TFmAudioSettings, FmAudioSettings);
  Application.Run;
end.
