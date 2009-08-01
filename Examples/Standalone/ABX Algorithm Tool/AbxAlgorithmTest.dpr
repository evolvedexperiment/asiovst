program AbxAlgorithmTest;

uses
  FastMM4,
  FastMove,
  Forms,
  AbxMain in 'AbxMain.pas' {FmAbxAlgorithmTest},
  AbxTest in 'AbxTest.pas' {FmAbxTest},
  AbxAudio in 'AbxAudio.pas' {FmAudioSettings},
  AbxTestSetup in 'AbxTestSetup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ABX algorithm Test';
  Application.CreateForm(TFmAbxAlgorithmTest, FmAbxAlgorithmTest);
  Application.CreateForm(TFmAudioSettings, FmAudioSettings);
  Application.Run;
end.
