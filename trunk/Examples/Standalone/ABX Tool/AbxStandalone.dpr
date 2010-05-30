program AbxStandalone;

uses
  FastMM4,
  madListHardware, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  AbxStandaloneTest in 'AbxStandaloneTest.pas' {FmABXStandaloneTest},
  AbxStandaloneAudioSetup in 'AbxStandaloneAudioSetup.pas' {FmAudioSettings};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'ABX Test (Standalone)';
  Application.CreateForm(TFmABXStandaloneTest, FmABXStandaloneTest);
  Application.CreateForm(TFmAudioSettings, FmAudioSettings);
  Application.Run;
end.
