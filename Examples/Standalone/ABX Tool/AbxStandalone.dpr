program AbxStandalone;

uses
  FastMM4,
  FastMove,
  madExcept,
  madLinkDisAsm,
  madListHardware,
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
