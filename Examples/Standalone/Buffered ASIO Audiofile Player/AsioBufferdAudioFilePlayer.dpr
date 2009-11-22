program AsioBufferdAudioFilePlayer;

uses
  FastMM4, // either download the library or comment if there is an error here
  FastMove, // either download the library or comment if there is an error here
  madExcept, // either download madExcept or remove mad* if there is an error here
  madListHardware,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  Forms,
  AsioBufferdAudioFilePlayerGUI in 'AsioBufferdAudioFilePlayerGUI.pas' {FmAsioBufferdAudioFilePlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFmAsioBufferdAudioFilePlayer, FmAsioBufferdAudioFilePlayer);
  Application.Run;
end.
