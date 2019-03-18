program AsioBufferdAudioFilePlayer;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  madListModules,
  madListHardware,
  madListProcesses,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  AsioBufferdAudioFilePlayerGUI in 'AsioBufferdAudioFilePlayerGUI.pas' {FormAsioBufferdAudioFilePlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFormAsioBufferdAudioFilePlayer, FormAsioBufferdAudioFilePlayer);
  Application.Run;
end.
