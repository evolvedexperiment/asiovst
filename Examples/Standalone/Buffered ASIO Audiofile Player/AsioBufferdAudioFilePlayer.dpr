program AsioBufferdAudioFilePlayer;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  madLinkDisAsm,
  madListProcesses,
  madListModules,
  madListHardware,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  {$IFDEF UseMadExcept}
  madExcept,
  {$ENDIF }
  Forms,
  AsioBufferdAudioFilePlayerGUI in 'AsioBufferdAudioFilePlayerGUI.pas' {FormAsioBufferdAudioFilePlayer};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFormAsioBufferdAudioFilePlayer, FormAsioBufferdAudioFilePlayer);
  Application.Run;
end.
