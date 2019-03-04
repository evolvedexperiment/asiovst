program ASIOMP3VST;

{$I DAV_Compiler.inc}

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  ASIOMP3VSTGUI in 'ASIOMP3VSTGUI.pas' {FmASIOMP3VST},
  ASIOMP3VSTSetup in 'ASIOMP3VSTSetup.pas' {FmSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO MP3 Player';
  Application.CreateForm(TFmASIOMP3VST, FmASIOMP3VST);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
