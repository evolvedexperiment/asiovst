program ASIOMP3VST;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  ASIOMP3VSTGUI in 'ASIOMP3VSTGUI.pas' {FormASIOMP3VST},
  ASIOMP3VSTSetup in 'ASIOMP3VSTSetup.pas' {FormSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO MP3 Player';
  Application.CreateForm(TFormASIOMP3VST, FormASIOMP3VST);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.Run;
end.
