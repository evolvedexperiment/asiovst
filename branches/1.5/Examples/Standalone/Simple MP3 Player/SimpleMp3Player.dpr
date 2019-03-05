program SimpleMp3Player;

{$I DAV_Compiler.inc}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  SmpMain in 'SmpMain.pas' {FormSimpleMp3Player},
  SmpSetup in 'SmpSetup.pas' {FormSetup};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFormSimpleMp3Player, FormSimpleMp3Player);
  Application.CreateForm(TFormSetup, FormSetup);
  Application.Run;
end.
