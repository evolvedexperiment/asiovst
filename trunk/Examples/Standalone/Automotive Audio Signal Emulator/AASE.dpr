program AASE;

uses
  FastMM4,
  FastMove,
  Forms,
  AaseMain in 'AaseMain.pas' {FmAASE},
  AaseSetup in 'AaseSetup.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFmAASE, FmAASE);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
