program ASIOMP3;

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  ASIOMP3GUI in 'ASIOMP3GUI.pas' {FmASIOMP3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Simple ASIO MP3 Player';
  Application.CreateForm(TFmASIOMP3, FmASIOMP3);
  Application.Run;
end.
