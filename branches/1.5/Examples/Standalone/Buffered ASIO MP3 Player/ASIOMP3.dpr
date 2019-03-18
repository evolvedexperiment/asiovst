program ASIOMP3;

uses
  FastMM4, // either download the library or comment if there is an error here
  {$IFDEF UseMadExcept}
  madExcept, // either download madExcept or remove mad* if there is an error here
  madLinkDisAsm,
  {$ENDIF}
  {$IFDEF UseFastMove}
  FastMove, // either download the library or disable the feature
  {$ENDIF}
  Forms,
  ASIOMP3GUI in 'ASIOMP3GUI.pas' {FormASIOMP3};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'Buffered ASIO MP3 Player';
  Application.CreateForm(TFormASIOMP3, FormASIOMP3);
  Application.Run;
end.
