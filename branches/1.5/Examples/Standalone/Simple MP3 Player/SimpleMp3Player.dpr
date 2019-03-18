program SimpleMp3Player;

{$I DAV_Compiler.inc}

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
