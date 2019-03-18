program ASIOMP3VST;

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
