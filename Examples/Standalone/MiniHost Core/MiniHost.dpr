program MiniHost;

{$I DAV_Compiler.inc}

{.$R 'EmbeddedPlugin.res' 'EmbeddedPlugin.rc'}

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
  MiniHostForm in 'MiniHostForm.pas' {FormMiniHost},
  OptionsForm in 'OptionsForm.pas' {FormOptions},
  PlayerForm in 'PlayerForm.pas' {Player},
  AboutForm in 'AboutForm.pas' {FormAbout};

{$R *.RES}
begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFormMiniHost, FormMiniHost);
  Application.Run;
end.
