program MiniHost;

{$I DAV_Compiler.inc}

{.$R 'EmbeddedPlugin.res' 'EmbeddedPlugin.rc'}

uses
  FastMM4,
  {$IFDEF UseFastMove}
  FastMove,
  {$ENDIF }
  Forms,
  MiniHostForm in 'MiniHostForm.pas' {FormMiniHost},
  OptionsForm in 'OptionsForm.pas' {FormOptions},
  PlayerForm in 'PlayerForm.pas' {Player},
  AboutForm in 'AboutForm.pas' {FmAbout};

{$R *.RES}
begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFormMiniHost, FormMiniHost);
  Application.Run;
end.
