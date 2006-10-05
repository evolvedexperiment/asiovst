program MiniHost;

uses
  Forms,
  MiniHostForm in 'MiniHostForm.pas' {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {Options},
  Windows, Dialogs, OpenASIO,
  aboutform in 'aboutform.pas' {about},
  PlayerForm in 'PlayerForm.pas' {Player};

{$R *.RES}
begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFmMiniHost, FmMiniHost);
 Application.Run;
end.
