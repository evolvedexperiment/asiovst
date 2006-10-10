program MiniHost;

{$MODE Delphi}

uses
  Forms,
  MiniHostForm in 'MiniHostForm.pas' {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {Options},
  LCLIntf, Dialogs, {OpenASIO,}
  aboutform in 'aboutform.pas' {about},
  PlayerForm in 'PlayerForm.pas' {Player},
  HostVSTLaz,
  HostASIOLaz;

{$R *.RES}
begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFmMiniHost, FmMiniHost);
 Application.Run;
end.
