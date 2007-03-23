program MiniHost;

{$MODE Delphi}

uses
  Interfaces,
  Forms, LCLIntf, Dialogs,
  MiniHostForm in 'MiniHostForm.pas' {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {Options},
  aboutform in 'AboutForm.pas' {about},
  PlayerForm in 'PlayerForm.pas' {Player}, HostVSTLaz, HostASIOLaz;

{$R *.RES}

begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFmMiniHost, FmMiniHost);
 Application.Run;
end.
