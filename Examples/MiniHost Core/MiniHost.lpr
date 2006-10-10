program MiniHost;

{$MODE Delphi}

uses
  interfaces,
  LCLIntf,
  Forms,
  MiniHostForm in 'MiniHostForm.pas' {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {Options},
  aboutform in 'aboutform.pas' {about},
  PlayerForm in 'PlayerForm.pas' {Player},
  HostVSTLaz,
  HostASIOLaz;

{$R *.RES}

begin
 Application.Initialize;
 Application.Title:='Mini Host Core';
 Application.CreateForm(TFmMiniHost, FmMiniHost);
 Application.Run;
end.
