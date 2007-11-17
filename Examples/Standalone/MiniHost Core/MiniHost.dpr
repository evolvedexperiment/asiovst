program MiniHost;

uses
  Forms,
  MiniHostForm in 'MiniHostForm.pas' {FmMiniHost},
  OptionsForm in 'OptionsForm.pas' {FmOptions},
  PlayerForm in 'PlayerForm.pas' {Player},
  AboutForm in 'aboutform.pas' {FmAbout};

{$R *.RES}
begin
 Application.Initialize;
 Application.Title := 'Tobybear MiniHost';
 Application.CreateForm(TFmMiniHost, FmMiniHost);
 Application.Run;
end.
