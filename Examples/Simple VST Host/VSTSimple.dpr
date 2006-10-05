program VSTSimple;

uses
  Forms,
  HVSTMain in 'HVSTMain.pas' {SimpleVSTHost},
  HVSTAbout in 'HVSTAbout.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSimpleVSTHost, SimpleVSTHost);
  Application.Run;
end.
