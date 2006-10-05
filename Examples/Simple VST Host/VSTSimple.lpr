program VSTSimple;

{$MODE Delphi}

uses
  Forms,
  Interfaces,
  HVSTMain in 'HVSTMain.pas' {Form1},
  HostVSTLaz, HVSTAbout, EQGraphL, AudioChartsL, GR32_DSGN_L;

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TSimpleVSTHost, SimpleVSTHost);
  Application.CreateForm(TFmAbout, FmAbout);
  Application.Run;
end.
