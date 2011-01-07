program AASE;

uses
  FastMM4,
  FastMove,
  Forms,
  AaseMain in 'AaseMain.pas' {FmAASE},
  AaseSetup in 'AaseSetup.pas';

{$R *.res}

{$SetPEFlags 1}

begin
  Application.Initialize;
  Application.Title := 'Automotive Audio Signal Emulator';
  Application.CreateForm(TFmAASE, FmAASE);
  Application.CreateForm(TFmSetup, FmSetup);
  Application.Run;
end.
