program BarberpoleTuner;

uses
  Forms,
  BTmain in 'BTmain.pas' {FmBarberpoleTuner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmBarberpoleTuner, FmBarberpoleTuner);
  Application.Run;
end.
