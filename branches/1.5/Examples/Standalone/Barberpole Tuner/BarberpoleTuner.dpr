program BarberpoleTuner;

uses
  FastMM4,
  FastMove,
  Forms,
  BTmain in 'BTmain.pas' {FormBarberpoleTuner};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormBarberpoleTuner, FormBarberpoleTuner);
  Application.Run;
end.
