program EqualizedNoiseGenerator;

uses
  Forms,
  EqualizedNoiseGeneratorForm in 'EqualizedNoiseGeneratorForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ASIO Equalized Noise Generator';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
