program EqualizedSoundGenerator;

uses
  Forms,
  EqualizedSoundGeneratorForm in 'EqualizedSoundGeneratorForm.pas' {FmASIO};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'ASIO Equalized Sound Generator';
  Application.CreateForm(TFmASIO, FmASIO);
  Application.Run;
end.
