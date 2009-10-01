program ButtonTest;

uses
  Forms,
  ButtonTestMain in 'ButtonTestMain.pas' {FmButton};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmButton, FmButton);
  Application.Run;
end.
