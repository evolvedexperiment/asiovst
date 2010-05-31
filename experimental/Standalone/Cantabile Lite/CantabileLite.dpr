program CantabileLite;

uses
  Forms,
  CLMain in 'CLMain.pas' {FmCantabileLite};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmCantabileLite, FmCantabileLite);
  Application.Run;
end.
