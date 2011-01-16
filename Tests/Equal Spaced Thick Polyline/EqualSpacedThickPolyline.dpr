program EqualSpacedThickPolyline;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmESTP, FmESTP);
  Application.Run;
end.

