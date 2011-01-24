program EqualSpacedThickPolyline;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {FmESTP},
  Magnifier in 'Magnifier.pas' {FmMagnifier};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmESTP, FmESTP);
  Application.CreateForm(TFmMagnifier, FmMagnifier);
  Application.Run;
end.

