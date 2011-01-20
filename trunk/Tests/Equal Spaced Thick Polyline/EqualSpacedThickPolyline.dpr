program EqualSpacedThickPolyline;

uses
  FastMM4,
  Forms,
  MainUnit in 'MainUnit.pas' {Form1},
  Magnifier in 'Magnifier.pas' {FmMagnifier},
  DAV_GuiFiltersBlur in '..\..\Source\DAV_GuiFiltersBlur.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmESTP, FmESTP);
  Application.CreateForm(TFmMagnifier, FmMagnifier);
  Application.Run;
end.

