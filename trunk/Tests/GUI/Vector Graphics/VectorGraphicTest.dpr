program VectorGraphicTest;

uses
  Forms,
  MainUnit in 'MainUnit.pas' {FmVectorGraphicTest},
  DAV_GuiVectorPixel in '..\..\..\Source\GUI\DAV_GuiVectorPixel.pas',
  DAV_GuiFixedPoint in '..\..\..\Source\GUI\DAV_GuiFixedPoint.pas',
  DAV_GuiVectorPixelLine in '..\..\..\Source\GUI\DAV_GuiVectorPixelLine.pas',
  DAV_GuiVectorPixelCircle in '..\..\..\Source\GUI\DAV_GuiVectorPixelCircle.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TFmVectorGraphicTest, FmVectorGraphicTest);
  Application.Run;
end.

