program PanelTest;

uses
  Forms,
  PanelTestMain in 'PanelTestMain.pas' {FmPanelTest},
  DAV_GuiPanelNew in '..\..\..\Source\GUI\DAV_GuiPanelNew.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPanelTest, FmPanelTest);
  Application.Run;
end.
