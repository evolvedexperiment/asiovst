program PanelTest;

uses
  Forms,
  PanelTestMain in 'PanelTestMain.pas' {FmPanelTest};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFmPanelTest, FmPanelTest);
  Application.Run;
end.
