unit PanelTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, ComCtrls, ExtCtrls, DAV_GuiPanel;

type
  TFmPanelTest = class(TForm)
    PanelA: TGuiPanel;
    PanelB: TGuiPanel;
    PanelC: TGuiPanel;
    PanelD: TGuiPanel;
    TbRoundRadius: TTrackBar;
    LbRoundRadius: TLabel;
    TbLineWidth: TTrackBar;
    Label1: TLabel;
    CbTransparent: TCheckBox;
    procedure TbRoundRadiusChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
  private
    { Private-Deklarationen }
  public
    { Public-Deklarationen }
  end;

var
  FmPanelTest: TFmPanelTest;

implementation

{$R *.dfm}

procedure TFmPanelTest.CbTransparentClick(Sender: TObject);
begin
 PanelA.Transparent := CbTransparent.Checked;
 PanelB.Transparent := CbTransparent.Checked;
 PanelC.Transparent := CbTransparent.Checked;
 PanelD.Transparent := CbTransparent.Checked;
end;

procedure TFmPanelTest.TbLineWidthChange(Sender: TObject);
begin
 PanelA.Linewidth := TbLinewidth.Position;
 PanelB.Linewidth := TbLinewidth.Position;
 PanelC.Linewidth := TbLinewidth.Position;
 PanelD.Linewidth := TbLinewidth.Position;
end;

procedure TFmPanelTest.TbRoundRadiusChange(Sender: TObject);
begin
 PanelA.Radius := TbRoundRadius.Position;
 PanelB.Radius := TbRoundRadius.Position;
 PanelC.Radius := TbRoundRadius.Position;
 PanelD.Radius := TbRoundRadius.Position;
end;

end.
