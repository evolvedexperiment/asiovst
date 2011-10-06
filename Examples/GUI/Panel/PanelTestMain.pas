unit PanelTestMain;

{$I DAV_Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiPanel,
  DAV_GuiSlider, DAV_GuiCheckBox, DAV_GuiGraphicControl, DAV_GuiLabel;

type
  TFmPanelTest = class(TForm)
    PanelA: TGuiPanel;
    PanelB: TGuiPanel;
    PanelC: TGuiPanel;
    PanelD: TGuiPanel;
    SlRoundRadius: TGuiSlider;
    SlLineWidth: TGuiSlider;
    LbRoundRadius: TGuiLabel;
    LbLineWidth: TGuiLabel;
    CbTransparent: TGuiControlsCheckBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure SlLineWidthChange(Sender: TObject);
    procedure SlRoundRadiusChange(Sender: TObject);
    procedure PanelAClick(Sender: TObject);
    procedure PanelBClick(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmPanelTest: TFmPanelTest;

implementation

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmPanelTest.FormCreate(Sender: TObject);
begin
 // Create Background Image
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmPanelTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmPanelTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmPanelTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLne : PPixel32Array;
begin
 with FBackground do
  begin
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLne := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       ScnLne[x].B := Round($70 - $34 * (s[1] - h));
       ScnLne[x].G := Round($84 - $48 * (s[1] - h));
       ScnLne[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmPanelTest.PanelAClick(Sender: TObject);
begin
 SlLineWidth.Value := 3;
 SlRoundRadius.Value := 3.9;
end;

procedure TFmPanelTest.PanelBClick(Sender: TObject);
begin
 SlLineWidth.Value := 6.632;
 SlRoundRadius.Value := 5.054;
end;

procedure TFmPanelTest.CbTransparentClick(Sender: TObject);
begin
 PanelA.Transparent := CbTransparent.Checked;
 PanelB.Transparent := CbTransparent.Checked;
 PanelC.Transparent := CbTransparent.Checked;
 PanelD.Transparent := CbTransparent.Checked;
end;

procedure TFmPanelTest.SlLineWidthChange(Sender: TObject);
begin
 PanelA.Borderwidth := SlLineWidth.Value;
 PanelB.Borderwidth := SlLineWidth.Value;
 PanelC.Borderwidth := SlLineWidth.Value;
 PanelD.Borderwidth := SlLineWidth.Value;
end;

procedure TFmPanelTest.SlRoundRadiusChange(Sender: TObject);
begin
 PanelA.BorderRadius := SlRoundRadius.Value;
 PanelB.BorderRadius := SlRoundRadius.Value;
 PanelC.BorderRadius := SlRoundRadius.Value;
 PanelD.BorderRadius := SlRoundRadius.Value;
end;

end.
