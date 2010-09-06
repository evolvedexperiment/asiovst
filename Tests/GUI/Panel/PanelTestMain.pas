unit PanelTestMain;

{$I DAV_Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiPanel;

type
  TFmPanelTest = class(TForm)
    CbTransparent: TCheckBox;
    LbLineWidth: TLabel;
    LbRoundRadius: TLabel;
    PanelA: TGuiPanel;
    PanelB: TGuiPanel;
    PanelC: TGuiPanel;
    PanelD: TGuiPanel;
    TbLineWidth: TTrackBar;
    TbRoundRadius: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbLineWidthChange(Sender: TObject);
    procedure TbRoundRadiusChange(Sender: TObject);
  private
    FBackgroundBitmap : TGuiPixelMapMemory;
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
 FBackgroundBitmap := TGuiPixelMapMemory.Create;
end;

procedure TFmPanelTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgroundBitmap);
end;

procedure TFmPanelTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackgroundBitmap)
  then FBackgroundBitmap.PaintTo(Canvas);
end;

procedure TFmPanelTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLne : PPixel32Array;
begin
 with FBackgroundBitmap do
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

procedure TFmPanelTest.CbTransparentClick(Sender: TObject);
begin
 PanelA.Transparent := CbTransparent.Checked;
 PanelB.Transparent := CbTransparent.Checked;
 PanelC.Transparent := CbTransparent.Checked;
 PanelD.Transparent := CbTransparent.Checked;
end;

procedure TFmPanelTest.TbLineWidthChange(Sender: TObject);
begin
 PanelA.Borderwidth := 0.25 * TbLinewidth.Position;
 PanelB.Borderwidth := 0.25 * TbLinewidth.Position;
 PanelC.Borderwidth := 0.25 * TbLinewidth.Position;
 PanelD.Borderwidth := 0.25 * TbLinewidth.Position;
end;

procedure TFmPanelTest.TbRoundRadiusChange(Sender: TObject);
begin
 PanelA.Radius := TbRoundRadius.Position;
 PanelB.Radius := TbRoundRadius.Position;
 PanelC.Radius := TbRoundRadius.Position;
 PanelD.Radius := TbRoundRadius.Position;
end;

end.
