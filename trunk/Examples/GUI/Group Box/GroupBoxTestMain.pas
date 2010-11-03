unit GroupBoxTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, ExtCtrls, DAV_GuiGroup, DAV_GuiPixelMap;

type
  TFmGroupBoxTest = class(TForm)
    CbTransparent: TCheckBox;
    ColorDialog: TColorDialog;
    GroupA: TGuiGroup;
    GroupB: TGuiGroup;
    GroupC: TGuiGroup;
    GroupD: TGuiGroup;
    LbColor: TLabel;
    LbOutlineWidth: TLabel;
    LbRoundRadius: TLabel;
    ShGroupColor: TShape;
    TbOutlineWidth: TTrackBar;
    TbRoundRadius: TTrackBar;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure TbRoundRadiusChange(Sender: TObject);
    procedure TbOutlineWidthChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure ShGroupColorMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
  private
    FBackground : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmGroupBoxTest: TFmGroupBoxTest;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmGroupBoxTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmGroupBoxTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmGroupBoxTest.FormPaint(Sender: TObject);
begin
 if CbTransparent.Checked and Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmGroupBoxTest.FormResize(Sender: TObject);
var
  x, y   : Integer;
  Filter : array [0..1] of Single;
  h, hr  : Single;
  ScnLn  : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   Filter[0] := 0;
   Filter[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       Filter[1] := 0.97 * Filter[0] + 0.03 * Random;
       Filter[0] := Filter[1];

       ScnLn[x].B := Round($70 - $34 * (Filter[1] - h));
       ScnLn[x].G := Round($84 - $48 * (Filter[1] - h));
       ScnLn[x].R := Round($8D - $50 * (Filter[1] - h));
      end;
    end;
  end;
end;

procedure TFmGroupBoxTest.ShGroupColorMouseDown(Sender: TObject;
  Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
 with ColorDialog do
  begin
   Color := ShGroupColor.Brush.Color;
   if Execute then
    begin
     ShGroupColor.Brush.Color := Color;
     GroupA.GroupColor := Color;
     GroupB.GroupColor := Color;
     GroupC.GroupColor := Color;
     GroupD.GroupColor := Color;
    end;
  end;
end;

procedure TFmGroupBoxTest.TbOutlineWidthChange(Sender: TObject);
begin
 GroupA.OutlineWidth := TbOutlineWidth.Position;
 GroupB.OutlineWidth := TbOutlineWidth.Position;
 GroupC.OutlineWidth := TbOutlineWidth.Position;
 GroupD.OutlineWidth := TbOutlineWidth.Position;
end;

procedure TFmGroupBoxTest.TbRoundRadiusChange(Sender: TObject);
begin
 GroupA.Radius := TbRoundRadius.Position;
 GroupB.Radius := TbRoundRadius.Position;
 GroupC.Radius := TbRoundRadius.Position;
 GroupD.Radius := TbRoundRadius.Position;
end;

procedure TFmGroupBoxTest.CbTransparentClick(Sender: TObject);
begin
 GroupA.Transparent := CbTransparent.Checked;
 GroupB.Transparent := CbTransparent.Checked;
 GroupC.Transparent := CbTransparent.Checked;
 GroupD.Transparent := CbTransparent.Checked;
 LbOutlineWidth.Transparent := CbTransparent.Checked;
 LbRoundRadius.Transparent := CbTransparent.Checked;
 Invalidate;
end;

end.
