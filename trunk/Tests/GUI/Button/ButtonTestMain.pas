unit ButtonTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ComCtrls, DAV_GuiBaseControl, DAV_GuiButton, DAV_GuiPixelMap;

type
  TFmButton = class(TForm)
    ButtonA: TGuiButton;
    ButtonB: TGuiButton;
    ButtonC: TGuiButton;
    ButtonD: TGuiButton;
    CbTransparent: TCheckBox;
    TbBorderWidth: TTrackBar;
    TbRadius: TTrackBar;
    LbRadius: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbBorderWidthChange(Sender: TObject);
    procedure TbRadiusChange(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  end;

var
  FmButton: TFmButton;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmButton.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmButton.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmButton.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmButton.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array [0..1] of Single;
  h, hr  : Single;
  ScnLne : PPixel32Array;
begin
 with FBackground do
  begin
   SetSize(ClientWidth, ClientHeight);
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLne := Scanline[y];
     h    := 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * Random;
       s[0] := s[1];

       ScnLne[x].B := Round($70 - $34 * (s[1] - h));
       ScnLne[x].G := Round($84 - $48 * (s[1] - h));
       ScnLne[x].R := Round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TFmButton.TbBorderWidthChange(Sender: TObject);
begin
 ButtonA.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
 ButtonB.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
 ButtonC.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
 ButtonD.BorderWidth := 0.25 *(3 + TbBorderWidth.Position);
end;

procedure TFmButton.CbTransparentClick(Sender: TObject);
begin
 ButtonA.Transparent := CbTransparent.Checked;
 ButtonB.Transparent := CbTransparent.Checked;
 ButtonC.Transparent := CbTransparent.Checked;
 ButtonD.Transparent := CbTransparent.Checked;
end;

procedure TFmButton.TbRadiusChange(Sender: TObject);
begin
 ButtonA.Radius := 0.5 * TbRadius.Position;
 ButtonB.Radius := 0.5 * TbRadius.Position;
 ButtonC.Radius := 0.5 * TbRadius.Position;
 ButtonD.Radius := 0.5 * TbRadius.Position;
end;

end.
