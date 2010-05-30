unit SelectBoxTest;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, ComCtrls, StdCtrls, DAV_GuiBaseControl, DAV_GuiSelectBox;

type
  TForm1 = class(TForm)
    SelectBoxA: TGuiSelectBox;
    SelectBoxB: TGuiSelectBox;
    SelectBoxC: TGuiSelectBox;
    SelectBoxD: TGuiSelectBox;
    Label1: TLabel;
    TbRoundRadius: TTrackBar;
    CbTransparent: TCheckBox;
    Label2: TLabel;
    TbArrowWidth: TTrackBar;
    procedure TbRoundRadiusChange(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
    procedure TbArrowWidthChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    { Public-Deklarationen }
  end;

var
  Form1: TForm1;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TForm1.CbTransparentClick(Sender: TObject);
begin
 SelectBoxA.Transparent := CbTransparent.Checked;
 SelectBoxB.Transparent := CbTransparent.Checked;
 SelectBoxC.Transparent := CbTransparent.Checked;
 SelectBoxD.Transparent := CbTransparent.Checked;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
 FBackgrounBitmap := TBitmap.Create;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TForm1.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TForm1.FormResize(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;
begin
 with FBackgrounBitmap do
  begin
   PixelFormat := pf24bit;
   Width := Self.Width;
   Height := Self.Height;
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     Line := Scanline[y];
     h    := 0.1 * (1 - sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.03 * random;
       s[0] := s[1];

       Line[x].B := round($70 - $34 * (s[1] - h));
       Line[x].G := round($84 - $48 * (s[1] - h));
       Line[x].R := round($8D - $50 * (s[1] - h));
      end;
    end;
  end;
end;

procedure TForm1.TbArrowWidthChange(Sender: TObject);
begin
 SelectBoxA.ArrowWidth := TbArrowWidth.Position;
 SelectBoxB.ArrowWidth := TbArrowWidth.Position;
 SelectBoxC.ArrowWidth := TbArrowWidth.Position;
 SelectBoxD.ArrowWidth := TbArrowWidth.Position;
end;

procedure TForm1.TbRoundRadiusChange(Sender: TObject);
begin
 SelectBoxA.Radius := TbRoundRadius.Position;
 SelectBoxB.Radius := TbRoundRadius.Position;
 SelectBoxC.Radius := TbRoundRadius.Position;
 SelectBoxD.Radius := TbRoundRadius.Position;
end;

end.
