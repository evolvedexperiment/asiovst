unit MBCGUI;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, Graphics, DVSTModule,
  Controls, StdCtrls;

type
  TFmMBC = class(TForm)
    LbAbout1: TLabel;
    LbAbout2: TLabel;
    procedure FormResize(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fBackground : TBitmap;
  public
    MBCDataModule: TVSTModule;
  end;

implementation

{$R *.DFM}

procedure TFmMBC.FormCreate(Sender: TObject);
begin
 fBackground := TBitmap.Create;
end;

procedure TFmMBC.FormDestroy(Sender: TObject);
begin
 fBackground.Free;
end;

procedure TFmMBC.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, fBackground);
end;

procedure TFmMBC.FormResize(Sender: TObject);
var x, y : Integer;
begin
 with fBackground do
  begin
   Width := ClientWidth;
   Height := ClientHeight;
   for x := 0 to Width - 1 do
    for y := 0 to Height - 1 do
     if (x mod 2 = 0) and (y mod 2 = 0)
      then Canvas.Pixels[X, Y] := $A0A0A0
      else Canvas.Pixels[X, Y] := $C0C0C0;
   Canvas.MoveTo(10, 30);
   Canvas.LineTo(Width - 10, 30);
   Canvas.Brush.Color := $D0D0D0;
   Canvas.Pen.Color   := $909090;
   Canvas.Rectangle(20, 40, 251, 181);
   Canvas.Rectangle(260, 40, 326, 181);
   Canvas.Rectangle(335, 40, 401, 181);
   Canvas.Rectangle(410, 40, 521, 181);
   Canvas.Rectangle(530, 40, 641, 181);
   Canvas.Rectangle(650, 40, 761, 181);

   Canvas.Rectangle(20, 190, 261, 300);
   Canvas.Rectangle(270, 190, 511, 300);
   Canvas.Rectangle(520, 190, 761, 300);
  end;
end;

end.