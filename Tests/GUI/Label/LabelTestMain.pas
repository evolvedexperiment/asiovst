unit LabelTestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiCommon, DAV_GuiBaseControl, DAV_GuiLabel, StdCtrls;

type
  TFmLabelTest = class(TForm)
    LabelA: TGuiLabel;
    LabelC: TGuiLabel;
    LabelB: TGuiLabel;
    LabelD: TGuiLabel;
    CbTransparent: TCheckBox;
    procedure FormPaint(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure CbTransparentClick(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    { Public-Deklarationen }
  end;

var
  FmLabelTest: TFmLabelTest;

implementation

{$R *.dfm}

procedure TFmLabelTest.FormCreate(Sender: TObject);
begin
 FBackgrounBitmap := TBitmap.Create;
end;

procedure TFmLabelTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmLabelTest.FormResize(Sender: TObject);
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

procedure TFmLabelTest.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmLabelTest.CbTransparentClick(Sender: TObject);
begin
 LabelA.Transparent := CbTransparent.Checked;
 LabelB.Transparent := CbTransparent.Checked;
 LabelC.Transparent := CbTransparent.Checked;
 LabelD.Transparent := CbTransparent.Checked;
end;

end.
