unit SonogramGui;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, Controls, ExtCtrls, Graphics,
  DAV_Types, DAV_VSTModule;

type
  TFmSonogram = class(TForm)
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  end;

implementation

uses
  DAV_GuiCommon, SonogramDM;

{$R *.DFM}

procedure TFmSonogram.FormCreate(Sender: TObject);
var
  x, y   : Integer;
  s      : array[0..1] of Single;
  h, hr  : Single;
  Line   : PRGB24Array;

begin
 // Create Background Image
 FBackgrounBitmap := TBitmap.Create;
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

 ControlStyle := ControlStyle + [csOpaque];
// Sonogram.ControlStyle := Sonogram.ControlStyle + [csOpaque];
end;

procedure TFmSonogram.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmSonogram.TimerTimer(Sender: TObject);
begin
 Invalidate;
end;

procedure TFmSonogram.FormPaint(Sender: TObject);
begin
 with FBackgrounBitmap.Canvas do
  begin
   Draw(8, 8, TSonogramDataModule(Owner).Sonogram.Bitmap);
   Pen.Color := $0070848D;
   Pen.Width := 1;
   Brush.Style := bsClear;
   RoundRect(7, 7, 265, 265, 2, 2);
  end;
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

end.
