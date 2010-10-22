unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  DAV_GuiGraphicControl, DAV_GuiCommon, DAV_GuiPixelMap;

type
  TFmPaintBoxTest = class(TForm)
    GuiPaintBox: TGuiPaintBox;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackground : TGuiCustomPixelMap;
  public
    { Public-Deklarationen }
  end;

var
  FmPaintBoxTest: TFmPaintBoxTest;

implementation

{$R *.dfm}

procedure TFmPaintBoxTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;
end;

procedure TFmPaintBoxTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmPaintBoxTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmPaintBoxTest.FormResize(Sender: TObject);
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

end.

