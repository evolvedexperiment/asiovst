unit SliderTestMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiCommon, DAV_GuiSlider, DAV_GuiPixelMap;

type
  TFmSliderTest = class(TForm)
    GuiEQSlide1: TGuiSlider;
    GuiEQSlide2: TGuiSlider;
    GuiEQSlide3: TGuiSlider;
    GuiEQSlide4: TGuiSlider;
    procedure FormResize(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
  private
    FBackground      : TGuiCustomPixelMap;
    FBackgroundColor : TPixel32;
  public
    procedure RenderBackground;
  end;

var
  FmSliderTest: TFmSliderTest;

implementation

{$R *.dfm}

uses
  DAV_Common;

procedure TFmSliderTest.FormCreate(Sender: TObject);
begin
 FBackground := TGuiPixelMapMemory.Create;

 FBackgroundColor := ConvertColor(Color);
 FBackgroundColor.A := $FF;
end;

procedure TFmSliderTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackground);
end;

procedure TFmSliderTest.FormPaint(Sender: TObject);
begin
 if Assigned(FBackground)
  then FBackground.PaintTo(Canvas);
end;

procedure TFmSliderTest.FormResize(Sender: TObject);
begin
 FBackground.SetSize(ClientWidth, ClientHeight);
 RenderBackground;
end;

procedure TFmSliderTest.RenderBackground;
var
  x, y  : Integer;
  s     : array [0..1] of Single;
  Scale : Single;
  h, hr : Single;
  ScnLn : PPixel32Array;
begin
 with FBackground do
  begin
   s[0] := 0;
   s[1] := 0;
   hr   := 1 / Height;
   for y := 0 to Height - 1 do
    begin
     ScnLn := Scanline[y];
     h    := 0.5 * 0.1 * (1 - Sqr(2 * (y - Height div 2) * hr));
     for x := 0 to Width - 1 do
      begin
       s[1] := 0.97 * s[0] + 0.015 * Random;
       s[0] := s[1];
       Scale := 1 - (s[1] - h);

       ScnLn[x].B := Round(FBackgroundColor.B * Scale);
       ScnLn[x].G := Round(FBackgroundColor.G * Scale);
       ScnLn[x].R := Round(FBackgroundColor.R * Scale);
       ScnLn[x].A := $FF;
      end;
    end;
  end;
end;

end.
