unit GraphXYtestMain;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, DAV_GuiBaseControl, DAV_GuiGraphXY;

type
  TFmGraphXY = class(TForm)
    GraphXYA: TGuiGraphXY;
    GraphXYB: TGuiGraphXY;
    GraphXYC: TGuiGraphXY;
    GraphXYD: TGuiGraphXY;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    FBackgrounBitmap : TBitmap;
  public
    function SimpleFunctionEvaluate(Sender: TObject; X: Double): Double;
  end;

var
  FmGraphXY: TFmGraphXY;

implementation

uses
  DAV_GuiCommon;

{$R *.dfm}

procedure TFmGraphXY.FormCreate(Sender: TObject);
begin
 FBackgrounBitmap := TBitmap.Create;
 TGuiGraphXYFunctionSeries(GraphXYA[0].Series).OnEvaluate := SimpleFunctionEvaluate;
 TGuiGraphXYFunctionSeries(GraphXYB[0].Series).OnEvaluate := SimpleFunctionEvaluate;
 TGuiGraphXYFunctionSeries(GraphXYC[0].Series).OnEvaluate := SimpleFunctionEvaluate;
 TGuiGraphXYFunctionSeries(GraphXYD[0].Series).OnEvaluate := SimpleFunctionEvaluate;
end;

procedure TFmGraphXY.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FBackgrounBitmap);
end;

procedure TFmGraphXY.FormPaint(Sender: TObject);
begin
 Canvas.Draw(0, 0, FBackgrounBitmap);
end;

procedure TFmGraphXY.FormResize(Sender: TObject);
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

function TFmGraphXY.SimpleFunctionEvaluate(Sender: TObject; X: Double): Double;
begin
 Result := X * sqr(X) * 0.1;
end;

end.
