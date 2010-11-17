unit BtMain;

{$I DAV_Compiler.inc}

interface

uses
  {$IFDEF FPC} LCLIntf, LMessages, {$ELSE} Windows, {$ENDIF} SysUtils, Classes,
  Graphics, Controls, Forms, Dialogs, ExtCtrls, StdCtrls, DAV_GuiPixelMap;

{-$DEFINE DIB}

type
  TFmPixelMapTest = class(TForm)
    BtCountTest: TButton;
    BtPaintTest: TButton;
    BtSave: TButton;
    BtSimpleTest: TButton;
    PaintBox: TPaintBox;
    procedure BtCountTestClick(Sender: TObject);
    procedure BtPaintTestClick(Sender: TObject);
    procedure BtSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  protected
    {$IFDEF DIB}
    FGuiBitmap : TGuiPixelMapDIB;
    {$ELSE}
    FGuiBitmap : TGuiPixelMapMemory;
    {$ENDIF}
  public
    procedure ClearBitmap;
    procedure RenderFrameRectBitmap;
    procedure RenderFillRectBitmap;
    procedure RenderLineBitmap;
    procedure RenderLineCircleBitmap;
  end;

var
  FmPixelMapTest: TFmPixelMapTest;

implementation

uses
  DAV_Common, DAV_Math, DAV_GuiCommon;

{$IFDEF FPC}
{$R *.lfm}
{$ELSE}
{$R *.dfm}
{$ENDIF}

procedure TFmPixelMapTest.FormCreate(Sender: TObject);
begin
 {$IFDEF DIB}
 FGuiBitmap := TGuiPixelMapDIB.Create;
 {$ELSE}
 FGuiBitmap := TGuiPixelMapMemory.Create;
 {$ENDIF}

 with FGuiBitmap do
  begin
   Width := PaintBox.Width;
   Height := PaintBox.Height;
  end;

 PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
 {$IFDEF FPC}
 ControlStyle := ControlStyle + [csOpaque];
 DoubleBuffered := True;
 {$ENDIF}
end;

procedure TFmPixelMapTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FGuiBitmap);
end;

procedure TFmPixelMapTest.PaintBoxPaint(Sender: TObject);
begin
 if Assigned(FGuiBitmap)
  then FGuiBitmap.PaintTo(PaintBox.Canvas);
end;

procedure TFmPixelMapTest.RenderFillRectBitmap;
var
  Color : TPixel32;
  Level : Integer;
  X, Y  : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Color.R := Random($FF);
    Color.G := Random($FF);
    Color.B := Random($FF);
    Color.A := Random($FF);

    X := Random(Width);
    Y := Random(Height);
    FillRect(Rect(X, Y, X + Random(Width - 1 - X), Y + Random(Height - 1 - Y)), Color);
   end;
end;

procedure TFmPixelMapTest.RenderFrameRectBitmap;
var
  Color : TPixel32;
  Level : Integer;
  X, Y  : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Color.R := Random($FF);
    Color.G := Random($FF);
    Color.B := Random($FF);
    Color.A := Random($FF);

    X := Random(Width);
    Y := Random(Height);
    FrameRect(Rect(X, Y, X + Random(Width - X), Y + Random(Height - Y)), Color);
   end;
end;

procedure TFmPixelMapTest.RenderLineBitmap;
var
  Color : TPixel32;
  Level : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Color.R := Random($FF);
    Color.G := Random($FF);
    Color.B := Random($FF);
    Color.A := Random($FF);

    Line(Random(Width), Random(Height), Random(Width), Random(Height), Color);
   end;
end;

procedure TFmPixelMapTest.RenderLineCircleBitmap;
var
  Color  : TPixel32;
  Level  : Integer;
  Index  : Integer;
  Center : TPoint;
  X, Y   : Single;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Center.X := Width div 2;
    Center.Y := Height div 2;
    for Index := 0 to 35 do
     begin
      Color.R := Random($FF);
      Color.G := Random($FF);
      Color.B := Random($FF);
      Color.A := Random($FF);

      GetSinCos(Pi * Index / 18, X, Y);
      Line(Center.X, Center.Y, Round(Center.X * (1 + 0.9 * X )),
        Round(Center.Y * (1 + 0.9 * Y)), Color);
     end;
   end;
end;

procedure TFmPixelMapTest.BtCountTestClick(Sender: TObject);
var
  i : Integer;
begin
 i := 0;
 while i < 1 shl 16 do
  try
   with TGuiPixelMapMemory.Create do
    begin
     Width := 100;
     Height := 100;
    end;
   Inc(i);
  except
   raise Exception.CreateFmt('Only %d bitmaps created', [i]);
  end;
end;

procedure TFmPixelMapTest.BtPaintTestClick(Sender: TObject);
begin
 ClearBitmap;
 RenderLineCircleBitmap;
 RenderFillRectBitmap;
 RenderFrameRectBitmap;
 PaintBox.Invalidate;
end;

procedure TFmPixelMapTest.BtSaveClick(Sender: TObject);
begin
 RenderLineCircleBitmap;
 FGuiBitmap.SaveToFile('Test.bmp');
end;

procedure TFmPixelMapTest.ClearBitmap;
begin
 FGuiBitmap.Clear;
end;

end.
