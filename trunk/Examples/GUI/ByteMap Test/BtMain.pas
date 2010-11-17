unit BtMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ExtCtrls, StdCtrls, DAV_GuiByteMap;

{$DEFINE DIB}

type
  TFmBytemapTest = class(TForm)
    BtCountTest: TButton;
    BtSimpleTest: TButton;
    PaintBox: TPaintBox;
    BtPaintTest: TButton;
    BtSave: TButton;
    procedure BtCountTestClick(Sender: TObject);
    procedure BtPaintTestClick(Sender: TObject);
    procedure BtSaveClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure PaintBoxPaint(Sender: TObject);
  protected
    {$IFDEF DIB}
    FGuiBitmap : TGuiByteMapDIB;
    {$ELSE}
    FGuiBitmap : TGuiByteMapMemory;
    {$ENDIF}
  public
    procedure ClearBitmap;
    procedure RenderFrameRectBitmap;
    procedure RenderFillRectBitmap;
    procedure RenderLineBitmap;
    procedure RenderLineCircleBitmap;
  end;

var
  FmBytemapTest: TFmBytemapTest;

implementation

uses
  DAV_Common, DAV_Math, DAV_GuiCommon;

{$R *.dfm}

procedure TFmBytemapTest.FormCreate(Sender: TObject);
begin
 {$IFDEF DIB}
 FGuiBitmap := TGuiByteMapDIB.Create;
 {$ELSE}
 FGuiBitmap := TGuiByteMapMemory.Create;
 {$ENDIF}

 with FGuiBitmap do
  begin
   Width := PaintBox.Width;
   Height := PaintBox.Height;
  end;

 PaintBox.ControlStyle := PaintBox.ControlStyle + [csOpaque];
end;

procedure TFmBytemapTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FGuiBitmap);
end;

procedure TFmBytemapTest.PaintBoxPaint(Sender: TObject);
begin
 FGuiBitmap.PaintTo(PaintBox.Canvas);
end;

procedure TFmBytemapTest.RenderFillRectBitmap;
var
  Data  : Byte;
  Level : Integer;
  X, Y  : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Data := Random($FF);
    X := Random(Width);
    Y := Random(Height);
    FillRect(Rect(X, Y, X + Random(Width - 1 - X), Y + Random(Height - 1 - Y)), Data);
   end;
end;

procedure TFmBytemapTest.RenderFrameRectBitmap;
var
  Data  : Byte;
  Level : Integer;
  X, Y  : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Data := Random($FF);
    X := Random(Width);
    Y := Random(Height);
    FrameRect(Rect(X, Y, X + Random(Width - X), Y + Random(Height - Y)), Data);
   end;
end;

procedure TFmBytemapTest.RenderLineBitmap;
var
  Data  : Byte;
  Level : Integer;
begin
 with FGuiBitmap do
  for Level := 0 to 10 do
   begin
    Data := Random($FF);
    Line(Random(Width), Random(Height), Random(Width), Random(Height), Data);
   end;
end;

procedure TFmBytemapTest.RenderLineCircleBitmap;
var
  Data   : Byte;
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
      Data := Random($FF);
      GetSinCos(Pi * Index / 18, X, Y);
      Line(Center.X, Center.Y, Round(Center.X * (1 + 0.9 * X )),
        Round(Center.Y * (1 + 0.9 * Y)), Data);
     end;
   end;
end;

procedure TFmBytemapTest.BtCountTestClick(Sender: TObject);
var
  i : Integer;
begin
 i := 0;
 while i < 1 shl 16 do
  try
   with TGuiByteMapMemory.Create do
    begin
     Width := 100;
     Height := 100;
    end;
   Inc(i);
  except
   raise Exception.CreateFmt('Only %d bitmaps created', [i]);
  end;
end;

procedure TFmBytemapTest.BtPaintTestClick(Sender: TObject);
begin
 ClearBitmap;
 RenderLineCircleBitmap;
// RenderFillRectBitmap;
// RenderFrameRectBitmap;
 PaintBox.Invalidate;
end;

procedure TFmBytemapTest.BtSaveClick(Sender: TObject);
begin
 RenderLineCircleBitmap;
 FGuiBitmap.SaveToFile('Test.bmp');
end;

procedure TFmBytemapTest.ClearBitmap;
begin
 FGuiBitmap.Clear;
end;

end.
