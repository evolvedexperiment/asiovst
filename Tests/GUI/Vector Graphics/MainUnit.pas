unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls, ExtCtrls, SyncObjs, AppEvnts, DAV_GuiPixelMap, DAV_GuiVector,
  DAV_GuiFixedPoint, DAV_GuiVectorPixel, DAV_GuiVectorPixelLine,
  DAV_GuiVectorPixelCircle;

type
  TFmVectorGraphicTest = class(TForm)
    LbTestType: TLabel;
    PaintBox: TPaintBox;
    CbTestType: TComboBox;
    CbDraft: TCheckBox;
    ApplicationEvents: TApplicationEvents;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
    procedure FormDblClick(Sender: TObject);
    procedure FormClick(Sender: TObject);
    procedure CbTestTypeChange(Sender: TObject);
    procedure CbDraftClick(Sender: TObject);
    procedure ApplicationEventsIdle(Sender: TObject; var Done: Boolean);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
  private
    FPixelMap        : TGuiPixelMapMemory;
    FPrimitiveClass  : TCustomGuiPixelPrimitiveClass;
    FPrimitives      : array of TCustomGuiPixelPrimitive;
    FCriticalSection : TCriticalSection;
    FIniFileName     : TFileName;
  protected
    procedure BuildRandomPrimitives;
  public
    procedure Render;
  end;

var
  FmVectorGraphicTest: TFmVectorGraphicTest;

implementation

{$R *.dfm}

uses
  Math, IniFiles;

procedure TFmVectorGraphicTest.FormCreate(Sender: TObject);
begin
 FPixelMap := TGuiPixelMapMemory.Create;
 FPrimitiveClass := TGuiPixelFilledRectangle;
 FIniFileName := ExtractFilePath(ParamStr(0)) + 'VectorGraphicText.ini';
 ControlStyle := ControlStyle + [csOpaque];
 FCriticalSection := TCriticalSection.Create;
 Randomize;
end;

procedure TFmVectorGraphicTest.FormDestroy(Sender: TObject);
begin
 FreeAndNil(FPixelMap);
 FreeAndNil(FCriticalSection)
end;

procedure TFmVectorGraphicTest.FormPaint(Sender: TObject);
begin
 FCriticalSection.Enter;
 try
  if Assigned(FPixelMap)
   then FPixelMap.PaintTo(PaintBox.Canvas);
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TFmVectorGraphicTest.FormResize(Sender: TObject);
begin
 if Assigned(FPixelMap) then
  begin
   FPixelMap.SetSize(PaintBox.Width, PaintBox.Height);
   BuildRandomPrimitives;
   Render;
  end;
end;

procedure TFmVectorGraphicTest.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FIniFileName) do
  begin
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);
   CbTestType.ItemIndex := CbTestType.Items.IndexOf(
     ReadString('Recent', 'Primitive', CbTestType.Text));
   CbTestTypeChange(Self);
   CbDraft.Checked := ReadBool('Recent', 'Draft', CbDraft.Checked);
  end;
end;

procedure TFmVectorGraphicTest.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFileName) do
  begin
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
   WriteString('Recent', 'Primitive', CbTestType.Text);
   WriteBool('Recent', 'Draft', CbDraft.Checked);
  end;
end;

procedure TFmVectorGraphicTest.FormClick(Sender: TObject);
begin
 ApplicationEvents.OnIdle := nil;
 BuildRandomPrimitives;
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.FormDblClick(Sender: TObject);
begin
 ApplicationEvents.OnIdle := ApplicationEventsIdle;
end;

procedure TFmVectorGraphicTest.CbDraftClick(Sender: TObject);
begin
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.CbTestTypeChange(Sender: TObject);
const
  CPrimitiveClasses : array [0..6] of TCustomGuiPixelPrimitiveClass = (
    TGuiPixelFilledRectangle, TGuiPixelFilledCircle, TGuiPixelFilledEllipse,
    TGuiPixelFrameRectangle, TGuiPixelFrameCircle, TGuiPixelThinLine,
    TGuiPixelLine);
begin
 Assert(CbTestType.Items.Count = Length(CPrimitiveClasses));
 FPrimitiveClass := CPrimitiveClasses[CbTestType.ItemIndex];
 BuildRandomPrimitives;
 FPixelMap.Clear;
 Render;
end;

procedure TFmVectorGraphicTest.ApplicationEventsIdle(Sender: TObject;
  var Done: Boolean);
begin
 BuildRandomPrimitives;
 FPixelMap.Clear;
 Render;
 Done := False;
end;

procedure TFmVectorGraphicTest.BuildRandomPrimitives;
var
  Index : Integer;
begin
 for Index := 0 to Length(FPrimitives) - 1 do
  if Assigned(FPrimitives[Index])
   then FreeAndNil(FPrimitives[Index]);

 SetLength(FPrimitives, 10);

 for Index := 0 to Length(FPrimitives) - 1 do
  begin
   FPrimitives[Index] := FPrimitiveClass.Create;

   // set color and alpha value
   if FPrimitives[Index] is TCustomGuiPixelSimplePrimitive then
    with TCustomGuiPixelSimplePrimitive(FPrimitives[Index]) do
     begin
      Color := Random($7FFF);
      Alpha := Random($FF);
     end;

   with FPrimitives[Index] do
    if GeometricShape is TGuiCircle then
     with FPixelMap, TGuiCircle(GeometricShape) do
      begin
       CenterX := ConvertToFixed24Dot8Point(Random(Width - 1) + Random);
       CenterY := ConvertToFixed24Dot8Point(Random(Height - 1) + Random);
       Radius := ConvertToFixed24Dot8Point(Random(Width div 4) + Random);
      end else
    if GeometricShape is TGuiEllipse then
     with FPixelMap, TGuiEllipse(GeometricShape) do
      begin
       CenterX := ConvertToFixed24Dot8Point(Random(Width - 1) + Random);
       CenterY := ConvertToFixed24Dot8Point(Random(Height - 1) + Random);
       RadiusX := ConvertToFixed24Dot8Point(Random(Width div 4) + Random);
       RadiusY := ConvertToFixed24Dot8Point(Random(Height div 4) + Random);
      end else
    if GeometricShape is TGuiRectangle then
     with FPixelMap, TGuiRectangle(GeometricShape) do
      begin
       Left := ConvertToFixed24Dot8Point(Random(2 * (FPixelMap.Width - 1)) - FPixelMap.Width div 2 + Random);
       Top := ConvertToFixed24Dot8Point(Random(2 * (FPixelMap.Height - 1)) - FPixelMap.Height div 2 + Random);
       Right := FixedAdd(Left, ConvertToFixed24Dot8Point(Random(FPixelMap.Width - 1)));
       Bottom := FixedAdd(Top, ConvertToFixed24Dot8Point(Random(FPixelMap.Height - 1)));
      end else
    if GeometricShape is TGuiLine then
     with FPixelMap, TGuiLine(GeometricShape) do
      begin
       XA := ConvertToFixed24Dot8Point(Random(2 * (Width - 1)) - Width div 2 + Random);
       YA := ConvertToFixed24Dot8Point(Random(2 * (Height - 1)) - Height div 2 + Random);
       XB := ConvertToFixed24Dot8Point(Random(2 * (Width - 1)) - Width div 2 + Random);
       YB := ConvertToFixed24Dot8Point(Random(2 * (Height - 1)) - Height div 2 + Random);
      end;
  end;
end;

procedure TFmVectorGraphicTest.Render;
var
  Index : Integer;
begin
 FCriticalSection.Enter;
 try
  if CbDraft.Checked then
   for Index := 0 to Length(FPrimitives) - 1
    do FPrimitives[Index].DrawDraft(FPixelMap)
  else
   for Index := 0 to Length(FPrimitives) - 1
    do FPrimitives[Index].Draw(FPixelMap);
  Invalidate;
 finally
  FCriticalSection.Leave;
 end;
end;

end.
