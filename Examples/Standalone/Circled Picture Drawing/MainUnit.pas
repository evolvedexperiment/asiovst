unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, Menus, ExtCtrls, StdCtrls, DAV_Common, DAV_Types, DAV_ChunkClasses,
  DAV_GuiCommon, DAV_DifferentialEvolution, DAV_GuiPixelMap, DAV_GuiFixedPoint,
  DAV_GuiVector, DAV_GuiVectorPixel, DAV_GuiVectorPixelCircle;

const
  CTrialsPerCircle = 10000;

type
  TCircleChunk = class(TDefinedChunk)
  protected
    FAlpha   : Byte;
    FCenterX : TFixed24Dot8Point;
    FCenterY : TFixed24Dot8Point;
    FColor   : TColor;
    FRadius  : TFixed24Dot8Point;
  public
    constructor Create; override;
    procedure LoadFromStream(Stream : TStream); override;
    procedure SaveToStream(Stream : TStream); override;
    class function GetClassChunkName: TChunkName; override;

    property Radius: TFixed24Dot8Point read FRadius write FRadius;
    property CenterX: TFixed24Dot8Point read FCenterX write FCenterX;
    property CenterY: TFixed24Dot8Point read FCenterY write FCenterY;
    property Color: TColor read FColor write FColor;
    property Alpha: Byte read FAlpha write FAlpha;
  end;

  TCircleChunkContainer = class(TChunkContainer)
  private
    function GetCircle(Index: Integer): TCircleChunk;
  public
    constructor Create; override;
    class function GetClassChunkName: TChunkName; override;

    property Circle[Index: Integer]: TCircleChunk read GetCircle; default;
  end;

  TEvolutionThread = class(TThread)
  protected
    procedure Execute; override;
  end;

  TFmCircledPictureDialog = class(TForm)
    LbCosts: TLabel;
    MainMenu: TMainMenu;
    MiEvolve: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiOpenReference: TMenuItem;
    MiSaveDrawing: TMenuItem;
    MiStart: TMenuItem;
    MiStopContinue: TMenuItem;
    MiNext: TMenuItem;
    N1: TMenuItem;
    OpenDialog: TOpenDialog;
    PaintBoxDraw: TPaintBox;
    PaintBoxRef: TPaintBox;
    SaveDialog: TSaveDialog;
    N2: TMenuItem;
    LbTrialCount: TLabel;
    MiOpenDrawing: TMenuItem;
    MiSaveResult: TMenuItem;
    N3: TMenuItem;
    OpenDialogCircles: TOpenDialog;
    SaveDialogCircles: TSaveDialog;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiOpenReferenceClick(Sender: TObject);
    procedure MiStopContinueClick(Sender: TObject);
    procedure MiStartClick(Sender: TObject);
    procedure MiNextClick(Sender: TObject);
    procedure PaintBoxRefPaint(Sender: TObject);
    procedure PaintBoxDrawPaint(Sender: TObject);
    procedure MiOpenDrawingClick(Sender: TObject);
    procedure MiSaveDrawingClick(Sender: TObject);
    procedure MiSaveResultClick(Sender: TObject);
  private
    FReference     : TGuiCustomPixelMap;
    FDrawing       : TGuiCustomPixelMap;
    FNewDrawing    : TGuiCustomPixelMap;
    FBestDrawing   : TGuiCustomPixelMap;
    FIniFileName   : TFileName;
    FDiffEvol      : TDifferentialEvolution;
    FEvolution     : TEvolutionThread;
    FCircles       : array of TGuiPixelFilledCircle;
    FCurrentCircle : Integer;
    FWorstCost     : Double;
    FTrialCount    : Integer;
    procedure DrawPopulation(Population: TDifferentialEvolutionPopulation;
      PixelMap: TGuiCustomPixelMap);
    procedure SaveDrawing(FileName: TFileName);
    procedure LoadDrawing(FileName: TFileName);
  public
    procedure LoadReference(FileName: TFileName);
    function CalculateError(Sender: TObject; const Population: TDifferentialEvolutionPopulation): Double;
    procedure Evolve;
    procedure InitializeEvolution;
    procedure DrawResults;

    property DifferentialEvolution: TDifferentialEvolution read FDiffEvol;
    property EvolutionThread: TEvolutionThread read FEvolution;
  end;

var
  FmCircledPictureDialog: TFmCircledPictureDialog;

implementation

{$R *.dfm}

uses
  Math, Registry;

{ TCircleChunk }

constructor TCircleChunk.Create;
begin
 inherited;
 FChunkSize := 3 * SizeOf(TFixed24Dot8Point) + SizeOf(TColor) + SizeOf(Byte);
end;

class function TCircleChunk.GetClassChunkName: TChunkName;
begin
 Result := 'circ';
end;

procedure TCircleChunk.LoadFromStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Assert(Stream.Size >= FChunkSize);
   Read(FRadius, SizeOf(TFixed24Dot8Point));
   Read(FCenterX, SizeOf(TFixed24Dot8Point));
   Read(FCenterY, SizeOf(TFixed24Dot8Point));
   Read(FAlpha, SizeOf(Byte));
   Read(FColor, SizeOf(TColor));
  end;
end;

procedure TCircleChunk.SaveToStream(Stream: TStream);
begin
 inherited;

 with Stream do
  begin
   Write(FRadius, SizeOf(TFixed24Dot8Point));
   Write(FCenterX, SizeOf(TFixed24Dot8Point));
   Write(FCenterY, SizeOf(TFixed24Dot8Point));
   Write(FAlpha, SizeOf(Byte));
   Write(FColor, SizeOf(TColor));
  end;
end;


{ TCircleChunkContainer }

constructor TCircleChunkContainer.Create;
begin
 RegisterChunkClass(TCircleChunk);
 inherited;
end;


{ TEvolution }

procedure TEvolutionThread.Execute;
var
  Count : Integer;
begin
 Count := 0;
 while not Terminated do
  begin
   FmCircledPictureDialog.Evolve;
   Inc(Count);

   if Count >= 100 then
    begin
     Synchronize(FmCircledPictureDialog.DrawResults);
     Count := 0;
     Sleep(10);
    end;
  end;
end;

function TCircleChunkContainer.GetCircle(Index: Integer): TCircleChunk;
begin
 if (Index >= 0) and (Index < Count) and
  (FChunkList[Index] is TCircleChunk)
  then Result := TCircleChunk(FChunkList[Index])
  else Result := nil;
end;

class function TCircleChunkContainer.GetClassChunkName: TChunkName;
begin
 Result := 'Crcl';
end;


{ TFmCircledPictureDialog }

procedure TFmCircledPictureDialog.FormCreate(Sender: TObject);
begin
 Randomize;
 FIniFileName := 'CPD.ini';

 FReference := TGuiPixelMapMemory.Create;
 FDrawing := TGuiPixelMapMemory.Create;
 FNewDrawing := TGuiPixelMapMemory.Create;
 FBestDrawing := TGuiPixelMapMemory.Create;
 FDiffEvol := TDifferentialEvolution.Create(Self);
 FEvolution := TEvolutionThread.Create(True);

 PaintBoxRef.ControlStyle := PaintBoxDraw.ControlStyle + [csOpaque];
 PaintBoxDraw.ControlStyle := PaintBoxDraw.ControlStyle + [csOpaque];

 with FDiffEvol do
  begin
   PopulationCount := 1000;
   VariableCount := 7;
   CrossOver := 0.5;
   OnCalcCosts := CalculateError;
  end;
end;

procedure TFmCircledPictureDialog.FormDestroy(Sender: TObject);
begin
 if Assigned(FEvolution) then
  begin
   FEvolution.Terminate;
   if FEvolution.Suspended
    then FEvolution.Suspended := False;
   FEvolution.WaitFor;
   FreeAndNil(FEvolution);
  end;

 FreeAndNil(FReference);
 FreeAndNil(FDrawing);
 FreeAndNil(FNewDrawing);
 FreeAndNil(FBestDrawing);
 FreeAndNil(FDiffEvol);
end;

procedure TFmCircledPictureDialog.FormShow(Sender: TObject);
begin
 with TRegIniFile.Create(FIniFileName) do
  try
   Left := ReadInteger('Program', 'Left', Left);
   Top := ReadInteger('Program', 'Top', Top);
   LoadReference(ReadString('Recent', 'Reference', ''));
  finally
   Free;
  end;
end;

procedure TFmCircledPictureDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 with TRegIniFile.Create(FIniFileName) do
  try
   WriteInteger('Program', 'Left', Left);
   WriteInteger('Program', 'Top', Top);
  finally
   Free;
  end;
end;

function TFmCircledPictureDialog.CalculateError(Sender: TObject;
  const Population: TDifferentialEvolutionPopulation): Double;
var
  PixelData : array [0..1] of PPixel32Array;
  Index     : Integer;
const
  COne255th : Double = 1 / 255;
begin
 Assert(FReference <> nil);
 Assert(FDrawing <> nil);
 Assert(FNewDrawing <> nil);
 Assert(FBestDrawing <> nil);

 if (Population[0] < -Width) or
    (Population[0] > 2 * Width) or
    ((Population[0] < 0) and (Population[2] < -Population[0])) or
    ((Population[0] > Width) and (Population[2] < (Population[0] - Width))) or
    (Population[1] < -Height) or
    (Population[1] > 2 * Height) or
    ((Population[1] < 0) and (Population[2] < -Population[1])) or
    ((Population[1] > Height) and (Population[2] < (Population[1] - Height))) or
    (Population[2] < 1) or
    (Population[2] > Max(Width, Height)) or
    (Population[3] < 0) or (Population[3] > 1) or
    (Population[4] < 0) or (Population[4] > 1) or
    (Population[5] < 0) or (Population[5] > 1) or
    (Population[6] < 0) or (Population[6] > 1) then
  begin
   Result := FWorstCost;
   Exit;
  end;

 Result := 0;
 with FNewDrawing do
  begin
   Assert(FReference.Width * FReference.Height = Width * Height);
   Assert(FDrawing.Width * FDrawing.Height = Width * Height);
   Assert(FNewDrawing.Width * FNewDrawing.Height = Width * Height);

   DrawPopulation(Population, FNewDrawing);

   PixelData[0] := FReference.DataPointer;
   PixelData[1] := DataPointer;
   for Index := 0 to Width * Height - 1 do
    begin
     Result := Result +
      (Abs(PixelData[0, Index].B - PixelData[1, Index].B) +
       Abs(PixelData[0, Index].G - PixelData[1, Index].G) +
       Abs(PixelData[0, Index].R - PixelData[1, Index].R)) * COne255th;
    end;
  end;
 Result := 10 * Log2(Result);
end;

procedure TFmCircledPictureDialog.DrawPopulation(
  Population: TDifferentialEvolutionPopulation; PixelMap: TGuiCustomPixelMap);
begin
 // copy recent drawing
 with PixelMap
  do Move(FDrawing.DataPointer^, DataPointer^, Width * Height * SizeOf(TPixel32));

 with FCircles[FCurrentCircle] do
  begin
   GeometricShape.CenterX := ConvertToFixed24Dot8Point(Population[0]);
   GeometricShape.CenterY := ConvertToFixed24Dot8Point(Population[1]);
   GeometricShape.Radius := ConvertToFixed24Dot8Point(Population[2]);
   Color := Round($FF * Population[3]) shl 16 +
     Round($FF * Population[4]) shl 8 + Round($FF * Population[5]);
   Alpha := Round($FF * Population[6]);

   Draw(PixelMap);
//   DrawDraft(PixelMap);
  end;
end;

procedure TFmCircledPictureDialog.DrawResults;
begin
 DrawPopulation(FDiffEvol.GetBestPopulation, FBestDrawing);
 LbCosts.Caption := 'Costs: ' + FloatToStrF(FDiffEvol.GetBestCost, ffGeneral, 5, 5);
 LbTrialCount.Caption := 'Trial: ' + IntToStr(FTrialCount);
 PaintBoxDraw.Invalidate;
end;

procedure TFmCircledPictureDialog.Evolve;
begin
 if MiNext.Checked or (FTrialCount >= CTrialsPerCircle) then
  begin
   DrawPopulation(FDiffEvol.GetBestPopulation, FDrawing);
   SetLength(FCircles, Length(FCircles) + 1);
   FCircles[Length(FCircles) - 1] := TGuiPixelFilledCircle.Create;

   InitializeEvolution;
   MiNext.Checked := False;
  end;
 Inc(FTrialCount);
 DifferentialEvolution.Evolve;
end;

procedure TFmCircledPictureDialog.LoadReference(FileName: TFileName);
begin
 if not FileExists(FileName) then Exit;

 FReference.LoadFromFile(FileName);
 with FDrawing do
  begin
   Width := FReference.Width;
   Height := FReference.Height;
   Clear;
  end;

 with FNewDrawing do
  begin
   Width := FReference.Width;
   Height := FReference.Height;
   Clear;
  end;

 with FBestDrawing do
  begin
   Width := FReference.Width;
   Height := FReference.Height;
   Clear;
  end;

 PaintBoxRef.Width := FReference.Width;
 PaintBoxRef.Height := FReference.Height;
 PaintBoxDraw.Width := FReference.Width;
 PaintBoxDraw.Height := FReference.Height;
 PaintBoxDraw.Left := PaintBoxRef.Left + PaintBoxRef.Width + 8;
 ClientWidth := 2 * FReference.Width + 24;
 ClientHeight := FReference.Height + 16;

 PaintBoxRef.Invalidate;
 PaintBoxDraw.Invalidate;
end;

procedure TFmCircledPictureDialog.SaveDrawing(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Circle  : TCircleChunk;
  Index   : Integer;
begin
 try
  Circles := TCircleChunkContainer.Create;
  Circle  := TCircleChunk.Create;

  with FCircles[FCurrentCircle] do
   begin
    GeometricShape.CenterX := ConvertToFixed24Dot8Point(FDiffEvol.GetBestPopulation[0]);
    GeometricShape.CenterY := ConvertToFixed24Dot8Point(FDiffEvol.GetBestPopulation[1]);
    GeometricShape.Radius := ConvertToFixed24Dot8Point(FDiffEvol.GetBestPopulation[2]);
    Color := Round($FF * FDiffEvol.GetBestPopulation[3]) shl 16 +
      Round($FF * FDiffEvol.GetBestPopulation[4]) shl 8 +
      Round($FF * FDiffEvol.GetBestPopulation[5]);
    Alpha := Round($FF * FDiffEvol.GetBestPopulation[6]);
   end;

  for Index := 0 to Length(FCircles) - 1 do
   begin
    Circle.Alpha := FCircles[Index].Alpha;
    Circle.Color := FCircles[Index].Color;
    Circle.Radius := FCircles[Index].GeometricShape.Radius;
    Circle.CenterX := FCircles[Index].GeometricShape.CenterX;
    Circle.CenterY := FCircles[Index].GeometricShape.CenterY;
    Circles.AddChunk(Circle);
   end;

  Circles.SaveToFile(FileName);
 finally
  if Assigned(Circles) then FreeAndNil(Circles);
 end;
end;

procedure TFmCircledPictureDialog.LoadDrawing(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Index   : Integer;
begin
 try
  Circles := TCircleChunkContainer.Create;
  Circles.LoadFromFile(FileName);

  FDrawing.Clear;
  FCurrentCircle := Circles.Count;
  SetLength(FCircles, Circles.Count + 1);
  for Index := 0 to Circles.Count - 1 do
   begin
    // eventually create
    if not Assigned(FCircles[Index])
     then FCircles[Index] := TGuiPixelFilledCircle.Create;

    Assert(Circles[Index] <> nil);

    with FCircles[Index] do
     begin
      Alpha := Circles[Index].Alpha;
      Color := Circles[Index].Color;
      GeometricShape.Radius := Circles[Index].Radius;
      GeometricShape.CenterX := Circles[Index].CenterX;
      GeometricShape.CenterY := Circles[Index].CenterY;
      Draw(FDrawing);
     end;
   end;

  // eventually create current circle
  if not Assigned(FCircles[FCurrentCircle])
   then FCircles[FCurrentCircle] := TGuiPixelFilledCircle.Create;

  FBestDrawing.Assign(FDrawing);
  PaintBoxDraw.Invalidate;
 finally
  if Assigned(Circles) then FreeAndNil(Circles);
 end;
end;

procedure TFmCircledPictureDialog.MiExitClick(Sender: TObject);
begin
 Close;
end;

procedure TFmCircledPictureDialog.MiOpenDrawingClick(Sender: TObject);
begin
 with OpenDialogCircles do
  begin
   if Execute
    then LoadDrawing(FileName);
  end;
end;

procedure TFmCircledPictureDialog.MiSaveDrawingClick(Sender: TObject);
begin
 with SaveDialogCircles do
  begin
   if Execute
    then SaveDrawing(FileName);
  end;
end;

procedure TFmCircledPictureDialog.MiSaveResultClick(Sender: TObject);
begin
 with SaveDialog do
  begin

   if Execute then
    begin

    end;
  end;
end;

procedure TFmCircledPictureDialog.MiOpenReferenceClick(Sender: TObject);
begin
 with OpenDialog do
  begin

   if Execute then
    begin
     LoadReference(FileName);
     with TRegIniFile.Create(FIniFileName) do
      try
       WriteString('Recent', 'Reference', FileName);
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmCircledPictureDialog.InitializeEvolution;
begin
 FTrialCount := 0;
 with FDiffEvol do
  begin
   MinArr[0] := -Width;
   MaxArr[0] := 2 * Width;
   MinArr[1] := -Height;
   MaxArr[1] := 2 * Height;
   MinArr[2] := 1;
   MaxArr[2] := Max(Width, Height);
   MinArr[3] := 0;
   MaxArr[3] := 1;
   MinArr[4] := 0;
   MaxArr[4] := 1;
   MinArr[5] := 0;
   MaxArr[5] := 1;
   MinArr[6] := 0;
   MaxArr[6] := 1;
   Initialize;
  end;
end;

procedure TFmCircledPictureDialog.MiStartClick(Sender: TObject);
var
  Index : Integer;
begin
 for Index := 0 to Length(FCircles) - 1
  do FreeAndNil(FCircles[Index]);

 // initialize first circle
 SetLength(FCircles, 1);
 FCircles[0] := TGuiPixelFilledCircle.Create;

 // calculate worst cost
 FWorstCost := 256 * 3 * FReference.Width * FReference.Height;

 // initialize evolution
 InitializeEvolution;

 if Assigned(FEvolution) then
  begin
   FEvolution.Start;
   MiStopContinue.Enabled := True;
   MiSaveDrawing.Enabled := False;
  end;
end;

procedure TFmCircledPictureDialog.MiStopContinueClick(Sender: TObject);
begin
 MiStopContinue.Tag := 1 - MiStopContinue.Tag;

 if MiStopContinue.Tag = 0 then
  begin
   if Assigned(FEvolution)
    then FEvolution.Suspended := False;

   MiSaveDrawing.Enabled := False;
   MiStopContinue.Caption := 'St&op'
  end
 else
  begin
   if Assigned(FEvolution)
    then FEvolution.Suspended := True;

   MiSaveDrawing.Enabled := True;
   MiStopContinue.Caption := 'C&ontinue';
  end;
end;

procedure TFmCircledPictureDialog.MiNextClick(Sender: TObject);
begin
 MiNext.Checked := True;
end;

procedure TFmCircledPictureDialog.PaintBoxDrawPaint(Sender: TObject);
begin
 FBestDrawing.PaintTo(PaintBoxDraw.Canvas);
end;

procedure TFmCircledPictureDialog.PaintBoxRefPaint(Sender: TObject);
begin
 FReference.PaintTo(PaintBoxRef.Canvas);
end;

end.
