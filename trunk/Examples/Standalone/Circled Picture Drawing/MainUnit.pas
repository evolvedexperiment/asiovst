unit MainUnit;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, StdCtrls, ComCtrls, ActnList,
  DAV_Common, DAV_Types, DAV_ChunkClasses, DAV_DifferentialEvolution,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiFixedPoint, DAV_GuiVector,
  DAV_GuiVectorPixel, DAV_GuiVectorPixelCircle;

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
    AcBack: TAction;
    AcNext: TAction;
    AcSettings: TAction;
    AcStart: TAction;
    ActionList: TActionList;
    MainMenu: TMainMenu;
    MiBack: TMenuItem;
    MiEvolve: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiNext: TMenuItem;
    MiOpenDrawing: TMenuItem;
    MiOpenReference: TMenuItem;
    MiSaveDrawing: TMenuItem;
    MiSaveHighResolution: TMenuItem;
    MiSaveResult: TMenuItem;
    MiSettings: TMenuItem;
    MiStart: TMenuItem;
    MiStopContinue: TMenuItem;
    N1: TMenuItem;
    N2: TMenuItem;
    N3: TMenuItem;
    N4: TMenuItem;
    OpenDialog: TOpenDialog;
    OpenDialogCircles: TOpenDialog;
    PaintBoxDraw: TPaintBox;
    PaintBoxRef: TPaintBox;
    SaveDialog: TSaveDialog;
    SaveDialogCircles: TSaveDialog;
    StatusBar: TStatusBar;
    MiOpenBest: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure AcBackExecute(Sender: TObject);
    procedure AcNextExecute(Sender: TObject);
    procedure AcSettingsExecute(Sender: TObject);
    procedure AcStartExecute(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiOpenDrawingClick(Sender: TObject);
    procedure MiOpenReferenceClick(Sender: TObject);
    procedure MiSaveDrawingClick(Sender: TObject);
    procedure MiSaveHighResolutionClick(Sender: TObject);
    procedure MiSaveResultClick(Sender: TObject);
    procedure MiStopContinueClick(Sender: TObject);
    procedure PaintBoxDrawPaint(Sender: TObject);
    procedure PaintBoxRefPaint(Sender: TObject);
    procedure MiOpenBestClick(Sender: TObject);
  private
    FWorstCost               : Double;
    FMaximumCost             : Double;
    FMaximumRadius           : Double;
    FLastBestCosts           : Double;
    FCumulatedError          : array [0..1] of PDAVDoubleFixedArray;
    FReference               : TGuiCustomPixelMap;
    FDrawing                 : TGuiCustomPixelMap;
    FNewDrawing              : TGuiCustomPixelMap;
    FBestDrawing             : TGuiCustomPixelMap;
    FIniFileName             : TFileName;
    FDiffEvol                : TDifferentialEvolution;
    FEvolution               : TEvolutionThread;
    FCircles                 : array of TGuiPixelFilledCircle;
    FCurrentCircle           : Integer;
    FTrialCount              : Integer;
    FTrialsPerCircle         : Integer;
    FUpdateTrials            : Integer;
    FTrialsSinceUpdate       : Integer;
    FInitialSeed             : Integer;
    FNumberOfCircles         : Integer;
    FCrossover               : Single;
    FAutoNextTrial           : Boolean;
    FCorrectColor            : Boolean;
    FCorrectRadius           : Boolean;
    FCorrectPosition         : Boolean;
    FCorrectInvisibleCircles : Boolean;
    FRandomCircle            : Boolean;
    procedure SetInitialSeed(const Value: Integer);
    procedure SetTrialsPerCircle(const Value: Integer);
    procedure SetUpdateTrials(const Value: Integer);
    procedure SetNumberOfCircles(const Value: Integer);
    procedure SetCrossover(const Value: Single);
  protected
    procedure CalculateStaticCosts;
    procedure SaveDrawing(FileName: TFileName);
    procedure LoadDrawing(FileName: TFileName);
    procedure LoadBest(FileName: TFileName);
    procedure DrawPopulation(Population: TDifferentialEvolutionPopulation;
      PixelMap: TGuiCustomPixelMap);
  public
    procedure LoadReference(FileName: TFileName);
    function CalculateError(Sender: TObject; var Population: TDifferentialEvolutionPopulation): Double;
    procedure Evolve;
    procedure InitializeEvolution;
    procedure DrawResults;

    property IniFileName: TFileName read FIniFileName;
    property DifferentialEvolution: TDifferentialEvolution read FDiffEvol;
    property EvolutionThread: TEvolutionThread read FEvolution;
    property AutoNextTrial: Boolean read FAutoNextTrial write FAutoNextTrial;
    property TrialsPerCircle: Integer read FTrialsPerCircle write SetTrialsPerCircle;
    property UpdateTrials: Integer read FUpdateTrials write SetUpdateTrials;
    property InitialSeed: Integer read FInitialSeed write SetInitialSeed;
    property Crossover: Single read FCrossover write SetCrossover;
    property NumberOfCircles: Integer read FNumberOfCircles write SetNumberOfCircles;
    property CorrectColor: Boolean read FCorrectColor write FCorrectColor;
    property CorrectRadius: Boolean read FCorrectRadius write FCorrectRadius;
    property CorrectPosition: Boolean read FCorrectPosition write FCorrectPosition;
    property CorrectInvisible: Boolean read FCorrectInvisibleCircles write FCorrectInvisibleCircles;
    property RandomCircle: Boolean read FRandomCircle write FRandomCircle;
  end;

var
  FmCircledPictureDialog: TFmCircledPictureDialog;

implementation

{$R *.dfm}

uses
  Math, Registry, IniFiles, DAV_Approximations, SettingsUnit;

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

   if Count >= FmCircledPictureDialog.UpdateTrials then
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

 // initialize default values
 FIniFileName :=  ExtractFilePath(ParamStr(0)) + 'CPD.ini';
 FAutoNextTrial := True;
 FInitialSeed := 1000;
 FTrialsPerCircle := 3000;
 FTrialsSinceUpdate := 30;
 FNumberOfCircles := 1;

 // create pixel maps
 FReference := TGuiPixelMapMemory.Create;
 FDrawing := TGuiPixelMapMemory.Create;
 FNewDrawing := TGuiPixelMapMemory.Create;
 FBestDrawing := TGuiPixelMapMemory.Create;

 // set paintboxes to opaque
 PaintBoxRef.ControlStyle := PaintBoxDraw.ControlStyle + [csOpaque];
 PaintBoxDraw.ControlStyle := PaintBoxDraw.ControlStyle + [csOpaque];

 // create and initialize differential evolution optimizer
 FDiffEvol := TDifferentialEvolution.Create(Self);
 with FDiffEvol do
  begin
   PopulationCount := FInitialSeed;
   VariableCount := 7 * FNumberOfCircles;
   CrossOver := 0.5;
   OnCalculateCosts := CalculateError;
  end;
end;

procedure TFmCircledPictureDialog.FormDestroy(Sender: TObject);
var
  Index : Integer;
begin
 // stop and free evolution thread
 if Assigned(FEvolution) then
  begin
   FEvolution.Terminate;
   if FEvolution.Suspended
    then FEvolution.Suspended := False;
   FEvolution.WaitFor;
   FreeAndNil(FEvolution);
  end;

 // dispose cumulated error
 if Assigned(FCumulatedError[0]) then Dispose(FCumulatedError[0]);
 if Assigned(FCumulatedError[1]) then Dispose(FCumulatedError[1]);

 // free all circles
 for Index := 0 to Length(FCircles) - 1
  do FreeAndNil(FCircles[Index]);

 // free all pixel maps
 FreeAndNil(FReference);
 FreeAndNil(FDrawing);
 FreeAndNil(FNewDrawing);

 // free misc.
 FreeAndNil(FBestDrawing);
 FreeAndNil(FDiffEvol);
end;

procedure TFmCircledPictureDialog.FormShow(Sender: TObject);
begin
 with TIniFile.Create(FIniFileName) do
  try
   Left := ReadInteger('Layout', 'Left', Left);
   Top := ReadInteger('Layout', 'Top', Top);

   FAutoNextTrial := ReadBool('Settings', 'Auto Trials', True);
   FTrialsPerCircle := ReadInteger('Settings', 'Trials Per Circle', FTrialsPerCircle);
   FUpdateTrials := ReadInteger('Settings', 'Update Trials', FUpdateTrials);
   FInitialSeed := ReadInteger('Settings', 'Initial Seed', FInitialSeed);
   FCrossover := 0.01 * ReadInteger('Settings', 'Crossover', Round(100 * FCrossover));
   FCorrectColor := ReadBool('Settings', 'Correct Color', True);
   FCorrectRadius := ReadBool('Settings', 'Correct Radius', True);
   FCorrectPosition := ReadBool('Settings', 'Correct Position', True);
   FCorrectInvisibleCircles := ReadBool('Settings', 'Correct Invisible Circles', True);
   FRandomCircle := ReadBool('Settings', 'Random Circle', True);

   NumberOfCircles := ReadInteger('Settings', 'Number of Circles', FNumberOfCircles);
   LoadReference(ReadString('Recent', 'Reference', ''));
  finally
   Free;
  end;
end;

procedure TFmCircledPictureDialog.FormClose(Sender: TObject;
  var Action: TCloseAction);
begin
 with TIniFile.Create(FIniFileName) do
  try
   WriteInteger('Layout', 'Left', Left);
   WriteInteger('Layout', 'Top', Top);
  finally
   Free;
  end;
end;

procedure AssignCircleData(Circle: TGuiPixelFilledCircle;
  Data: PDAV8DoubleArray);
begin
 with Circle do
  begin
   GeometricShape.CenterX := ConvertToFixed24Dot8Point(Data^[0]);
   GeometricShape.CenterY := ConvertToFixed24Dot8Point(Data^[1]);
   GeometricShape.Radius := ConvertToFixed24Dot8Point(Data^[2]);
   Color := HLSToRGB(Data^[3], Data^[4], Data^[5]);
   Alpha := Round($FF * Data^[6]);
  end;
end;

procedure TFmCircledPictureDialog.AcBackExecute(Sender: TObject);
begin
 AcBack.Checked := True;
end;

procedure TFmCircledPictureDialog.AcSettingsExecute(Sender: TObject);
begin
 with TFmSettings.Create(Self) do
  try
   if Assigned(EvolutionThread)
    then SeCircleCount.Enabled := EvolutionThread.Suspended
    else SeCircleCount.Enabled := True;
   ShowModal;
  finally
   Free;
  end;
end;

procedure TFmCircledPictureDialog.AcStartExecute(Sender: TObject);
var
  Index : Integer;
begin
 for Index := 0 to Length(FCircles) - 1
  do FreeAndNil(FCircles[Index]);

 // initialize first circle
 FCurrentCircle := 0;

 // initialize evolution
 InitializeEvolution;

 if Assigned(FEvolution) then
  begin
   FEvolution.Terminate;
   if FEvolution.Suspended
    then FEvolution.Suspended := False;
   FEvolution.WaitFor;
   FreeAndNil(FEvolution);
  end;

 FEvolution := TEvolutionThread.Create(True);
 FEvolution.Priority := tpLower;
 FEvolution.Start;
 MiStopContinue.Enabled := True;
 MiSaveDrawing.Enabled := False;
 StatusBar.Panels[0].Text := 'Running';
 StatusBar.Panels[1].Text := 'Circles: ' + IntToStr(FCurrentCircle + FNumberOfCircles);
end;

procedure TFmCircledPictureDialog.AcNextExecute(Sender: TObject);
begin
 AcNext.Checked := True;
end;

function TFmCircledPictureDialog.CalculateError(Sender: TObject;
  var Population: TDifferentialEvolutionPopulation): Double;
var
  TempData      : array [0..6] of Double;
  PixelData     : array [0..1] of PPixel32Array;
  PixelRange    : array [0..1] of Integer;
  CircleIndex   : Integer;
  Index, Offset : Integer;
  NewOffset     : Integer;
begin
 Assert(FReference <> nil);
 Assert(FDrawing <> nil);
 Assert(FNewDrawing <> nil);
 Assert(FBestDrawing <> nil);

 with FNewDrawing do
  begin
   Assert(FReference.Width * FReference.Height = Width * Height);
   Assert(FDrawing.Width * FDrawing.Height = Width * Height);
   Assert(FNewDrawing.Width * FNewDrawing.Height = Width * Height);

   if FNumberOfCircles = 1 then
    begin
     if (Population[2] < 1) or (Population[2] > FMaximumRadius)
      then Population[2] := 1 + Random * (FMaximumRadius - 1);
     if (Population[3] < 0) or (Population[3] > 1)
      then Population[3] := Random;
     if (Population[4] < 0) or (Population[4] > 1)
      then Population[4] := Random;
     if (Population[5] < 0) or (Population[5] > 1)
      then Population[5] := Random;
     if (Population[6] < 0) or (Population[6] > 1)
      then Population[6] := Random;

     if (Population[0] < -Width) or (Population[0] > 2 * Width) or
       ((Population[0] < 0) and (Population[2] < -Population[0])) or
       ((Population[0] > Width) and (Population[2] < (Population[0] - Width))) or
       (Population[1] < -Height) or (Population[1] > 2 * Height) or
       ((Population[1] < 0) and (Population[2] < -Population[1])) or
       ((Population[1] > Height) and (Population[2] < (Population[1] - Height))) or
       (Population[2] < 1) or (Population[2] > FMaximumRadius) or
       (Population[3] < 0) or (Population[3] > 1) or
       (Population[4] < 0) or (Population[4] > 1) or
       (Population[5] < 0) or (Population[5] > 1) or
       (Population[6] < 0) or (Population[6] > 1) then
      begin
       Result := FWorstCost;
       Exit;
      end;

     DrawPopulation(Population, FNewDrawing);
     Result := 0;

     PixelData[0] := FReference.DataPointer;
     PixelData[1] := DataPointer;

     PixelRange[0] := Round(Population[1] - Population[2] - 2) * Width;
     if PixelRange[0] <= 0
      then PixelRange[0] := 0
      else Result := FCumulatedError[0, PixelRange[0] - 1];

     PixelRange[1] := Round(Population[1] + Population[2] + 2) * Width;
     if PixelRange[1] >= Width * Height
      then PixelRange[1] := Width * Height
      else Result := Result + FCumulatedError[1, PixelRange[1]];

     for Index := PixelRange[0] to PixelRange[1] - 1 do
      begin
       Result := Result +
        (Abs(PixelData[0, Index].B - PixelData[1, Index].B) +
         Abs(PixelData[0, Index].G - PixelData[1, Index].G) +
         Abs(PixelData[0, Index].R - PixelData[1, Index].R)) * COne255th;
      end;
    end
   else
    begin
     for CircleIndex := 0 to FNumberOfCircles - 1 do
      begin
       if Random > 0.1 then Break;

       // get offsets
       Offset := Random(FNumberOfCircles - 1) * 7;
       repeat
        NewOffset := Random(FNumberOfCircles - 1) * 7;
       until NewOffset <> Offset;

       Move(Population[Offset], TempData[0], SizeOf(TempData));
       Move(Population[NewOffset], Population[Offset], SizeOf(TempData));
       Move(TempData[0], Population[NewOffset], SizeOf(TempData));
      end;

     for CircleIndex := 0 to FNumberOfCircles - 1 do
      begin
       Offset := 7 * CircleIndex;

       if FRandomCircle and (Random < 0.01) then
        begin
         if Random < 0.3 then
          begin
           Population[Offset + 0] := (2 * Random - 0.5) * FReference.Width;
           Population[Offset + 1] := (2 * Random - 0.5) * FReference.Height;
           Population[Offset + 2] :=  1 + Random * (FMaximumRadius - 1);
          end;
         if Random < 0.7 then
          begin
           Population[Offset + 3] :=  Random;
           Population[Offset + 4] :=  Random;
           Population[Offset + 5] :=  Random;
           Population[Offset + 6] :=  Random;
          end;
        end;

       // eventually set new x and y coordinates
       if FCorrectPosition then
        begin
         while (Population[Offset] < -Width) or (Population[Offset] > 2 * Width)
          do Population[Offset] := (2 * Random - 0.5) * Width;
         while (Population[Offset + 1] < -Height) or (Population[Offset + 1] > 2 * Height)
          do Population[Offset + 1] := (2 * Random - 0.5) * Height;

         if FCorrectInvisibleCircles then
          begin
           while (Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])
            do Population[Offset] := -Population[Offset + 2] + Random * (Width - Population[Offset + 2]);
           while (Population[Offset] > Width) and (Population[Offset + 2] < Population[Offset] - Width)
            do Population[Offset] := (Width + Population[Offset + 2]) - Random * (Width - Population[Offset + 2]);
           while (Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])
            do Population[Offset + 1] := -Population[Offset + 2] + Random * (Height - Population[Offset + 2]);
           while (Population[Offset + 1] > Height) and (Population[Offset + 2] < Population[Offset + 1] - Height)
            do Population[Offset + 1] := (Height + Population[Offset + 2]) - Random * (Height - Population[Offset + 2]);
          end;
        end;

       // eventually correct radius
       if FCorrectRadius then
        begin
         while (Population[Offset + 2] < 1) or (Population[Offset + 2] > FMaximumRadius)
          do Population[Offset + 2] := 1 + Random * (FMaximumRadius - 1);

         if FCorrectInvisibleCircles then
          begin
           while (Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])
            do Population[Offset + 2] := -Population[Offset] + Random * (FMaximumRadius + Population[Offset]);
           while (Population[Offset] > Width) and (Population[Offset + 2] < Population[Offset] - Width)
            do Population[Offset + 2] := (Population[Offset] - Width) + Random * (FMaximumRadius - Population[Offset] + Width);
           while (Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])
            do Population[Offset + 2] := -Population[Offset + 1] + Random * (FMaximumRadius + Population[Offset + 1]);
           while (Population[Offset + 1] > Height) and (Population[Offset + 2] < Population[Offset + 1] - Height)
            do Population[Offset + 2] := (Population[Offset + 1] - Height) + Random * (FMaximumRadius - Population[Offset + 1] + Height);
          end;
        end;

       if FCorrectColor then
        begin
         if (Population[Offset + 3] < 0) or (Population[Offset + 3] > 1)
          then Population[Offset + 3] := Random;
         if (Population[Offset + 4] < 0) or (Population[Offset + 4] > 1)
          then Population[Offset + 4] := Random;
         if (Population[Offset + 5] < 0) or (Population[Offset + 5] > 1)
          then Population[Offset + 5] := Random;
         if (Population[Offset + 6] < 0) or (Population[Offset + 6] > 1)
          then Population[Offset + 6] := Random;
        end;

       if (Population[Offset] < -Width) or (Population[Offset] > 2 * Width) or
         ((Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])) or
         ((Population[Offset] > Width) and (Population[Offset + 2] < (Population[Offset] - Width))) or
         (Population[Offset + 1] < -Height) or (Population[Offset + 1] > 2 * Height) or
         ((Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])) or
         ((Population[Offset + 1] > Height) and (Population[Offset + 2] < (Population[Offset + 1] - Height))) or
         (Population[Offset + 2] < 1) or (Population[Offset + 2] > FMaximumRadius) or
         (Population[Offset + 3] < 0) or (Population[Offset + 3] > 1) or
         (Population[Offset + 4] < 0) or (Population[Offset + 4] > 1) or
         (Population[Offset + 5] < 0) or (Population[Offset + 5] > 1) or
         (Population[Offset + 6] < 0) or (Population[Offset + 6] > 1) then
        begin
         Result := FWorstCost;
         Exit;
        end;
      end;

     DrawPopulation(Population, FNewDrawing);

     PixelData[0] := FReference.DataPointer;
     PixelData[1] := DataPointer;

     PixelRange[0] := 0;
     PixelRange[1] := Width * Height;

     Result := 0;
     for Index := PixelRange[0] to PixelRange[1] - 1 do
      begin
       Result := Result +
        (Abs(PixelData[0, Index].B - PixelData[1, Index].B) +
         Abs(PixelData[0, Index].G - PixelData[1, Index].G) +
         Abs(PixelData[0, Index].R - PixelData[1, Index].R)) * COne255th;
      end;
    end;
  end;

 {$IFDEF UseApproximation}
 Result := 10 * FastLog2ContinousError4(Result);
 {$ELSE}
 Result := 10 * Log2(Result);
 {$ENDIF}
end;

procedure TFmCircledPictureDialog.DrawPopulation(Population: TDifferentialEvolutionPopulation;
  PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
begin
 // copy recent drawing
 with PixelMap
  do Move(FDrawing.DataPointer^, DataPointer^, Width * Height * SizeOf(TPixel32));

 with TGuiPixelFilledCircle.Create do
  try
   for Index := 0 to FNumberOfCircles - 1 do
    begin
     GeometricShape.CenterX := ConvertToFixed24Dot8Point(Population[7 * Index + 0]);
     GeometricShape.CenterY := ConvertToFixed24Dot8Point(Population[7 * Index + 1]);
     GeometricShape.Radius := ConvertToFixed24Dot8Point(Population[7 * Index + 2]);
     Color := HLSToRGB(Population[7 * Index + 3], Population[7 * Index + 4],
       Population[7 * Index + 5]);
     Alpha := Round($FF * Population[7 * Index + 6]);

     Draw(PixelMap);
    end;
  finally
   Free;
  end;
end;

procedure TFmCircledPictureDialog.DrawResults;
begin
 DrawPopulation(FDiffEvol.GetBestPopulation, FBestDrawing);
 StatusBar.Panels[2].Text := 'Trials: ' + IntToStr(FTrialCount);
 StatusBar.Panels[3].Text := 'Costs: ' + FloatToStrF(FDiffEvol.GetBestCost - FMaximumCost, ffGeneral, 5, 5);
 StatusBar.Panels[4].Text := 'Global Costs: ' + FloatToStrF(FDiffEvol.GetBestCost, ffGeneral, 5, 5);
 PaintBoxDraw.Invalidate;
end;

procedure TFmCircledPictureDialog.Evolve;
var
  BestCosts      : Double;
  BestPopulation : TDifferentialEvolutionPopulation;
  Index          : Integer;
  OldFileName    : string;
begin
 if AcBack.Checked and AcNext.Checked then
  begin
   AcNext.Checked := False;
   AcBack.Checked := False;
  end else
 if AcNext.Checked or (FTrialCount >= FTrialsPerCircle) or
  ((FTrialsSinceUpdate >= 1000) and (FLastBestCosts - FMaximumCost < 0) and FAutoNextTrial) then
  begin
   BestPopulation := FDiffEvol.GetBestPopulation;
   DrawPopulation(BestPopulation, FDrawing);

   SetLength(FCircles, Length(FCircles) + FNumberOfCircles);
   for Index := 0 to FNumberOfCircles - 1 do
    begin
     FCircles[FCurrentCircle + Index] := TGuiPixelFilledCircle.Create;
     AssignCircleData(FCircles[FCurrentCircle + Index], @BestPopulation[7 * Index]);
    end;
   FCurrentCircle := FCurrentCircle + FNumberOfCircles;

   OldFileName := 'Backup' + IntToStr(FCurrentCircle) + '.circles';
   FCurrentCircle := Length(FCircles);
   SaveDrawing('Backup' + IntToStr(FCurrentCircle) + '.circles');
   if FileExists(OldFileName)
    then DeleteFile(OldFileName);

   StatusBar.Panels[1].Text := 'Circles: ' + IntToStr(FCurrentCircle + FNumberOfCircles);

   InitializeEvolution;
   AcNext.Checked := False;
  end else
 if AcBack.Checked then
  begin
   if (FCurrentCircle > 0) then
    begin
     Dec(FCurrentCircle, FNumberOfCircles);
     for Index := FCurrentCircle to Length(FCircles) - 1
      do FreeAndNil(FCircles[Index]);
     SetLength(FCircles, FCurrentCircle + 1);
     InitializeEvolution;
    end;
   AcBack.Checked := False;
  end;

 Randomize;
 Inc(FTrialCount);
 DifferentialEvolution.Evolve;

 BestCosts := DifferentialEvolution.GetBestCost;
 if BestCosts < FLastBestCosts
  then FTrialsSinceUpdate := 0
  else Inc(FTrialsSinceUpdate);

 FLastBestCosts := BestCosts;
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
 ClientHeight := FReference.Height + StatusBar.Height + 16;

 PaintBoxRef.Invalidate;
 PaintBoxDraw.Invalidate;
 StatusBar.Panels[0].Text := 'Ready';
end;

procedure TFmCircledPictureDialog.SaveDrawing(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Circle  : TCircleChunk;
  Index   : Integer;
begin
 try
  Circles := TCircleChunkContainer.Create;

  if (FCurrentCircle < Length(FCircles)) and Assigned(FCircles[FCurrentCircle]) then
   with FCircles[FCurrentCircle] do
    begin
     GeometricShape.CenterX := ConvertToFixed24Dot8Point(FDiffEvol.GetBestPopulation[0]);
     GeometricShape.CenterY := ConvertToFixed24Dot8Point(FDiffEvol.GetBestPopulation[1]);
     GeometricShape.Radius := ConvertToFixed24Dot8Point(FDiffEvol.GetBestPopulation[2]);
     Color := HLSToRGB(FDiffEvol.GetBestPopulation[3], FDiffEvol.GetBestPopulation[4],
       FDiffEvol.GetBestPopulation[5]);
     Alpha := Round($FF * FDiffEvol.GetBestPopulation[6]);
    end;

  for Index := 0 to Length(FCircles) - 1 do
   if Assigned(FCircles[Index]) then
    begin
     Circle := TCircleChunk.Create;
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

procedure TFmCircledPictureDialog.SetCrossover(const Value: Single);
begin
 if (FCrossover <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FCrossover := Value;
   FDiffEvol.CrossOver := Value;
  end;
end;

procedure TFmCircledPictureDialog.SetInitialSeed(const Value: Integer);
begin
 if (FInitialSeed <> Value) and (Value > 0)
  then FInitialSeed := Value;
end;

procedure TFmCircledPictureDialog.SetNumberOfCircles(const Value: Integer);
begin
 if (FNumberOfCircles <> Value) and (Value > 0) then
  begin
   FNumberOfCircles := Value;
   FDiffEvol.VariableCount := 7 * FNumberOfCircles;
  end;
end;

procedure TFmCircledPictureDialog.SetTrialsPerCircle(const Value: Integer);
begin
 if (FTrialsPerCircle <> Value) and (Value > 0) then
  begin
   FTrialsPerCircle := Value;
   if FUpdateTrials > FTrialsPerCircle
    then FUpdateTrials := FTrialsPerCircle;
  end;
end;

procedure TFmCircledPictureDialog.SetUpdateTrials(const Value: Integer);
begin
 if (FUpdateTrials <> Value) and (Value > 0) and (FUpdateTrials <= FTrialsPerCircle)
  then FUpdateTrials := Value;
end;

procedure TFmCircledPictureDialog.LoadBest(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Index   : Integer;
  Pixel   : TPixel32;
  H, S, L : Single;
begin
 try
  Circles := TCircleChunkContainer.Create;
  Circles.LoadFromFile(FileName);

  for Index := 0 to Min(FNumberOfCircles, Circles.Count) - 1 do
   with DifferentialEvolution do
    begin
     BestPopulation[7 * Index    ] := ConvertFromFixed24Dot8Point(Circles[Index].CenterX);
     BestPopulation[7 * Index + 1] := ConvertFromFixed24Dot8Point(Circles[Index].CenterY);
     BestPopulation[7 * Index + 2] := ConvertFromFixed24Dot8Point(Circles[Index].Radius);
     Pixel.R := Circles[Index].Color and $FF;
     Pixel.G := (Circles[Index].Color shr 8) and $FF;
     Pixel.B := (Circles[Index].Color shr 16) and $FF;

     PixelToHLS(Pixel, H, S, L);
     BestPopulation[7 * Index + 3] := H;
     BestPopulation[7 * Index + 4] := S;
     BestPopulation[7 * Index + 5] := L;
     BestPopulation[7 * Index + 6] := Circles[Index].Alpha * COne255th;
    end;
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

  for Index := 0 to Length(FCircles) - 1
   do FreeAndNil(FCircles[Index]);

  SetLength(FCircles, Circles.Count);
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
  if Assigned(FEvolution) then
   begin
    FEvolution.Terminate;
    if FEvolution.Suspended
     then FEvolution.Suspended := False;
    FEvolution.WaitFor;
    FreeAndNil(FEvolution);
   end;

  InitializeEvolution;
  FEvolution := TEvolutionThread.Create(True);
  FEvolution.Priority := tpLower;
  FEvolution.Start;
  FEvolution.Suspended := True;
  MiStopContinue.Enabled := True;
  MiSaveDrawing.Enabled := True;
  MiStopContinue.Tag := 1;
  MiStopContinue.Caption := 'C&ontinue';

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

procedure TFmCircledPictureDialog.MiOpenBestClick(Sender: TObject);
begin
 with OpenDialogCircles do
  begin
   if Execute
    then LoadBest(FileName);
  end;
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

procedure TFmCircledPictureDialog.MiSaveHighResolutionClick(Sender: TObject);
var
  DrawingHR : TGuiPixelMapMemory;
  Index     : Integer;
begin
 with SaveDialog do
  begin
   if Execute then
    try
     DrawingHR := TGuiPixelMapMemory.Create;
     DrawingHR.Width := 4 * FBestDrawing.Width;
     DrawingHR.Height := 4 * FBestDrawing.Height;
     DrawingHR.Clear;

     for Index := 0 to Length(FCircles) - 1 do
      with FCircles[Index] do
       begin
        GeometricShape.Radius := FixedMul(GeometricShape.Radius, 4 shl 8);
        GeometricShape.CenterX := FixedMul(GeometricShape.CenterX, 4 shl 8);
        GeometricShape.CenterY := FixedMul(GeometricShape.CenterY, 4 shl 8);
        Draw(DrawingHR);
        GeometricShape.Radius := FixedDiv(GeometricShape.Radius, 4 shl 8);
        GeometricShape.CenterX := FixedDiv(GeometricShape.CenterX, 4 shl 8);
        GeometricShape.CenterY := FixedDiv(GeometricShape.CenterY, 4 shl 8);
       end;

     // make opaque
     for Index := 0 to DrawingHR.Height * DrawingHR.Width - 1
      do DrawingHR.DataPointer[Index].A := $FF;

     DrawingHR.SaveToFile(FileName);
    finally
     if Assigned(DrawingHR) then FreeAndNil(DrawingHR);
    end;
  end;
end;

procedure TFmCircledPictureDialog.MiSaveResultClick(Sender: TObject);
begin
 with SaveDialog do
  begin
   if Execute
    then FBestDrawing.SaveToFile(FileName);
  end;
end;

procedure TFmCircledPictureDialog.MiOpenReferenceClick(Sender: TObject);
begin
 with OpenDialog do
  begin

   if Execute then
    begin
     LoadReference(FileName);
     with TIniFile.Create(FIniFileName) do
      try
       WriteString('Recent', 'Reference', OpenDialog.FileName);
      finally
       Free;
      end;
    end;
  end;
end;

procedure TFmCircledPictureDialog.InitializeEvolution;
var
  Index : Integer;
begin
 // calculate maximum radius
 FMaximumRadius := 1.5 * Max(Width, Height);
 if FMaximumRadius < 1 then FMaximumRadius := 1;

 FTrialCount := 0;
 with FDiffEvol do
  begin
   CrossOver := FCrossover;
   PopulationCount := FInitialSeed;
   for Index := 0 to FNumberOfCircles - 1 do
    begin
     MinConstraints[7 * Index + 0] := -0.5 * FReference.Width;
     MaxConstraints[7 * Index + 0] :=  1.5 * FReference.Width;
     MinConstraints[7 * Index + 1] := -0.5 * FReference.Height;
     MaxConstraints[7 * Index + 1] :=  1.5 * FReference.Height;
     MinConstraints[7 * Index + 2] :=  1;
     MaxConstraints[7 * Index + 2] :=  0.5 * FMaximumRadius;
     MinConstraints[7 * Index + 3] :=  0;
     MaxConstraints[7 * Index + 3] :=  1;
     MinConstraints[7 * Index + 4] :=  0;
     MaxConstraints[7 * Index + 4] :=  1;
     MinConstraints[7 * Index + 5] :=  0;
     MaxConstraints[7 * Index + 5] :=  1;
     MinConstraints[7 * Index + 6] :=  0;
     MaxConstraints[7 * Index + 6] :=  1;
    end;
   Initialize;
  end;
 CalculateStaticCosts;
end;

procedure TFmCircledPictureDialog.CalculateStaticCosts;
var
  CurrentCost : Double;
  DataSize    : Integer;
  PixelData   : array [0..1] of PPixel32Array;
  Index       : Integer;
begin
 // calculate worst cost
 {$IFDEF UseApproximation}
 FWorstCost := 10 * FastLog2ContinousError4(256 * 3 * FReference.Width * FReference.Height);
 {$ELSE}
 FWorstCost := 10 * Log2(256 * 3 * FReference.Width * FReference.Height);
 {$ENDIF}

 // calculate temporary data size
 DataSize := FReference.Width * FReference.Height * SizeOf(Double);

 // allocate memory for cumulated error
 ReallocMem(FCumulatedError[0], DataSize);
 ReallocMem(FCumulatedError[1], DataSize);

 with FDrawing do
  begin
   Assert(FReference.Width * FReference.Height = Width * Height);

   CurrentCost := 0;
   PixelData[0] := FReference.DataPointer;
   PixelData[1] := DataPointer;
   for Index := 0 to Width * Height - 1 do
    begin
     CurrentCost := CurrentCost +
      (Abs(PixelData[0, Index].B - PixelData[1, Index].B) +
       Abs(PixelData[0, Index].G - PixelData[1, Index].G) +
       Abs(PixelData[0, Index].R - PixelData[1, Index].R)) * COne255th;
     FCumulatedError[0, Index] := CurrentCost;
    end;

   {$IFDEF UseApproximation}
   FMaximumCost := 10 * FastLog2ContinousError4(CurrentCost);
   {$ELSE}
   FMaximumCost := 10 * Log2(CurrentCost);
   {$ENDIF}

   CurrentCost := 0;
   for Index := Width * Height - 1 downto 0 do
    begin
     CurrentCost := CurrentCost +
      (Abs(PixelData[0, Index].B - PixelData[1, Index].B) +
       Abs(PixelData[0, Index].G - PixelData[1, Index].G) +
       Abs(PixelData[0, Index].R - PixelData[1, Index].R)) * COne255th;
     FCumulatedError[1, Index] := CurrentCost;
    end;
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
   MiStopContinue.Caption := 'St&op';
   StatusBar.Panels[0].Text := 'Running';
  end
 else
  begin
   if Assigned(FEvolution)
    then FEvolution.Suspended := True;

   MiSaveDrawing.Enabled := True;
   MiStopContinue.Caption := 'C&ontinue';
   StatusBar.Panels[0].Text := 'Paused';
  end;
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
