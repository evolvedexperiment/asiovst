unit MainUnit;

{$I DAV_Compiler.inc}

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  Menus, ExtCtrls, StdCtrls, ComCtrls, ActnList,
  DAV_Common, DAV_Types, DAV_ChunkClasses, DAV_DifferentialEvolution,
  DAV_GuiCommon, DAV_GuiPixelMap, DAV_GuiFixedPoint, DAV_GuiVector,
  DAV_GuiVectorPixel, DAV_GuiVectorPixelCircle, DAV_GuiFileFormats, DAV_GuiPng;

type
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
    MiCopyReference: TMenuItem;
    MiEvolve: TMenuItem;
    MiExit: TMenuItem;
    MiFile: TMenuItem;
    MiNext: TMenuItem;
    MiOpenBest: TMenuItem;
    MiOpenDrawing: TMenuItem;
    MiOpenReference: TMenuItem;
    MiSaveAnimation: TMenuItem;
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
    MiSavePopulation: TMenuItem;
    N5: TMenuItem;
    MiLoadPopulation: TMenuItem;
    procedure FormCreate(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure FormDestroy(Sender: TObject);
    procedure AcBackExecute(Sender: TObject);
    procedure AcNextExecute(Sender: TObject);
    procedure AcSettingsExecute(Sender: TObject);
    procedure AcStartExecute(Sender: TObject);
    procedure MiCopyReferenceClick(Sender: TObject);
    procedure MiExitClick(Sender: TObject);
    procedure MiLoadPopulationClick(Sender: TObject);
    procedure MiOpenBestClick(Sender: TObject);
    procedure MiOpenDrawingClick(Sender: TObject);
    procedure MiOpenReferenceClick(Sender: TObject);
    procedure MiSaveAnimationClick(Sender: TObject);
    procedure MiSaveDrawingClick(Sender: TObject);
    procedure MiSaveHighResolutionClick(Sender: TObject);
    procedure MiSavePopulationClick(Sender: TObject);
    procedure MiSaveResultClick(Sender: TObject);
    procedure MiStopContinueClick(Sender: TObject);
    procedure PaintBoxDrawPaint(Sender: TObject);
    procedure PaintBoxRefPaint(Sender: TObject);
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
    FCurrentOrder            : Integer;
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
    FRandomOrder             : Boolean;
    FChangeOrder             : Boolean;
    FWeightDither            : Boolean;
    FAutoInitialSeed         : Boolean;
    FWeight                  : Single;
    FBest                    : Single;
    FAdditional              : Single;
    FDrawDraft               : Boolean;
    procedure SetInitialSeed(const Value: Integer);
    procedure SetTrialsPerCircle(const Value: Integer);
    procedure SetUpdateTrials(const Value: Integer);
    procedure SetNumberOfCircles(const Value: Integer);
    procedure SetCrossover(const Value: Single);
    procedure SetWeight(const Value: Single);
    procedure SetBest(const Value: Single);
    procedure SetAdditional(const Value: Single);
    procedure SetRandomOrder(const Value: Boolean);
  protected
    procedure ResetCircles;
    procedure SaveDrawingBackup;
    procedure SavePopulationBackup(FileName: TFileName);
    procedure LoadPopulationBackup(FileName: TFileName);
    procedure CalculateStaticCosts; virtual;
    procedure SaveDrawing(FileName: TFileName);
    procedure SaveAnimation(FileName: TFileName; ScaleFactor: Single;
      AnimatedCircle: Boolean = False);
    procedure LoadDrawing(FileName: TFileName);
    procedure LoadBest(FileName: TFileName);
    procedure DrawPopulation(Population: TDifferentialEvolutionPopulation;
      PixelMap: TGuiCustomPixelMap);
  public
    procedure LoadReference(FileName: TFileName);
    function CalculateError(Sender: TObject; var Population: TDifferentialEvolutionPopulation): Double;
    procedure Evolve;
    procedure InitializeEvolution(InitializePopulation: Boolean = True);
    procedure StartOptimization(InitializePopulation: Boolean = True);
    procedure DrawResults;

    property Additional: Single read FAdditional write SetAdditional;
    property AutoInitialSeed: Boolean read FAutoInitialSeed write FAutoInitialSeed;
    property AutoNextTrial: Boolean read FAutoNextTrial write FAutoNextTrial;
    property Best: Single read FBest write SetBest;
    property ChangeOrder: Boolean read FChangeOrder write FChangeOrder;
    property CorrectColor: Boolean read FCorrectColor write FCorrectColor;
    property CorrectInvisible: Boolean read FCorrectInvisibleCircles write FCorrectInvisibleCircles;
    property CorrectPosition: Boolean read FCorrectPosition write FCorrectPosition;
    property CorrectRadius: Boolean read FCorrectRadius write FCorrectRadius;
    property Crossover: Single read FCrossover write SetCrossover;
    property DifferentialEvolution: TDifferentialEvolution read FDiffEvol;
    property EvolutionThread: TEvolutionThread read FEvolution;
    property IniFileName: TFileName read FIniFileName;
    property InitialSeed: Integer read FInitialSeed write SetInitialSeed;
    property NumberOfCircles: Integer read FNumberOfCircles write SetNumberOfCircles;
    property RandomCircle: Boolean read FRandomCircle write FRandomCircle;
    property RandomOrder: Boolean read FRandomOrder write SetRandomOrder;
    property TrialsPerCircle: Integer read FTrialsPerCircle write SetTrialsPerCircle;
    property UpdateTrials: Integer read FUpdateTrials write SetUpdateTrials;
    property Weight: Single read FWeight write SetWeight;
    property WeightDither: Boolean read FWeightDither write FWeightDither;
  end;

var
  FmCircledPictureDialog: TFmCircledPictureDialog;

implementation

{$R *.lfm}

uses
  Filectrl, Math, Registry, IniFiles, DAV_Approximations, SettingsUnit,
  ProgressBarUnit, AdditionalChunks;


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
     if FmCircledPictureDialog.WindowState <> wsMinimized
      then Sleep(10);
    end;
  end;
end;


{ TFmCircledPictureDialog }

procedure TFmCircledPictureDialog.FormCreate(Sender: TObject);
begin
 Randomize;

 // initialize default values
 FIniFileName :=  ExtractFilePath(ParamStr(0)) + 'CPD.ini';
 FAutoNextTrial := True;
 FAutoInitialSeed := False;
 FInitialSeed := 1000;
 FTrialsPerCircle := 3000;
 FTrialsSinceUpdate := 30;
 FNumberOfCircles := 1;
 FWeight := 0.7;
 FBest := 0;
 FAdditional := 0;
 FDrawDraft := False;
 FRandomCircle := False;
 FRandomOrder := False;
 FWeightDither := False;
 FCorrectColor := True;
 FCorrectRadius := True;
 FCorrectPosition := True;
 FCorrectInvisibleCircles := True;

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
   CrossOver := 0.9;
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

   FAutoInitialSeed := ReadBool('Settings', 'Auto Initial Seed', FAutoInitialSeed);
   FAutoNextTrial := ReadBool('Settings', 'Auto Trials', True);
   FTrialsPerCircle := ReadInteger('Settings', 'Trials Per Circle', FTrialsPerCircle);
   FUpdateTrials := ReadInteger('Settings', 'Update Trials', FUpdateTrials);
   FInitialSeed := ReadInteger('Settings', 'Initial Seed', FInitialSeed);
   FAdditional := 0.01 * ReadInteger('Settings', 'Additional', Round(100 * FAdditional));
   FCrossover := 0.01 * ReadInteger('Settings', 'Crossover', Round(100 * FCrossover));
   FWeight := 0.01 * ReadInteger('Settings', 'Weight', Round(100 * FWeight));
   FBest := 0.01 * ReadInteger('Settings', 'Best', Round(100 * FBest));
   FWeightDither := ReadBool('Settings', 'Weight Dither', FWeightDither);
   FChangeOrder := ReadBool('Settings', 'Change Order', FChangeOrder);
   FRandomOrder := ReadBool('Settings', 'Random Order', FRandomOrder);
   FCorrectColor := ReadBool('Settings', 'Correct Color', FCorrectColor);
   FCorrectRadius := ReadBool('Settings', 'Correct Radius', FCorrectRadius);
   FCorrectPosition := ReadBool('Settings', 'Correct Position', FCorrectPosition);
   FCorrectInvisibleCircles := ReadBool('Settings', 'Correct Invisible Circles', FCorrectInvisibleCircles);
   FRandomCircle := ReadBool('Settings', 'Random Circle', FRandomCircle);

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
begin
 StartOptimization;
end;

procedure TFmCircledPictureDialog.AcNextExecute(Sender: TObject);
begin
 AcNext.Checked := True;
end;

procedure TFmCircledPictureDialog.MiCopyReferenceClick(Sender: TObject);
begin
 FDrawing.Assign(FReference);
 FBestDrawing.Assign(FReference);
 PaintBoxDraw.Invalidate;
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

procedure TFmCircledPictureDialog.MiSaveAnimationClick(Sender: TObject);
var
  Dir : string;
begin
 Dir := ExtractFileDir(ParamStr(0));
 if SelectDirectory(Dir, [sdAllowCreate, sdPerformCreate, sdPrompt], 0)
  then SaveAnimation(Dir, 2);
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
      if Assigned(FCircles[Index]) then
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

procedure TFmCircledPictureDialog.MiSavePopulationClick(Sender: TObject);
begin
 SavePopulationBackup('Backup.pop');
end;

procedure TFmCircledPictureDialog.MiLoadPopulationClick(Sender: TObject);
begin
 with TOpenDialog.Create(Self) do
  try
   Filter := 'Population Backup (*.pop)|*.pop';
   DefaultExt := '.pop';
   if Execute
    then LoadPopulationBackup(FileName);
  finally
   Free;
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

procedure TFmCircledPictureDialog.SetAdditional(const Value: Single);
begin
 if (FAdditional <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FAdditional := Value;
   FDiffEvol.GainR3 := Value;
  end;
end;

procedure TFmCircledPictureDialog.SetBest(const Value: Single);
begin
 if (FBest <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FBest := Value;
   FDiffEvol.GainBest := Value;
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

procedure TFmCircledPictureDialog.SetRandomOrder(const Value: Boolean);
begin
 FRandomOrder := (FNumberOfCircles = 1) and Value;
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

procedure TFmCircledPictureDialog.SetWeight(const Value: Single);
begin
 if (FWeight <> Value) and (Value >= 0) and (Value <= 1) then
  begin
   FWeight := Value;
   FDiffEvol.GainR1 := -Value;
   FDiffEvol.GainR2 := Value;
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

function ErrorWeighting(Current, Reference: TPixel32): Single; inline;
begin
 Result := (Abs(Reference.B - Current.B) +
   Abs(Reference.G - Current.G) +
   Abs(Reference.R - Current.R)) * COne255th;
 Result := 0.1 * Result + 0.9 * Sqr(Result);
end;

procedure TFmCircledPictureDialog.ResetCircles;
var
  Index : Integer;
begin
 for Index := 0 to Length(FCircles) - 1
  do FreeAndNil(FCircles[Index]);

 // initialize first circle
 FCurrentCircle := 0;
 FCurrentOrder := 0;
end;

procedure TFmCircledPictureDialog.StartOptimization(InitializePopulation: Boolean = True);
begin
 ResetCircles;

 // initialize evolution
 InitializeEvolution(InitializePopulation);

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
 MiSavePopulation.Enabled := True;
 MiNext.Enabled := True;
 MiBack.Enabled := True;
 StatusBar.Panels[0].Text := 'Running';
 StatusBar.Panels[1].Text := 'Circles: ' + IntToStr(FCurrentCircle + FNumberOfCircles);
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
     // eventually set new x and y coordinates
     if FCorrectPosition then
      begin
       if (Population[0] < -Width) or (Population[0] > 2 * Width)
        then Population[0] := (2 * Random - 0.5) * Width;
       if (Population[1] < -Height) or (Population[1] > 2 * Height)
        then Population[1] := (2 * Random - 0.5) * Height;

       if FCorrectInvisibleCircles then
        begin
         if (Population[0] < 0) and (Population[2] < -Population[0])
          then Population[0] := -Population[2] + Random * (Width - Population[2]);
         if (Population[0] > Width) and (Population[2] < Population[0] - Width)
          then Population[0] := (Width + Population[2]) - Random * (Width - Population[2]);
         if (Population[1] < 0) and (Population[2] < -Population[1])
          then Population[1] := -Population[2] + Random * (Height - Population[2]);
         if (Population[1] > Height) and (Population[2] < Population[1] - Height)
          then Population[1] := (Height + Population[2]) - Random * (Height - Population[2]);
        end;
      end;

     // eventually correct radius
     if FCorrectRadius then
      begin
       if (Population[2] < 1) or (Population[2] > FMaximumRadius)
        then Population[2] := 1 + Random * (FMaximumRadius - 1);

       if FCorrectInvisibleCircles then
        begin
         if (Population[0] < 0) and (Population[2] < -Population[0])
          then Population[2] := -Population[0] + Random * (FMaximumRadius + Population[0]);
         if (Population[0] > Width) and (Population[2] < Population[0] - Width)
          then Population[2] := (Population[0] - Width) + Random * (FMaximumRadius - Population[0] + Width);
         if (Population[1] < 0) and (Population[2] < -Population[1])
          then Population[2] := -Population[1] + Random * (FMaximumRadius + Population[1]);
         if (Population[1] > Height) and (Population[2] < Population[1] - Height)
          then Population[2] := (Population[1] - Height) + Random * (FMaximumRadius - Population[1] + Height);
        end;
      end;

     if FCorrectColor then
      begin
       if (Population[3] < 0) or (Population[3] > 1)
        then Population[3] := Random;
       if (Population[4] < 0) or (Population[4] > 1)
        then Population[4] := Random;
       if (Population[5] < 0) or (Population[5] > 1)
        then Population[5] := Random;
       if (Population[6] < 0) or (Population[6] > 1)
        then Population[6] := Random;
      end;

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

     for Index := PixelRange[0] to PixelRange[1] - 1
      do Result := Result + ErrorWeighting(PixelData[1, Index], PixelData[0, Index]);
    end
   else
    begin
     if FChangeOrder and (FNumberOfCircles > 2) then
      for CircleIndex := 0 to FNumberOfCircles - 1 do
       begin
        if Random > 0.1 then Break;

        // get offsets
        Offset := Random(FNumberOfCircles) * 7;
        repeat
         NewOffset := Random(FNumberOfCircles) * 7;
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
         if (Population[Offset] < -Width) or (Population[Offset] > 2 * Width)
          then Population[Offset] := (2 * Random - 0.5) * Width;
         if (Population[Offset + 1] < -Height) or (Population[Offset + 1] > 2 * Height)
          then Population[Offset + 1] := (2 * Random - 0.5) * Height;

         if FCorrectInvisibleCircles then
          begin
           if (Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])
            then Population[Offset] := -Population[Offset + 2] + Random * (FMaximumRadius - Population[Offset + 2]);
           if (Population[Offset] > Width) and (Population[Offset + 2] < Population[Offset] - Width)
            then Population[Offset] := (Width + Population[Offset + 2]) - Random * (FMaximumRadius - Population[Offset + 2]);
           if (Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])
            then Population[Offset + 1] := -Population[Offset + 2] + Random * (FMaximumRadius - Population[Offset + 2]);
           if (Population[Offset + 1] > Height) and (Population[Offset + 2] < Population[Offset + 1] - Height)
            then Population[Offset + 1] := (Height + Population[Offset + 2]) - Random * (FMaximumRadius - Population[Offset + 2]);
          end;
        end;

       // eventually correct radius
       if FCorrectRadius then
        begin
         if (Population[Offset + 2] < 1) or (Population[Offset + 2] > FMaximumRadius)
          then Population[Offset + 2] := 1 + Random * (FMaximumRadius - 1);

         if FCorrectInvisibleCircles then
          begin
           if (Population[Offset] < 0) and (Population[Offset + 2] < -Population[Offset])
            then Population[Offset + 2] := -Population[Offset] + Random * (FMaximumRadius + Population[Offset]);
           if (Population[Offset] > Width) and (Population[Offset + 2] < Population[Offset] - Width)
            then Population[Offset + 2] := (Population[Offset] - Width) + Random * (FMaximumRadius - Population[Offset] + Width);

           if (Population[Offset + 1] < 0) and (Population[Offset + 2] < -Population[Offset + 1])
            then Population[Offset + 2] := -Population[Offset + 1] + Random * (FMaximumRadius + Population[Offset + 1]);
           if (Population[Offset + 1] > Height) and (Population[Offset + 2] < Population[Offset + 1] - Height)
            then Population[Offset + 2] := (Population[Offset + 1] - Height) + Random * (FMaximumRadius - Population[Offset + 1] + Height);
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
         (Population[Offset + 6] < 0) or (Population[Offset + 6] > 1) or
         (Round($FF * Population[Offset + 6]) = 0) then
        begin
         Result := FWorstCost;
         Exit;
        end;
      end;

     DrawPopulation(Population, FNewDrawing);

     PixelData[0] := FReference.DataPointer;
     PixelData[1] := DataPointer;

     Result := 0;
     for Index := 0 to Width * Height - 1
      do Result := Result + ErrorWeighting(PixelData[1, Index], PixelData[0, Index]);
    end;
  end;

 {$IFDEF UseApproximation}
 Result := 10 * FastLog2ContinousError4(1E-30 + Result);
 {$ELSE}
 Result := 10 * Log2(1E-30 + Result);
 {$ENDIF}
end;

procedure TFmCircledPictureDialog.DrawPopulation(Population: TDifferentialEvolutionPopulation;
  PixelMap: TGuiCustomPixelMap);
var
  Index : Integer;
begin
 if FRandomOrder then
  begin
   // clear pixel map
   PixelMap.Clear;

   // draw background circles
   if FDrawDraft then
    for Index := 0 to FCurrentOrder - 1
     do FCircles[Index].DrawDraft(PixelMap)
   else
    for Index := 0 to FCurrentOrder - 1
     do FCircles[Index].Draw(PixelMap);

   // draw recent circle
   with TGuiPixelFilledCircle.Create do
    try
     GeometricShape.CenterX := ConvertToFixed24Dot8Point(Population[0]);
     GeometricShape.CenterY := ConvertToFixed24Dot8Point(Population[1]);
     GeometricShape.Radius := ConvertToFixed24Dot8Point(Population[2]);
     Color := HLSToRGB(Population[3], Population[4], Population[5]);
     Alpha := Round($FF * Population[6]);

     if FDrawDraft
      then DrawDraft(PixelMap)
      else Draw(PixelMap);
    finally
     Free;
    end;

   // draw background circles
   for Index := FCurrentOrder to Length(FCircles) - 1
    do FCircles[Index].Draw(PixelMap);
  end
 else
  begin
   // copy recent drawing
   with PixelMap
    do Move(FDrawing.DataPointer^, DataPointer^, Width * Height * SizeOf(TPixel32));

   // draw latest circle
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

       if FDrawDraft
        then DrawDraft(PixelMap)
        else Draw(PixelMap);
      end;
    finally
     Free;
    end;
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

procedure TFmCircledPictureDialog.InitializeEvolution(InitializePopulation: Boolean = True);
var
  Index : Integer;
begin
 // calculate maximum radius
 FMaximumRadius := 1.5 * Max(Width, Height);
 if FMaximumRadius < 1 then FMaximumRadius := 1;

 FTrialCount := 0;

 if InitializePopulation then
  begin
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
     CurrentCost := CurrentCost + ErrorWeighting(PixelData[1, Index], PixelData[0, Index]);
     FCumulatedError[0, Index] := CurrentCost;
    end;

   if CurrentCost = 0
    then FMaximumCost := FWorstCost
    else
   {$IFDEF UseApproximation}
   FMaximumCost := 10 * FastLog2ContinousError4(CurrentCost);
   {$ELSE}
   FMaximumCost := 10 * Log2(CurrentCost);
   {$ENDIF}

   CurrentCost := 0;
   for Index := Width * Height - 1 downto 0 do
    begin
     CurrentCost := CurrentCost + ErrorWeighting(PixelData[1, Index],
       PixelData[0, Index]);
     FCumulatedError[1, Index] := CurrentCost;
    end;
  end;
end;

procedure TFmCircledPictureDialog.SaveDrawingBackup;
var
  OldFileName : TFileName;
begin
 OldFileName := 'Backup' + IntToStr(FCurrentCircle) + '.circles';
 FCurrentCircle := FCurrentCircle + FNumberOfCircles;
 SaveDrawing('Backup' + IntToStr(FCurrentCircle) + '.circles');
 if FileExists(OldFileName) and (FCurrentCircle mod 128 <> 0)
  then DeleteFile(OldFileName);
end;

procedure TFmCircledPictureDialog.SavePopulationBackup(FileName: TFileName);
var
  Index             : Integer;
  VariableIndex     : Integer;
  CurrentPopulation : TPopulationChunk;
begin
 with TPopulationChunkContainer.Create do
  try
   for Index := 0 to FDiffEvol.PopulationCount - 1 do
    begin
     CurrentPopulation := TPopulationChunk.Create;
     CurrentPopulation.VariableCount := FDiffEvol.VariableCount;
     for VariableIndex := 0 to FDiffEvol.VariableCount - 1
      do CurrentPopulation.Variable[VariableIndex] := FDiffEvol.Population[Index][VariableIndex];
     AddChunk(CurrentPopulation);
    end;
   SaveToFile(FileName);
  finally
   Free;
  end;
end;

procedure TFmCircledPictureDialog.LoadPopulationBackup(FileName: TFileName);
var
  Index             : Integer;
  VariableIndex     : Integer;
  CurrentPopulation : TPopulationChunk;
begin
 with TPopulationChunkContainer.Create do
  try
   LoadFromFile(FileName);
   for Index := 0 to Count - 1 do
    for VariableIndex := 0 to Min(Population[Index].VariableCount, FDiffEvol.VariableCount) - 1
     do FDiffEvol.Population[Index][VariableIndex] := Population[Index].Variable[VariableIndex];
   FDiffEvol.Initialize(False);
   if not MiStopContinue.Enabled
    then StartOptimization(False);
  finally
   Free;
  end;
end;

procedure TFmCircledPictureDialog.Evolve;
var
  BestCosts      : Double;
  BestPopulation : TDifferentialEvolutionPopulation;
  Index          : Integer;
begin
 if AcBack.Checked and AcNext.Checked then
  begin
   AcNext.Checked := False;
   AcBack.Checked := False;
  end else
 if AcNext.Checked or (FTrialCount >= FTrialsPerCircle) or
  ((FTrialsSinceUpdate >= (0.25 * FTrialsPerCircle)) and
  (FLastBestCosts - FMaximumCost < 0) and FAutoNextTrial) then
  begin
   BestPopulation := FDiffEvol.GetBestPopulation;
   DrawPopulation(BestPopulation, FDrawing);

   SetLength(FCircles, Length(FCircles) + FNumberOfCircles);

   if FRandomOrder then
    begin
     Assert(FNumberOfCircles = 1);
     Move(FCircles[FCurrentOrder], FCircles[FCurrentOrder + 1],
       (Length(FCircles) - (FCurrentOrder + 1)) * SizeOf(TGuiPixelFilledCircle));
     FCircles[FCurrentOrder] := TGuiPixelFilledCircle.Create;
     AssignCircleData(FCircles[FCurrentOrder], @BestPopulation[0]);
     FCurrentOrder := Random(Length(FCircles) + 1);
    end
   else
    for Index := 0 to FNumberOfCircles - 1 do
     begin
      FCircles[FCurrentCircle + Index] := TGuiPixelFilledCircle.Create;
      AssignCircleData(FCircles[FCurrentCircle + Index], @BestPopulation[7 * Index]);
     end;

   // save backup
   SaveDrawingBackup;

   // update status bar
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

 if FWeightDither then
  begin
   DifferentialEvolution.GainR2 := 0.5 * (1 + Random);
   DifferentialEvolution.GainR1 := - DifferentialEvolution.GainR2;
  end;

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

procedure TFmCircledPictureDialog.SaveAnimation(FileName: TFileName;
  ScaleFactor: Single; AnimatedCircle: Boolean = False);
var
  Drawing    : TGuiPixelMapMemory;
  Circle     : TGuiPixelFilledCircle;
  OffsetX    : TFixed24Dot8Point;
  OffsetY    : TFixed24Dot8Point;
  FrameIndex : Integer;
  Index      : Integer;
  IntScale   : Integer;
begin
 with TFmProgressBar.Create(Self) do
  try
   Show;
   Drawing := TGuiPixelMapMemory.Create;
   Drawing.Width := Round(2 * ScaleFactor * FBestDrawing.Width);
   Drawing.Height := Round(2 * ScaleFactor * FBestDrawing.Height);
   Drawing.Clear;
   Drawing.SaveToFile(FileName + '\Frame0.png');

   Circle := TGuiPixelFilledCircle.Create;

   IntScale := Round(ScaleFactor * (1 shl 8));
   OffsetX := ConvertToFixed24Dot8Point(Drawing.Width div 4);
   OffsetY := ConvertToFixed24Dot8Point(Drawing.Height div 4);

   if AnimatedCircle then
    begin
     ProgressBar.Max := Length(FCircles);
     for FrameIndex := 0 to Length(FCircles) - 1 do
      begin
       Drawing.Clear;

       for Index := 0 to FrameIndex - 1 do
        if Assigned(FCircles[Index]) then
         with FCircles[Index] do
          begin
           Circle.GeometricShape.Radius  := FixedMul(GeometricShape.Radius, IntScale);
           Circle.GeometricShape.CenterX := FixedAdd(FixedMul(GeometricShape.CenterX, IntScale), OffsetX);
           Circle.GeometricShape.CenterY := FixedAdd(FixedMul(GeometricShape.CenterY, IntScale), OffsetY);
           Circle.Draw(Drawing);
          end;

        if Assigned(FCircles[FrameIndex]) then
         with FCircles[FrameIndex] do
          begin
           Circle.GeometricShape.Radius  := FixedMul(GeometricShape.Radius, IntScale);
           Circle.GeometricShape.CenterX := FixedAdd(FixedMul(GeometricShape.CenterX, IntScale), OffsetX);
           Circle.GeometricShape.CenterY := FixedAdd(FixedMul(GeometricShape.CenterY, IntScale), OffsetY);
           Circle.Draw(Drawing);
          end;

       Drawing.ResetAlpha;
       Drawing.SaveToFile(FileName + '\Frame' + IntToStr(FrameIndex + 1) + '.png');
      end;
    end
   else
    begin
     ProgressBar.Max := Length(FCircles) + 16;
     for Index := 0 to Length(FCircles) - 1 do
      if Assigned(FCircles[Index]) then
       with FCircles[Index] do
        begin
         Circle.GeometricShape.Radius := FixedMul(GeometricShape.Radius, IntScale);
         Circle.GeometricShape.CenterX := FixedAdd(FixedMul(GeometricShape.CenterX, IntScale), OffsetX);
         Circle.GeometricShape.CenterY := FixedAdd(FixedMul(GeometricShape.CenterY, IntScale), OffsetY);
         Circle.Color := Color;
         Circle.Alpha := Alpha;
         Circle.Draw(Drawing);
         Drawing.ResetAlpha;
         Drawing.SaveToFile(FileName + '\Frame' + IntToStr(Index + 1) + '.png');
         ProgressBar.StepIt;
         Application.ProcessMessages;
        end;
     (*
     for Index := 0 to 15 do
      begin
       Drawing.Draw(FReference, Drawing.Width div 4, Drawing.Height div 4, $8);
       Drawing.SaveToFile(FileName + '\Frame' + IntToStr(Length(FCircles) + Index + 1) + '.png');
       ProgressBar.StepIt;
       Application.ProcessMessages;
      end;
      *)
    end;
  finally
   if Assigned(Circle) then FreeAndNil(Circle);
   if Assigned(Drawing) then FreeAndNil(Drawing);
   Free;
  end;
end;

procedure TFmCircledPictureDialog.SaveDrawing(FileName: TFileName);
var
  Circles : TCircleChunkContainer;
  Circle  : TCircleChunk;
  Index   : Integer;
begin
 try
  Circles := TCircleChunkContainer.Create;

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

procedure TFmCircledPictureDialog.PaintBoxDrawPaint(Sender: TObject);
begin
 FBestDrawing.PaintTo(PaintBoxDraw.Canvas);
end;

procedure TFmCircledPictureDialog.PaintBoxRefPaint(Sender: TObject);
begin
 FReference.PaintTo(PaintBoxRef.Canvas);
end;

end.
