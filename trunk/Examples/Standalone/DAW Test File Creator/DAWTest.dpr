program DAWTest;

{$I DAV_Compiler.inc}
{$APPTYPE CONSOLE}
{$DEFINE PrintWeightsToFile}

uses
  {$IFNDEF FPC} Windows, {$ENDIF} SysUtils, Math, Classes, DAV_Types,
  DAV_Common, DAV_Approximations, DAV_ChunkClasses, DAV_DspSimpleOscillator,
  DAV_AudioData, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF,
  DAV_DifferentialEvolution;

type
  TTestType = (ttGeneric, ttExtreme, ttOptimized, ttFile);
  TWeight = Double;
  PWeight = ^TWeight;
  TWeightArray = array of TWeight;

  TWeightChunk = class (TDefinedChunk)
  private
    FWeightData : array of Extended;
  public
    constructor Create; override;
    class function GetClassChunkName : TChunkName; override;

    procedure AssignWeight(Weight: TWeightArray);
    procedure AssignToWeight(var Weight: TWeightArray);

    procedure SaveToStream(Stream: TStream); override;
    procedure LoadFromStream(Stream: TStream); override;
  end;

  TWeightOptimizer = class
  private
    FWeights : TWeightArray;
  public
    constructor Create; virtual;
    destructor Destroy; override;

    function CalculateCostEvent(Sender: TObject;
      var Population: TDifferentialEvolutionPopulation): Double;
  end;

var
  NumberOfFiles    : Integer = 128;
  TestType         : TTestType = ttGeneric;
  TestBits         : Integer = 32;
  OptimizeSteps    : Integer = 0;
  GenericScale     : Single = 0.95;
  FullScaleLevel   : Single = 0.97;
  InputFileName    : TFileName;
  SineFrequency    : Single = 1000;
  MasterSampleRate : Single = 44100;
  Duration         : Single = 1;
  WeightFileName   : TFileName;
  SampleCount      : Integer;

{ TWeightChunk }

constructor TWeightChunk.Create;
begin
  inherited;
end;

class function TWeightChunk.GetClassChunkName: TChunkName;
begin
  Result := 'DAWC';
end;

procedure TWeightChunk.AssignToWeight(var Weight: TWeightArray);
var
  Index : Integer;
begin
 SetLength(Weight, Length(FWeightData));
 for Index := 0 to Length(FWeightData) - 1 do
   Weight[Index] := FWeightData[Index];
end;

procedure TWeightChunk.AssignWeight(Weight: TWeightArray);
var
  Index : Integer;
begin
 SetLength(FWeightData, Length(Weight));
 for Index := 0 to Length(Weight) - 1 do
   FWeightData[Index] := Weight[Index];
end;

procedure TWeightChunk.LoadFromStream(Stream: TStream);
begin
  inherited;
  SetLength(FWeightData, ChunkSize);
  Stream.Read(FWeightData[0], ChunkSize * SizeOf(Extended));
end;

procedure TWeightChunk.SaveToStream(Stream: TStream);
begin
  FChunkSize := Length(FWeightData);
  inherited;
  Stream.Write(FWeightData[0], ChunkSize * SizeOf(Extended));
end;


{ TWeightOptimizer }

constructor TWeightOptimizer.Create;
begin
  inherited;
  SetLength(FWeights, NumberOfFiles);
end;

destructor TWeightOptimizer.Destroy;
begin
  inherited;
end;

procedure CalculateDifferences(Weights: TWeightArray; out AverageDifference,
  MaximumDifference: Extended);
var
  FileIndex    : Integer;
  FileIndexMax : Integer;
  WeightRev    : PWeight;
  SampleIndex  : Integer;
  Value        : array [0..1] of Single;
  Difference   : Extended;
  DiffSum      : Extended;
begin
  FileIndexMax := NumberOfFiles - 1;
  DiffSum := 0;
  MaximumDifference := 0;

  with TCustomSimpleOscillator64.Create do
  try
    Frequency := SineFrequency;
    for SampleIndex := 0 to (SampleCount div 4) - 1 do
    begin
      Value[0] := 0;
      Value[1] := 0;
      WeightRev := @Weights[FileIndexMax];
      for FileIndex := 0 to FileIndexMax do
      begin
        Value[0] := Value[0] + Sine * Weights[FileIndex];
        Value[1] := Value[1] + Sine * WeightRev^;
        Dec(WeightRev);
      end;

      Difference := Abs(Value[0] - Value[1]);
      DiffSum := DiffSum + Difference;
      if Difference > MaximumDifference then
        MaximumDifference := Difference;

      CalculateNextSample;
    end;
  finally
    Free;
  end;

  AverageDifference := DiffSum / SampleCount;
  Assert(AverageDifference <= MaximumDifference);
end;

procedure CalculateWeightsFromPopulation(var Weights: TWeightArray;
  Population: TDifferentialEvolutionPopulation);
var
  FileIndex  : Integer;
  WeightSum  : TWeight;
  TempWeight : Extended;
begin
  Assert(Length(Weights) = Length(Population) + 1);

  Weights[0] := 1;
  WeightSum := Weights[0];
  for FileIndex := 0 to NumberOfFiles - 2 do
  begin
    Weights[FileIndex + 1] := Weights[FileIndex] * Population[FileIndex];
    WeightSum := WeightSum + Weights[FileIndex];
  end;

  // pre-normalize
  TempWeight := FullScaleLevel / WeightSum;
  for FileIndex := 0 to NumberOfFiles - 1 do
    Weights[FileIndex] := TempWeight * Weights[FileIndex];
end;

function TWeightOptimizer.CalculateCostEvent(Sender: TObject;
  var Population: TDifferentialEvolutionPopulation): Double;
var
  FileIndex    : Integer;
  TempWeight   : TWeight;
  WeightSum    : TWeight;
  MaxDiff      : Extended;
  AvgDiff      : Extended;
begin
  FWeights[0] := 1;
  WeightSum := FWeights[0];
  for FileIndex := 0 to NumberOfFiles - 2 do
  begin
    if Population[FileIndex] > 1 then
      Population[FileIndex] := 1;
    if Population[FileIndex] < 0 then
      Population[FileIndex] := random;

    FWeights[FileIndex + 1] := FWeights[FileIndex] * Population[FileIndex];
    WeightSum := WeightSum + FWeights[FileIndex];
  end;

  // pre-normalize
  TempWeight := FullScaleLevel / WeightSum;
  for FileIndex := 0 to NumberOfFiles - 1 do
  begin
    FWeights[FileIndex] := TempWeight * FWeights[FileIndex];
    Assert(FWeights[FileIndex] >= 0);
    Assert(FWeights[FileIndex] < 1);
  end;

  CalculateDifferences(FWeights, AvgDiff, MaxDiff);
  Result := -(AvgDiff + 2 * MaxDiff) * SampleCount;
end;


{ Main Program }

procedure PrintWeights(Weights: TWeightArray; AvgDiff, MaxDiff: Extended); overload;
var
  FileIndex  : Integer;
begin
  for FileIndex := 0 to NumberOfFiles - 1 do
    WriteLn('Weight File #' + IntToStr(FileIndex + 1) + ': ' +
      FloatToStr(Weights[FileIndex]) + ' -> ' +
      FloatToStr(Amp_to_dB(Weights[FileIndex])) + ' dB');
  WriteLn('Average Error: ' + FloatToStr(AvgDiff));
  WriteLn('Maximum Error: ' + FloatToStr(MaxDiff) + ' = Bits: ' + FloatToStr(1 + Abs(Log2(MaxDiff + 1E-30))));
end;

procedure PrintWeights(Weights: TWeightArray; AvgDiff, MaxDiff: Extended; FileName: TFileName); overload;
var
  FileIndex  : Integer;
begin
  with TStringList.Create do
  try
    for FileIndex := 0 to NumberOfFiles - 1 do
      Add('Weight File #' + IntToStr(FileIndex + 1) + ': ' +
        FloatToStr(Weights[FileIndex]) + ' -> ' +
        FloatToStr(Amp_to_dB(Weights[FileIndex])) + ' dB');
    Add('Average Error: ' + FloatToStr(AvgDiff));
    Add('Maximum Error: ' + FloatToStr(MaxDiff) + ' = Bits: ' + FloatToStr(1 + Abs(Log2(MaxDiff + 1E-30))));
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure SaveWeightsToFile(Weights: TWeightArray; FileName: TFileName);
var
  BackupFile : TFileName;
begin
  BackupFile := ChangeFileExt(FileName, '.bak');

  // save backup file
  if FileExists(FileName) then
  begin
    if FileExists(BackupFile) then
      RenameFile(BackupFile, BackupFile + '.tmp');
    RenameFile(FileName, BackupFile);
    if FileExists(BackupFile + '.tmp') then
      DeleteFile(BackupFile + '.tmp');
  end;

  with TWeightChunk.Create do
  try
    AssignWeight(Weights);
    SaveToFile(FileName);
  finally
    Free;
  end;
end;

procedure CalculateOptimizedWeights(var Weights: TWeightArray);
var
  FileIndex    : Integer;
  Trial        : Integer;
  Costs        : Double;
  LastCosts    : Double;
  TimeStamp    : array [0..1] of TTime;
  DiffEvol     : TDifferentialEvolution;
  WeigthOpt    : TWeightOptimizer;
  MaxDiff      : Extended;
  AvgDiff      : Extended;
  WeightFile   : TFileName;
begin
  if OptimizeSteps = 0 then
    WriteLn('Running Optimizer (endlessly, break by pressing [CTRL]+C)')
  else
    WriteLn('Running Optimizer (this may take a while)...');
  DiffEvol := TDifferentialEvolution.Create(nil);
  WeigthOpt := TWeightOptimizer.Create;
  try
    DiffEvol.VariableCount := NumberOfFiles - 1;
    DiffEvol.PopulationCount := Round(10 * (NumberOfFiles + 1000 / NumberOfFiles));
    for FileIndex := 0 to DiffEvol.VariableCount - 1 do
    begin
      DiffEvol.MinConstraints[FileIndex] := 0;
      DiffEvol.MaxConstraints[FileIndex] := 1;
    end;
    DiffEvol.Initialize(True);
    DiffEvol.OnCalculateCosts := WeigthOpt.CalculateCostEvent;

    LastCosts := 0;
    TimeStamp[0] := Now;
    Trial := 0;

    // set weight file name and eventually backup if file already exists
    WeightFile := 'Weights' + IntToStr(NumberOfFiles);
    if FileExists(WeightFile + '.bin') then
    begin
      if FileExists(WeightFile + '.old') then
        DeleteFile(WeightFile + '.old');
      RenameFile(WeightFile + '.bin', WeightFile + '.old');
    end;

    repeat
      Costs := DiffEvol.Evolve;
      TimeStamp[1] := Now;

      if (Costs <> LastCosts) or ((TimeStamp[1] - TimeStamp[0]) > 1E-4) then
      begin
        WriteLn('Trial #' + IntToStr(Trial) + ', Costs: ' + FloatToStr(-Costs));
        if (OptimizeSteps = 0) and (Costs <> LastCosts) then
        begin
          CalculateWeightsFromPopulation(Weights, DiffEvol.GetBestPopulation);
          try
            SaveWeightsToFile(Weights, WeightFile + '.bin');
            {$IFDEF PrintWeightsToFile}
            CalculateDifferences(Weights, AvgDiff, MaxDiff);
            PrintWeights(Weights, AvgDiff, MaxDiff, WeightFile + '.txt');
            {$ENDIF}
          except
            WriteLn('Error writing file!')
          end;
        end;

        TimeStamp[0] := TimeStamp[1];
        LastCosts := Costs;
      end;
      Inc(Trial);
    until (OptimizeSteps > 0) and (Trial >= OptimizeSteps);

    CalculateWeightsFromPopulation(Weights, DiffEvol.GetBestPopulation);
    CalculateDifferences(Weights, AvgDiff, MaxDiff);
    PrintWeights(Weights, AvgDiff, MaxDiff);
    SaveWeightsToFile(Weights, WeightFile + '.bin');

    WriteLn('');
    WriteLn('Please press a key to continue!');
    ReadLn;
  finally
    FreeAndNil(DiffEvol);
    FreeAndNil(WeigthOpt);
  end;
end;

procedure CalculateWeights(var Weights: TWeightArray);
var
  FileIndex  : Integer;
  TempWeight : TWeight;
  WeightSum  : TWeight;
begin
  case TestType of
    ttGeneric :
      begin
        WeightSum := 0;
        for FileIndex := 0 to NumberOfFiles - 1 do
        begin
          Weights[FileIndex] := Power(2, -GenericScale * FileIndex) * (1 - (FileIndex + 1) / (NumberOfFiles + 1));
          WeightSum := WeightSum + Weights[FileIndex];
        end;

        // pre-normalize
        TempWeight := FullScaleLevel / WeightSum;
        for FileIndex := 0 to NumberOfFiles - 1 do
          Weights[FileIndex] := TempWeight * Weights[FileIndex];
      end;
    ttExtreme :
      begin
        Weights[0] := 1;
        WeightSum := Weights[0];
        TempWeight := Power(2, -TestBits);
        for FileIndex := 1 to NumberOfFiles - 1 do
        begin
          Weights[FileIndex] := TempWeight * 0.25 * (4 - FileIndex / NumberOfFiles);
          WeightSum := WeightSum + Weights[FileIndex];
        end;

        // pre-normalize
        TempWeight := FullScaleLevel / WeightSum;
        for FileIndex := 0 to NumberOfFiles - 1 do
          Weights[FileIndex] := TempWeight * Weights[FileIndex];
      end;
    ttOptimized :
      CalculateOptimizedWeights(Weights);
    ttFile :
      begin
        if FileExists(WeightFileName) then
          with TWeightChunk.Create do
          try
            LoadFromFile(WeightFileName);
            AssignToWeight(Weights);
            NumberOfFiles := Length(Weights);
          finally
            Free;
          end;
      end;
  end;
end;

procedure BuildFiles;
var
  FileIndex   : Integer;
  SampleIndex : Integer;
  Weights     : TWeightArray;
  Weight      : array [0..1] of TWeight;
  Data        : array [0..2] of PDAVSingleFixedArray;
  InputData   : TAudioDataCollection32;
  WavFileName : TFileName;
begin
  InputData := nil;
  with TAudioDataCollection32.Create(nil) do
  try
    ChannelCount := 2;
    SampleRate := MasterSampleRate;

    // set sampleframes (either from file or default)
    if FileExists(InputFileName) then
    begin
      InputData := TAudioDataCollection32.Create(nil);
      InputData.LoadFromFile(InputFileName);
      SampleFrames := InputData.SampleFrames;
      SampleCount := SampleFrames;

      Data[2] := InputData.ChannelDataPointer[0];
    end
    else
      SampleFrames := SampleCount;

    Data[0] := ChannelDataPointer[0];
    Data[1] := ChannelDataPointer[1];
    SetLength(Weights, NumberOfFiles);
    CalculateWeights(Weights);

    for FileIndex := 0 to NumberOfFiles - 1 do
    begin
      Weight[0] := Weights[FileIndex];
      Weight[1] := Weights[NumberOfFiles - 1 - FileIndex];
      Assert(Weight[0] >= 0);
      Assert(Weight[0] < 1);

      if Assigned(InputData) then
        for SampleIndex := 0 to SampleCount - 1 do
        begin
         Data[0]^[SampleIndex] := Data[2]^[SampleIndex] * Weight[0];
         Data[1]^[SampleIndex] := Data[2]^[SampleIndex] * Weight[1];
        end
      else
        with TCustomSimpleOscillator64.Create do
        try
          Frequency := SineFrequency;
          for SampleIndex := 0 to SampleCount - 1 do
          begin
           Data[0]^[SampleIndex] := Sine * Weight[0];
           Data[1]^[SampleIndex] := Sine * Weight[1];
           CalculateNextSample;
          end;
        finally
          Free;
        end;

      WavFileName := 'DAW Test File ';
      if FileIndex + 1 < 10 then WavFileName := WavFileName + '0';
      if FileIndex + 1 < 100 then WavFileName := WavFileName + '0';
      WavFileName := WavFileName + IntToStr(FileIndex + 1) + '.wav';

      SaveToFile(WavFileName, 32, aeFloat);
    end;
  finally
    if Assigned(InputData) then
      FreeAndNil(InputData);
    Free;
  end;
end;

function ScanParameters: Boolean;
var
  ParamIndex : Integer;
  Current    : string;
begin
  Result := True;
  for ParamIndex := 1 to ParamCount do
  begin
    Current := ParamStr(ParamIndex);
    if (Length(Current) >= 2) and
      {$IFDEF FPC}
      (Current[1] in ['/', '-']) then
      {$ELSE}
      CharInSet(Current[1], ['/', '-']) then
      {$ENDIF}
    case Current[2] of
      '?', 'h' :
        begin
          WriteLn('Usage: ' + ExtractFileName(ParamStr(0)) + ' [/option]');
          WriteLn('');
          WriteLn('Options: ');
          WriteLn('  /d___     Specify duration in seconds');
          WriteLn('  /f___     Frequency of the sine wave used for all tests');
          WriteLn('  /g___     Use generic test (with spread factor, default 0.95)');
          WriteLn('  /i___     Use specified input file as source');
          WriteLn('  /l___     Normalize level in dB');
          WriteLn('  /n___     Build a specified number of files (Default: /f128)');
          WriteLn('  /s___     Specify Sample Rate');
          WriteLn('  /r___     Recall coefficients from specified file');
          WriteLn('  /g___     Use test optimized for a maximum error (Parameter: Opt. Steps)');
          WriteLn('  /x__      Use extreme test custom tailored for specified bits');
          Result := False;
          Exit;
        end;
      'd' :
        begin
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            Duration := StrToFloat(Current);
        end;
      'f' :
        begin
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            SineFrequency := StrToFloat(Current);
        end;
      'g' :
        begin
          TestType := ttGeneric;
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            GenericScale := StrToFloat(Current);
        end;
      'i' :
        begin
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            InputFileName := Current;
        end;
      'l' :
        begin
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            FullScaleLevel := dB_to_Amp(StrToFloat(Current));
        end;
      'n' :
        begin
          Current := Copy(Current, 3, Length(Current));
          NumberOfFiles := StrToInt(Current);
        end;
      'o' :
        begin
          TestType := ttOptimized;
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            OptimizeSteps := StrToInt(Current);
        end;
      'r' :
        begin
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
          begin
            WeightFileName := Current;
            if FileExists(WeightFileName) then
              TestType := ttFile;
          end;
        end;
      's' :
        begin
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            MasterSampleRate := StrToFloat(Current);
        end;
      'w' :
        begin
          // read weights here
        end;
      'x' :
        begin
          TestType := ttExtreme;
          Current := Copy(Current, 3, Length(Current));
          if Current <> '' then
            TestBits := StrToInt(Current);
        end;
    end;
  end;

  // initialize SampleCount
  SampleCount := Round(Duration * MasterSampleRate);
end;

{$R *.res}

begin
  try
    if ScanParameters then
      BuildFiles;
  except
    on E: Exception do
      WriteLn(E.ClassName, ': ', E.Message);
  end;
end.
