program DAWTest;

{$APPTYPE CONSOLE}

uses
  SysUtils, Math, Classes, DAV_Types, DAV_Common, DAV_DspSimpleOscillator,
  DAV_AudioData, DAV_AudioFile, DAV_AudioFileWAV, DAV_AudioFileAIFF,
  DAV_DifferentialEvolution, DAV_ChunkClasses;

type
  TTestType = (ttGeneric, ttExtreme, ttOptimized);
  TWeight = Double;
  TWeightArray = array of TWeight;

  TWeightChunk = class (TDefinedChunk)
  private
    FWeightData : array of Extended;
  public
    constructor Create; override;
    class function GetClassChunkName : TChunkName; override;

    procedure AssignWeight(Weight: TWeightArray);

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
  OptimizeSteps    : Integer = 100;
  GenericScale     : Single = 0.95;
  FullScaleLevel   : Single = 0.97;
  InputFileName    : TFileName;
  SineFrequency    : Single = 1000;
  MasterSampleRate : Single = 44100;
  Duration         : Single = 1;
  SampleCount      : Integer;

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
    for SampleIndex := 0 to SampleCount - 1 do
    begin
      Value[0] := 0;
      Value[1] := 0;
      for FileIndex := 0 to FileIndexMax do
      begin
        Value[0] := Value[0] + Sine * Weights[FileIndex];
        Value[1] := Value[1] + Sine * Weights[FileIndexMax - FileIndex];
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
      Population[FileIndex] := Random;

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
  Result := -(AvgDiff + SampleCount * MaxDiff);
end;


{ TWeightChunk }

constructor TWeightChunk.Create;
begin
  inherited;
end;

class function TWeightChunk.GetClassChunkName: TChunkName;
begin
  Result := 'DAWC';
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


{ Main Program }

procedure CalculateOptimizedWeights(var Weights: TWeightArray);
var
  FileIndex  : Integer;
  Trial      : Integer;
  Costs      : Double;
  LastCosts  : Double;
  ShowInfo   : Boolean;
  TimeStamp  : array [0..1] of TTime;
  DiffEvol   : TDifferentialEvolution;
  WeigthOpt  : TWeightOptimizer;
  WeightSum  : TWeight;
  TempWeight : TWeight;
  MaxDiff    : Extended;
  AvgDiff    : Extended;
begin
  Writeln('Running Optimizer (this may take a while)...');
  DiffEvol := TDifferentialEvolution.Create(nil);
  WeigthOpt := TWeightOptimizer.Create;
  try
    DiffEvol.VariableCount := NumberOfFiles - 1;
    DiffEvol.PopulationCount := 10 * DiffEvol.VariableCount;
    for FileIndex := 0 to DiffEvol.VariableCount - 1 do
    begin
      DiffEvol.MinConstraints[FileIndex] := 0;
      DiffEvol.MaxConstraints[FileIndex] := 1;
    end;
    DiffEvol.Initialize(True);
    DiffEvol.OnCalculateCosts := WeigthOpt.CalculateCostEvent;

    LastCosts := 0;
    TimeStamp[0] := Now;
    for Trial := 0 to OptimizeSteps - 1 do
    begin
      Costs := DiffEvol.Evolve;
      TimeStamp[1] := Now;

      if (Costs <> LastCosts) then
      begin
        LastCosts := Costs;
        ShowInfo := True;
      end
      else
        ShowInfo := (TimeStamp[1] - TimeStamp[0]) > 1E-4;

      if ShowInfo then
      begin
        Writeln('Trial #' + IntToStr(Trial) + ', Costs: ' + FloatToStr(-Costs));
        TimeStamp[0] := TimeStamp[1];
      end;
    end;

    Weights[0] := 1;
    WeightSum := Weights[0];
    for FileIndex := 0 to NumberOfFiles - 2 do
    begin
      Weights[FileIndex + 1] := Weights[FileIndex] * DiffEvol.BestPopulation[FileIndex];
      WeightSum := WeightSum + Weights[FileIndex];
    end;

    // pre-normalize
    TempWeight := FullScaleLevel / WeightSum;
    for FileIndex := 0 to NumberOfFiles - 1 do
    begin
      Weights[FileIndex] := TempWeight * Weights[FileIndex];
      Writeln('Weight File #' + IntToStr(FileIndex + 1) + ': ' +
        FloatToStr(Weights[FileIndex]) + ' -> ' +
        FloatToStr(Amp_to_dB(Weights[FileIndex])) + ' dB');
    end;

    CalculateDifferences(Weights, AvgDiff, MaxDiff);
    WriteLn('Average Error: ' + FloatToStr(AvgDiff));
    WriteLn('Maximum Error: ' + FloatToStr(MaxDiff) + ' = Bits: ' + FloatToStr(1 + Abs(Log2(MaxDiff + 1E-30))));
    with TWeightChunk.Create do
    try
      AssignWeight(Weights);
      SaveToFile('Weights.bin');
    finally
      Free;
    end;
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

      SaveToFile(WavFileName);
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
    if (Length(Current) >= 2) and CharInSet(Current[1], ['/', '-']) then
    case Current[2] of
      '?', 'h' :
        begin
          Writeln('Usage: ' + ExtractFileName(ParamStr(0)) + ' [/option]');
          Writeln('');
          Writeln('Options: ');
          Writeln('  /d___     Specify duration in seconds');
          Writeln('  /f___     Frequency of the sine wave used for all tests');
          Writeln('  /g___     Use generic test (with spread factor, default 0.95)');
          Writeln('  /i___     Use specified input file as source');
          Writeln('  /l___     Normalize level in dB');
          Writeln('  /n___     Build a specified number of files (Default: /f128)');
          Writeln('  /s___     Specify Sample Rate');
          Writeln('  /g___     Use test optimized for a maximum error (Parameter: Opt. Steps)');
          Writeln('  /x__      Use extreme test custom tailored for specified bits');
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

begin
  try
    if ScanParameters then
      BuildFiles;
  except
    on E: Exception do
      Writeln(E.ClassName, ': ', E.Message);
  end;
end.
