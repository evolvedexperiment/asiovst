unit LoudnessMeterDSP;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs,
  DAV_Types, DAV_VSTModule, DAV_DspFilter, DAV_DspFilterBasics,
  DAV_DspDelayLines, DAV_DspR128;

type
  PPLinkedLoudnessRecord = ^PLinkedLoudnessRecord;
  PLinkedLoudnessRecord = ^TLinkedLoudnessRecord;
  TLinkedLoudnessRecord = record
    Loudness : Single;
    Value    : Single;
    Next     : PLinkedLoudnessRecord;
  end;

  TLoudnessMeterModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterScaleChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterScaleDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterStateChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterStateDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterTimeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLoudnessDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterPeakMomDisplay(Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
    procedure ParameterPeakMomChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLoudnessChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FShortIntSum         : Double;
    FMomIntSum           : Double;
    FAbsoluteGatedSum    : Double;
    FShortIntScale       : Double;
    FMomIntScale         : Double;
    FPeakHold            : Double;
    FShortIntValue       : array [0..1] of Double;
    FMomIntValue         : array [0..1] of Double;
    FAbsoluteGatedValue  : array [0..1] of Double;
    FAbsoluteGatedCount  : array [0..1] of Integer;

    FPreFilter           : array [0..1] of TBiquadIIRFilter;
    FRLBFilter           : array [0..1] of TBiquadIIRFilter;
    FDelayLine400ms      : array [0..1] of TDelayLineSamples32;
    FDelayLine2600ms     : array [0..1] of TDelayLineSamples32;
    FSampleCount         : Integer;
    FOverlapSamples      : Integer;
    FTotalSamples        : Integer;
    FUpdateSampleCount   : Integer;
    FUpdateSamples       : Integer;
    FIsRunning           : Boolean;
    FLinkedLoudness      : array [0..1] of PLinkedLoudnessRecord;
    FUnitOffset          : Single;
    FLoudness            : Single;
    FPeakHoldLoudness    : Single;
  protected
    FCriticalSection  : TCriticalSection;
    function GetLoudnessShort: Single;
    function GetLoudnessMomentary: Single;
    function GetLoudnessIntegration: Single;
    procedure ClearLinkedLoudness;
    procedure ProcessLongTermSample(const Index: Integer; Value: Single);

    procedure UpdateLoudness;
    procedure UpdatePeak;
  public
    procedure ChooseProcess;
    procedure ResetPeak;

    property LoudnessShort: Single read GetLoudnessShort;
    property LoudnessMomentary: Single read GetLoudnessMomentary;
    property LoudnessIntegration: Single read GetLoudnessIntegration;
    property Loudness: Single read FLoudness;
    property UnitOffset: Single read FUnitOffset;
    property PeakHold: Single read FPeakHoldLoudness;
    property TotalSamples: Integer read FTotalSamples;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Common, DAV_Approximations, LoudnessMeterGUI;

const
  CMeanSquareBias : Single = 1E-10;


{ TLoudnessMeterModule }

procedure TLoudnessMeterModule.VSTModuleCreate(Sender: TObject);
begin
 FCriticalSection := TCriticalSection.Create;
end;

procedure TLoudnessMeterModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TLoudnessMeterModule.VSTModuleOpen(Sender: TObject);
var
  ChannelIndex : Integer;
  BufferSize   : array [0..1] of Integer;
begin
 // calculate buffer sizes
 if Abs(SampleRate) = 0 then
  begin
   BufferSize[0] := 17640;
   BufferSize[1] := 114660;
  end
 else
  begin
   BufferSize[0] := Round(0.4 * Abs(SampleRate));
   BufferSize[1] := Round(2.6 * Abs(SampleRate));
  end;

 FMomIntScale := 1 / BufferSize[0];
 FShortIntScale := 1 / (BufferSize[0] + BufferSize[1]);
 FOverlapSamples := BufferSize[0] div 4;

 // create classes
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FPreFilter[ChannelIndex] := TBasicHighShelfFilter.Create;
   with FPreFilter[ChannelIndex] do
    begin
     if Abs(Self.SampleRate) > 0
      then SampleRate := Self.SampleRate;
     Frequency := 1500;
     Gain := 4;
     Bandwidth := 1.895;
    end;

   FRLBFilter[ChannelIndex] := TBasicLowcutFilter.Create;
   with FRLBFilter[ChannelIndex] do
    begin
     if Abs(Self.SampleRate) > 0
      then SampleRate := Self.SampleRate;
     Frequency := 38;
     Bandwidth := 2.54;
    end;

   FDelayLine400ms[ChannelIndex] := TDelayLineSamples32.Create(BufferSize[0]);
   FDelayLine2600ms[ChannelIndex] := TDelayLineSamples32.Create(BufferSize[1]);

   FShortIntValue[ChannelIndex] := 0;
   FMomIntValue[ChannelIndex] := 0;
  end;

 FMomIntSum := 0;
 FShortIntSum := 0;
 FAbsoluteGatedSum := 0;

 ChooseProcess;
 ResetPeak;

 // initialize parameters
 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;

 // set editor form class
 EditorFormClass := TFmLoudnessMeter;
end;

procedure TLoudnessMeterModule.VSTModuleClose(Sender: TObject);
var
  ChannelIndex : Integer;
begin
 for ChannelIndex := 0 to numInputs - 1 do
  begin
   FreeAndNil(FPreFilter[ChannelIndex]);
   FreeAndNil(FRLBFilter[ChannelIndex]);
   FreeAndNil(FDelayLine400ms[ChannelIndex]);
   FreeAndNil(FDelayLine2600ms[ChannelIndex]);
  end;
 ClearLinkedLoudness;
end;

{ Parameters }

procedure TLoudnessMeterModule.ParameterScaleChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Round(Value) = 0
  then FUnitOffset := 23.056476593
  else FUnitOffset := 0.056476593;

 FUpdateSamples := 0;

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateScale;
end;

procedure TLoudnessMeterModule.ParameterTimeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case Round(Value) of
  0 : FUpdateSampleCount := Round(0.1 * SampleRate);
  1 : FUpdateSampleCount := Round(SampleRate);
  2 : FUpdateSampleCount := Round(SampleRate);
 end;

 FUpdateSamples := 0;

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateTime;
end;

procedure TLoudnessMeterModule.ParameterStateChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FIsRunning := Round(Value) = 1;

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateState;
end;

procedure TLoudnessMeterModule.ParameterStateDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Stopped';
  1 : PreDefined := 'Running';
 end;
end;

procedure TLoudnessMeterModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 case Round(Parameter[Index]) of
  0 : PreDefined := 'Mom';
  1 : PreDefined := 'Short';
  2 : PreDefined := 'Int';
 end;
end;

procedure TLoudnessMeterModule.ParameterScaleDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
begin
 if Round(Parameter[Index]) = 0
  then PreDefined := 'LU'
  else PreDefined := 'LUFS';
end;

function TLoudnessMeterModule.GetLoudnessIntegration: Single;
var
  ChannelIndex : Integer;
  Value        : array [0..1] of Single;
  LLRec        : PLinkedLoudnessRecord;
  Loudness     : Single;
  ItemCount    : Integer;
begin
 // calculate integrated loudness level
 if FAbsoluteGatedCount[0] = 0
  then Value[0] := CMeanSquareBias
  else Value[0] := FAbsoluteGatedValue[0] / FAbsoluteGatedCount[0];
 if FAbsoluteGatedCount[1] = 0
  then Value[1] := CMeanSquareBias
  else Value[1] := FAbsoluteGatedValue[1] / FAbsoluteGatedCount[1];
 Loudness := -0.691 + 10 * FastLog10ContinousError3(Value[0] + Value[1]) - 8;

 // calculate integrated loudness level
 for ChannelIndex := 0 to 1 do
  begin
   ItemCount := 0;
   Value[ChannelIndex] := 0;
   LLRec := FLinkedLoudness[ChannelIndex];
   while Assigned(LLRec) do
    begin
     if LLRec^.Loudness < Loudness
      then Break;

     Inc(ItemCount);
     Value[ChannelIndex] := Value[ChannelIndex] + LLRec^.Value;
     LLRec := LLRec^.Next;
    end;
   if ItemCount = 0
    then Value[ChannelIndex] := CMeanSquareBias
    else Value[ChannelIndex] := Value[ChannelIndex] / ItemCount;
  end;

 Result := -0.691 + 10 * FastLog10ContinousError3(Value[0] + Value[1]);
end;

function TLoudnessMeterModule.GetLoudnessMomentary: Single;
begin
 Result := -0.691 + 10 * FastLog10ContinousError3(FMomIntSum);
end;

function TLoudnessMeterModule.GetLoudnessShort: Single;
begin
 Result := -0.691 + 10 * FastLog10ContinousError3(FShortIntSum);
end;

procedure TLoudnessMeterModule.ResetPeak;
begin
 FPeakHold := 0;
 FSampleCount := 0;
 FTotalSamples := 0;

 ClearLinkedLoudness;

 FAbsoluteGatedValue[0] := 0;
 FAbsoluteGatedValue[1] := 0;
 FAbsoluteGatedCount[0] := 0;
 FAbsoluteGatedCount[1] := 0;
end;

procedure TLoudnessMeterModule.UpdateLoudness;
begin
 case Round(Parameter[1]) of
  0 : FLoudness := GetLoudnessMomentary;
  1 : FLoudness := GetLoudnessShort;
  2 : FLoudness := GetLoudnessIntegration;
 end;

 // update parameter
 Parameter[3] := Limit(23 + FLoudness, -18, 9);

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdateLoudness;
end;

procedure TLoudnessMeterModule.UpdatePeak;
begin
 FPeakHoldLoudness := -0.691 + 10 * FastLog10ContinousError3(FPeakHold);

 // update parameter
 Parameter[4] := Limit(23 + FPeakHoldLoudness, -18, 9);

 // update GUI
 if EditorForm is TFmLoudnessMeter
  then TFmLoudnessMeter(EditorForm).UpdatePeak;
end;

procedure TLoudnessMeterModule.ClearLinkedLoudness;
var
  ChannelIndex       : Integer;
  LinkedLoudness     : PLinkedLoudnessRecord;
  ThisLinkedLoudness : PLinkedLoudnessRecord;
begin
 for ChannelIndex := 0 to 1 do
  begin
   LinkedLoudness := FLinkedLoudness[ChannelIndex];
   FLinkedLoudness[ChannelIndex] := nil;

   while Assigned(LinkedLoudness) do
    begin
     ThisLinkedLoudness := LinkedLoudness;
     LinkedLoudness := LinkedLoudness.Next;
     FreeMem(ThisLinkedLoudness);
    end;
  end;
end;

procedure TLoudnessMeterModule.ParameterLoudnessDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Value : Single;
begin
 Value := RoundTo(23 + Loudness, -1);

 // correct FS scale
 if Round(Parameter[0]) = 1
  then PreDefined := FloatToAnsiString(Value - 23, 4)
  else PreDefined := FloatToAnsiString(Value, 4);

 if Value < -18.1 then PreDefined := 'LOW' else
 if Value > 9 then PreDefined := 'OVER';
end;

procedure TLoudnessMeterModule.ParameterPeakMomDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: AnsiString);
var
  Value : Single;
begin
 Value := RoundTo(23 + PeakHold, -1);

 // correct FS scale
 if Round(Parameter[0]) = 1
  then PreDefined := FloatToAnsiString(Value - 23, 4)
  else PreDefined := FloatToAnsiString(Value, 4);

 if Value < -18.1 then PreDefined := 'LOW' else
 if Value > 9 then PreDefined := 'OVER';
end;

procedure TLoudnessMeterModule.ParameterLoudnessChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := Limit(23 + FLoudness, -18, 9);
end;

procedure TLoudnessMeterModule.ParameterPeakMomChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := Limit(23 + FPeakHoldLoudness, -18, 9);
end;

procedure TLoudnessMeterModule.ChooseProcess;
begin
 case numInputs of
   1 : OnProcess := VSTModuleProcessMono;
   2 : OnProcess := VSTModuleProcessStereo;
  else raise Exception.Create('Number of channels not supported');
 end;

 OnProcess32Replacing := OnProcess;
end;

procedure TLoudnessMeterModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
var
  ChannelIndex : Integer;
  BufferSize   : array [0..1] of Integer;
begin
 if Abs(Self.SampleRate) > 0 then
  begin
   BufferSize[0] := Round(0.4 * Abs(SampleRate));
   BufferSize[1] := Round(2.6 * Abs(SampleRate));

   FMomIntScale := 1 / BufferSize[0];
   FShortIntScale := 1 / (BufferSize[0] + BufferSize[1]);

   for ChannelIndex := 0 to numInputs - 1 do
    begin
     FPreFilter[ChannelIndex].SampleRate := Abs(SampleRate);
     FRLBFilter[ChannelIndex].SampleRate := Abs(SampleRate);
     FDelayLine400ms[ChannelIndex].BufferSize := BufferSize[0];
     FDelayLine2600ms[ChannelIndex].BufferSize := BufferSize[1];
    end;

   FOverlapSamples := BufferSize[0] div 4;
  end;
end;

procedure TLoudnessMeterModule.ProcessLongTermSample(const Index: Integer; Value: Single);
var
  Loudness    : Single;
  LLRec       : PLinkedLoudnessRecord;
  LinkPointer : PPLinkedLoudnessRecord;
begin
 // calculate loudness
 Loudness := -0.691 + 10 * FastLog10ContinousError3(Value);

 // check for absolute threshold
 if Loudness < -70 then Exit;

 // allocate memory for loudness record
 GetMem(LLRec, SizeOf(TLinkedLoudnessRecord));

 // set loudness / Value
 LLRec^.Loudness := Loudness;
 LLRec^.Value := Value;

 // search for matching linked loudness position
 LinkPointer := @FLinkedLoudness[Index];

 // update total sum
 FAbsoluteGatedValue[Index] := FAbsoluteGatedValue[Index] + Value;
 Inc(FAbsoluteGatedCount[Index]);

 // insert loudness in ordered list
 repeat
  if LinkPointer^ = nil then
   begin
    LinkPointer^ := LLRec;
    LLRec^.Next := nil;
    Exit;
   end else
  if (LinkPointer^)^.Loudness < Loudness then
   begin
    LLRec.Next := LinkPointer^;
    LinkPointer^ := LLRec;
    Exit;
   end;
  LinkPointer := @((LinkPointer^)^.Next);
 until False;
end;

procedure TLoudnessMeterModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  Value400ms   : Single;
  CurrentValue : Single;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   CurrentValue :=
     FRLBFilter[0].ProcessSample32(
     FPreFilter[0].ProcessSample32(Inputs[0, SampleIndex]));

   // temporarily copy to output
   Outputs[0, SampleIndex] := CurrentValue;

   // calculate momentary integration (400 ms)
   Value400ms := FDelayLine400ms[0].ProcessSample32(CurrentValue);
   FMomIntValue[0] := FMomIntValue[0] + Sqr(CurrentValue) - Sqr(Value400ms);
   if FMomIntValue[0] < CMeanSquareBias
    then FMomIntValue[0] := CMeanSquareBias;
   FMomIntSum := CMeanSquareBias + FMomIntValue[0] * FMomIntScale;

   // calculate short integration (3 seconds)
   FShortIntValue[0] := FShortIntValue[0] + Sqr(CurrentValue) -
     Sqr(FDelayLine2600ms[0].ProcessSample32(Value400ms));
   FShortIntSum := FShortIntValue[0] * FShortIntScale;

   // eventually process long term sample
   Inc(FSampleCount);
   if FSampleCount >= FOverlapSamples then
    begin
     FSampleCount := 0;
     if FIsRunning
      then ProcessLongTermSample(0, FMomIntValue[0] * FMomIntScale);
    end;

   // eventually increase total number of samples
   if FIsRunning then Inc(FTotalSamples);

   // override peak hold if necessary
   if FMomIntSum > FPeakHold then
    begin
     FPeakHold := FMomIntSum;
     UpdatePeak;
    end;

   // eventually update loudness
   Dec(FUpdateSamples);
   if FUpdateSamples < 0 then
    begin
     FUpdateSamples := FUpdateSampleCount - 1;
     UpdateLoudness;
    end;
  end;
end;

procedure TLoudnessMeterModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  SampleIndex  : Integer;
  ChannelIndex : Integer;
  CurrentValue : Single;
  Value400ms   : Single;
begin
 for SampleIndex := 0 to SampleFrames - 1 do
  begin
   for ChannelIndex := 0 to 1 do
    begin
     // preprocess left channel
     CurrentValue :=
       FRLBFilter[ChannelIndex].ProcessSample32(
       FPreFilter[ChannelIndex].ProcessSample32(Inputs[ChannelIndex, SampleIndex]));

     // calculate momentary integration (400 ms)
     Value400ms := FDelayLine400ms[ChannelIndex].ProcessSample32(CurrentValue);
     FMomIntValue[ChannelIndex] := FMomIntValue[ChannelIndex] + Sqr(CurrentValue) - Sqr(Value400ms);
     if FMomIntValue[ChannelIndex] < CMeanSquareBias
      then FMomIntValue[ChannelIndex] := CMeanSquareBias;

     // calculate short integration (3 seconds)
     FShortIntValue[ChannelIndex] := FShortIntValue[ChannelIndex] + Sqr(CurrentValue) -
       Sqr(FDelayLine2600ms[ChannelIndex].ProcessSample32(Value400ms));
     if FShortIntValue[ChannelIndex] < CMeanSquareBias
      then FShortIntValue[ChannelIndex] := CMeanSquareBias;

     // temporarily copy to output
     Outputs[ChannelIndex, SampleIndex] := CurrentValue;
    end;

   // calculate sum
   FMomIntSum := CMeanSquareBias + (FMomIntValue[0] + FMomIntValue[1]) * FMomIntScale;
   FShortIntSum := CMeanSquareBias + (FShortIntValue[0] + FShortIntValue[1]) * FShortIntScale;

   // eventually process long term sample
   Inc(FSampleCount);
   if FSampleCount >= FOverlapSamples then
    begin
     FSampleCount := 0;
     if FIsRunning then
      begin
       ProcessLongTermSample(0, FMomIntValue[0] * FMomIntScale);
       ProcessLongTermSample(1, FMomIntValue[1] * FMomIntScale);
      end;
    end;

   // eventually increase total number of samples
   if FIsRunning then Inc(FTotalSamples);

   // override peak hold if necessary
   if FMomIntSum > FPeakHold then
    begin
     FPeakHold := FMomIntSum;
     UpdatePeak;
    end;

   // eventually update loudness
   Dec(FUpdateSamples);
   if FUpdateSamples < 0 then
    begin
     FUpdateSamples := FUpdateSampleCount - 1;
     UpdateLoudness;
    end;
  end;
end;

end.
