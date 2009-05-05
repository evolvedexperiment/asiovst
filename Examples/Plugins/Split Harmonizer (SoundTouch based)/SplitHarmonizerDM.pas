unit SplitHarmonizerDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, SyncObjs, DAV_Common,
  DAV_VSTModule, DAV_SoundTouchDLLResource, DAV_DspDelayLines; //DAV_SoundTouch;

const
  CInputDelay = 5248;

type
  TSplitHarmonizerModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessLR(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMS(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessReplacing64LR(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessReplacing64MS(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterDelayAChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterDelayBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEncodeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterEncodeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixLeftChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMixRightChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSemiTonesAChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSemiTonesBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure ParameterUseAntiAliasChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterUseQuickSeekChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleDestroy(Sender: TObject);
  private
    FSoundTouch      : array [0..1] of TSoundTouch;
    FDelayLine       : array [0..1, 0..1] of TDelayLineSamples32;
    FMix             : array [0..1] of Single;
    FCriticalSection : TCriticalSection;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, Dialogs, SplitHarmonizerGUI, DAV_VSTCustomModule, DAV_VSTPrograms;

procedure TSplitHarmonizerModule.VSTModuleCreate(Sender: TObject);
begin
 InitialDelay := CInputDelay;
 FCriticalSection := TCriticalSection.Create;
end;

procedure TSplitHarmonizerModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(FCriticalSection);
end;

procedure TSplitHarmonizerModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSoundTouch) - 1 do
  begin
   FSoundTouch[Channel] := TSoundTouch.Create;
   FDelayLine[Channel, 0] := TDelayLineSamples32.Create;
   FDelayLine[Channel, 0].BufferSize := CInputDelay;
   FDelayLine[Channel, 1] := TDelayLineSamples32.Create;
  end;
 Parameter[0] := 0;
 Parameter[1] := -20;
 Parameter[2] := 10;
 Parameter[3] := 0;
 Parameter[4] := +20;
 Parameter[5] := 10;
 Parameter[6] := 0;
 Parameter[7] := 1;
 Parameter[8] := 0;

 Programs[0].SetParameters(FParameter);
 with Programs[1] do SetParameters([0, -9, 13.5, 52.5,   9, 14.1, 50.0, 1, 0]);
 with Programs[2] do SetParameters([1, -6, 23.1, 52.5,   6, 21.6, 50.0, 1, 0]);
 with Programs[3] do SetParameters([0, 16, 48.2, 13.5, -13, 79.1, 63.5, 1, 0]);
end;

procedure TSplitHarmonizerModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to NumInputs - 1
  do FreeAndNil(FSoundTouch[Channel]);
end;

procedure TSplitHarmonizerModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TFmSplitHarmonizer.Create(Self);
end;

procedure TSplitHarmonizerModule.ParameterSemiTonesAChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FSoundTouch[0])
   then FSoundTouch[0].Pitch := Power(2, Value / 1200);
 finally
  FCriticalSection.Leave;
 end;

 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateSemitones(0);
end;

procedure TSplitHarmonizerModule.ParameterSemiTonesBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FSoundTouch[1])
   then FSoundTouch[1].Pitch := Power(2, Value / 1200);
 finally
  FCriticalSection.Leave;
 end;

 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateSemitones(1);
end;

procedure TSplitHarmonizerModule.ParameterUseAntiAliasChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FSoundTouch[0])
   then FSoundTouch[0].UseAntiAliasFilter := Value > 0.5;
  if assigned(FSoundTouch[1])
   then FSoundTouch[1].UseAntiAliasFilter := Value > 0.5;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterUseQuickSeekChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FSoundTouch[0])
   then FSoundTouch[0].UseQuickSeek := Value > 0.5;
  if assigned(FSoundTouch[1])
   then FSoundTouch[1].UseQuickSeek := Value > 0.5;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.ParameterDelayAChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FDelayLine[0, 1])
   then FDelayLine[0, 1].BufferSize := Max(1, round(Value * SampleRate * 1E-3));
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateDelay(0);
end;

procedure TSplitHarmonizerModule.ParameterDelayBChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCriticalSection.Enter;
 try
  if assigned(FDelayLine[1, 1])
   then FDelayLine[1, 1].BufferSize := Max(1, round(Value * SampleRate * 1E-3));
 finally
  FCriticalSection.Leave;
 end;
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateDelay(1);
end;

procedure TSplitHarmonizerModule.ParameterMixLeftChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[0] := 0.01 * Value;
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateMix(0);
end;

procedure TSplitHarmonizerModule.ParameterMixRightChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FMix[1] := 0.01 * Value;
 if EditorForm is TFmSplitHarmonizer
  then TFmSplitHarmonizer(EditorForm).UpdateMix(1);
end;

procedure TSplitHarmonizerModule.ParameterEncodeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 case round(Value) of
  0 : begin
       OnProcess := VSTModuleProcessLR;
       OnProcessDoubleReplacing := VSTModuleProcessReplacing64LR;
      end;
  1 : begin
       OnProcess := VSTModuleProcessMS;
       OnProcessDoubleReplacing := VSTModuleProcessReplacing64MS;
      end;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TSplitHarmonizerModule.ParameterEncodeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'L/R';
  1 : PreDefined := 'M/S';
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessLR(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
  Mix     : array [0..1, 0..1] of Single;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);

    FSoundTouch[Channel].PutSamples(@Inputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := Mix[0, 0] * FDelayLine[0, 0].ProcessSample(Inputs[0, Sample]) + Mix[0, 1] * FDelayLine[0, 1].ProcessSample(Outputs[0, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample(Inputs[1, Sample]) + Mix[1, 1] * FDelayLine[1, 1].ProcessSample(Outputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessReplacing64LR(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
  Mix     : array [0..1, 0..1] of Double;
begin
 FCriticalSection.Enter;
 try
  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);

    FSoundTouch[Channel].PutSamples(@Inputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := Mix[0, 0] * FDelayLine[0, 0].ProcessSample(Inputs[0, Sample]) + Mix[0, 1] * FDelayLine[0, 1].ProcessSample(Outputs[0, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample(Inputs[1, Sample]) + Mix[1, 1] * FDelayLine[1, 1].ProcessSample(Outputs[1, Sample]);
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessMS(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
  Temp    : Single;
  Mix     : array [0..1, 0..1] of Single;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FDelayLine[0, 1].ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[1, Sample] := FDelayLine[1, 1].ProcessSample(CHalf32 * (Inputs[0, Sample] - Inputs[1, Sample]));
   end;

  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);
    FSoundTouch[Channel].PutSamples(@Outputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Temp := Mix[0, 0] * FDelayLine[0, 0].ProcessSample(Inputs[0, Sample]) + Mix[0, 1] * (Outputs[0, Sample] + Outputs[1, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample(Inputs[1, Sample]) + Mix[1, 1] * (Outputs[0, Sample] - Outputs[1, Sample]);
    Outputs[0, Sample] := Temp;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleProcessReplacing64MS(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  Channel : Integer;
  Sample  : Integer;
  Temp    : Double;
  Mix     : array [0..1, 0..1] of Double;
begin
 FCriticalSection.Enter;
 try
  for Sample := 0 to SampleFrames - 1 do
   begin
    Outputs[0, Sample] := FDelayLine[0, 1].ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
    Outputs[1, Sample] := FDelayLine[1, 1].ProcessSample(CHalf32 * (Inputs[0, Sample] - Inputs[1, Sample]));
   end;

  for Channel := 0 to NumInputs - 1 do
   begin
    Mix[Channel, 0] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) - FMix[Channel] + CHalf32);
    Mix[Channel, 1] := CHalf32 * (1 - CHalf32 * abs(FMix[Channel]) + FMix[Channel] + CHalf32);
    FSoundTouch[Channel].PutSamples(@Outputs[Channel, 0], SampleFrames);
    FSoundTouch[Channel].ReceiveSamples(@Outputs[Channel, 0], SampleFrames);
   end;

  for Sample := 0 to SampleFrames - 1 do
   begin
    Temp := Mix[0, 0] * FDelayLine[0, 0].ProcessSample(Inputs[0, Sample]) + Mix[0, 1] * (Outputs[0, Sample] + Outputs[1, Sample]);
    Outputs[1, Sample] := Mix[1, 0] * FDelayLine[1, 0].ProcessSample(Inputs[1, Sample]) + Mix[1, 1] * (Outputs[0, Sample] - Outputs[1, Sample]);
    Outputs[0, Sample] := Temp;
   end;
 finally
  FCriticalSection.Leave;
 end;
end;

procedure TSplitHarmonizerModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 if SampleRate = 0 then exit;
 for Channel := 0 to NumInputs - 1 do
  begin
   FSoundTouch[Channel].SampleRate := abs(SampleRate);
   FDelayLine[Channel, 0].BufferSize := CInputDelay;
   FDelayLine[Channel, 1].BufferSize := Max(1, round(Parameter[2 + 3 * Channel] * 1E-3 * abs(SampleRate)));
  end;
end;

end.
