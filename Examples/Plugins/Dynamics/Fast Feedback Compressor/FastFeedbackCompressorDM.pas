unit FastFeedbackCompressorDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TFastFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMonoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterStereoChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFastFeedbackCompressor : array [0..1] of TFastFeedbackCompressor;
    function GetFastFeedbackCompressor(Index: Integer): TFastFeedbackCompressor;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property FastFeedbackCompressor[Index: Integer]: TFastFeedbackCompressor read GetFastFeedbackCompressor;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, FastFeedbackCompressorGUI, DAV_VSTModuleWithPrograms;

procedure TFastFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..9] of Single = (
    (50, 500, -10, 3, 4, 3, 0, 1, 0, 100),
    (20, 100, -12, 4, 2.5, 6, 0, 0, 0, 100),
    (20,  80, -15, 8, 2, 8, 0, 1, 0, 100),
    (5, 60, -20, 7, 3, 13, 1, 0, 0, 100),
    (1, 50, -30, 6, 2, 18, 0, 0, 0, 100),
    (8, 64, -30, 12, 5, 17, 0, 0, 0, 100),
    (16, 78, -24, 15, 1.8, 19, 0, 1, 0, 100),
    (1, 20, -14, 5, 3, 8, 0, 1, 0, 100),
    (3, 44, -17, 7, 1, 9, 1, 0, 0, 100),
    (8, 56, -11, 9, 4, 5, 1, 1, 0, 100));
begin
 for Channel := 0 to Length(FFastFeedbackCompressor) - 1 do
  begin
   FFastFeedbackCompressor[Channel] := TFastFeedbackCompressor.Create;
   FFastFeedbackCompressor[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 15;
 Parameter[1] := 75;
 Parameter[2] := -10;
 Parameter[3] := 5;
 Parameter[4] := 2;
 Parameter[5] := 6;
 Parameter[6] := 0;
 Parameter[7] := 0;
 Parameter[8] := 0;
 Parameter[9] := 100;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FFastFeedbackCompressor[0]);
 FreeAndNil(FFastFeedbackCompressor[1]);
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmFastFeedbackCompressor.Create(Self);
end;

function TFastFeedbackCompressorDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastFeedbackCompressor[0].CharacteristicCurve_dB(Input);
end;

procedure TFastFeedbackCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TFastFeedbackCompressorDataModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := '�s' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TFastFeedbackCompressorDataModule.ParameterTimeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := FloatToStrF(RoundTo(1E3 * Val, -2), ffGeneral, 3, 3) else
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TFastFeedbackCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastFeedbackCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].MakeUpGain_dB := Value;
 FFastFeedbackCompressor[1].MakeUpGain_dB := FFastFeedbackCompressor[0].MakeUpGain_dB;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateMakeUp;
end;

procedure TFastFeedbackCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastFeedbackCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastFeedbackCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastFeedbackCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TFastFeedbackCompressorDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateStereo;
end;

procedure TFastFeedbackCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateLimit;
end;

procedure TFastFeedbackCompressorDataModule.ChooseProcess;
begin
 case round(Parameter[7]) of
  0 : case round(Parameter[6]) of
       0 : OnProcess := VSTModuleProcessMono;
       1 : OnProcess := VSTModuleProcessStereo;
      end;
  1 : case round(Parameter[6]) of
       0 : OnProcess := VSTModuleProcessMonoSoftClip;
       1 : OnProcess := VSTModuleProcessStereoSoftClip;
      end;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TFastFeedbackCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].AutoMakeUp := Boolean(round(Value));
 FFastFeedbackCompressor[1].AutoMakeUp := FFastFeedbackCompressor[0].AutoMakeUp;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateAutoMakeUpGain;
end;

function TFastFeedbackCompressorDataModule.GetFastFeedbackCompressor(Index: Integer): TFastFeedbackCompressor;
begin
 if Index in [0..Length(FFastFeedbackCompressor) - 1]
  then result := FFastFeedbackCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TFastFeedbackCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].Attack := Value;
 FFastFeedbackCompressor[1].Attack := FFastFeedbackCompressor[0].Attack;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateAttack;
end;

procedure TFastFeedbackCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].Release := Value;
 FFastFeedbackCompressor[1].Release := FFastFeedbackCompressor[0].Release;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateRelease;
end;

procedure TFastFeedbackCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].Threshold_dB := Value;
 FFastFeedbackCompressor[1].Threshold_dB := Value;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateThreshold;
end;

procedure TFastFeedbackCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].Ratio := Value;
 FFastFeedbackCompressor[1].Ratio := Value;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateRatio;
end;

procedure TFastFeedbackCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastFeedbackCompressor[0].Knee_dB := Value;
 FFastFeedbackCompressor[1].Knee_dB := Value;
 if EditorForm is TFmFastFeedbackCompressor
  then TFmFastFeedbackCompressor(EditorForm).UpdateKnee;
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFastFeedbackCompressor[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FFastFeedbackCompressor[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastFeedbackCompressor[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := MakeUpGain * GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhOpt3Term(FFastFeedbackCompressor[0].ProcessSample(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhOpt3Term(FFastFeedbackCompressor[1].ProcessSample(Inputs[1, Sample]));
  end;
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastFeedbackCompressor[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := MakeUpGain * GainReductionFactor;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TFastFeedbackCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FFastFeedbackCompressor[0].SampleRate := SampleRate;
 FFastFeedbackCompressor[1].SampleRate := SampleRate;
 if EditorForm is TFmFastFeedbackCompressor then
  with TFmFastFeedbackCompressor(EditorForm) do
   begin
    DialAttack.Min  := max(0.01, 2000 / SampleRate);
    DialRelease.Min := max(0.1, 2000 / SampleRate);
   end;
end;

end.
