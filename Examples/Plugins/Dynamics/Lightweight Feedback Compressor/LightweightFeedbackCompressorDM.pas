unit LightweightFeedbackCompressorDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TLightweightFeedbackCompressorDataModule = class(TVSTModule)
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
    FLightweightFeedbackCompressor : array [0..1] of TLightweightSoftKneeFeedbackCompressor;
    function GetLightweightFeedbackCompressor(Index: Integer): TLightweightSoftKneeFeedbackCompressor;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property LightweightFeedbackCompressor[Index: Integer]: TLightweightSoftKneeFeedbackCompressor read GetLightweightFeedbackCompressor;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, LightweightFeedbackCompressorGUI, DAV_VSTModuleWithPrograms;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FLightweightFeedbackCompressor) - 1 do
  begin
   FLightweightFeedbackCompressor[Channel] := TLightweightSoftKneeFeedbackCompressor.Create;
   FLightweightFeedbackCompressor[Channel].SampleRate := SampleRate;
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

procedure TLightweightFeedbackCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLightweightFeedbackCompressor[0]);
 FreeAndNil(FLightweightFeedbackCompressor[1]);
end;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmLightweightFeedbackCompressor.Create(Self);
end;

function TLightweightFeedbackCompressorDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FLightweightFeedbackCompressor[0].CharacteristicCurve_dB(Input);
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterTimeLabel(
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

procedure TLightweightFeedbackCompressorDataModule.ParameterTimeDisplay(
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

procedure TLightweightFeedbackCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].MakeUpGain_dB := Value;
 FLightweightFeedbackCompressor[1].MakeUpGain_dB := FLightweightFeedbackCompressor[0].MakeUpGain_dB;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateMakeUp;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateStereo;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateLimit;
end;

procedure TLightweightFeedbackCompressorDataModule.ChooseProcess;
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

procedure TLightweightFeedbackCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].AutoMakeUp := Boolean(round(Value));
 FLightweightFeedbackCompressor[1].AutoMakeUp := FLightweightFeedbackCompressor[0].AutoMakeUp;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateAutoMakeUpGain;
end;

function TLightweightFeedbackCompressorDataModule.GetLightweightFeedbackCompressor(Index: Integer): TLightweightSoftKneeFeedbackCompressor;
begin
 if Index in [0..Length(FLightweightFeedbackCompressor) - 1]
  then result := FLightweightFeedbackCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].Attack := Value;
 FLightweightFeedbackCompressor[1].Attack := FLightweightFeedbackCompressor[0].Attack;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateAttack;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].Release := Value;
 FLightweightFeedbackCompressor[1].Release := FLightweightFeedbackCompressor[0].Release;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateRelease;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].Threshold_dB := Value;
 FLightweightFeedbackCompressor[1].Threshold_dB := Value;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateThreshold;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].Ratio := Value;
 FLightweightFeedbackCompressor[1].Ratio := Value;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateRatio;
end;

procedure TLightweightFeedbackCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightFeedbackCompressor[0].Knee_dB := Value;
 FLightweightFeedbackCompressor[1].Knee_dB := Value;
 if EditorForm is TFmLightweightFeedbackCompressor
  then TFmLightweightFeedbackCompressor(EditorForm).UpdateKnee;
end;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FLightweightFeedbackCompressor[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FLightweightFeedbackCompressor[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightFeedbackCompressor[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := MakeUpGain * GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhOpt3Term(FLightweightFeedbackCompressor[0].ProcessSample(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhOpt3Term(FLightweightFeedbackCompressor[1].ProcessSample(Inputs[1, Sample]));
  end;
end;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightFeedbackCompressor[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := MakeUpGain * GainReductionFactor;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TLightweightFeedbackCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FLightweightFeedbackCompressor[0].SampleRate := SampleRate;
 FLightweightFeedbackCompressor[1].SampleRate := SampleRate;
 if EditorForm is TFmLightweightFeedbackCompressor then
  with TFmLightweightFeedbackCompressor(EditorForm) do
   begin
    DialAttack.Min  := max(0.01, 2000 / SampleRate);
    DialRelease.Min := max(0.1, 2000 / SampleRate);
   end;
end;

end.