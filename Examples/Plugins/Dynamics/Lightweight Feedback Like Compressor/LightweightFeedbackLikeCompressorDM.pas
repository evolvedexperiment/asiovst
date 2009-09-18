unit LightweightFeedbackLikeCompressorDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics, DAV_DspLightweightDynamics;

type
  TLightweightFeedbackLikeCompressorDataModule = class(TVSTModule)
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
    FLightweightFeedbackLikeCompressor : array [0..1] of TCustomCompressor;
    function GetLightweightFeedbackLikeCompressor(Index: Integer): TCustomCompressor;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property LightweightFeedbackLikeCompressor[Index: Integer]: TCustomCompressor read GetLightweightFeedbackLikeCompressor;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, LightweightFeedbackLikeCompressorGUI, DAV_VSTModuleWithPrograms;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FLightweightFeedbackLikeCompressor) - 1 do
  begin
   FLightweightFeedbackLikeCompressor[Channel] := TLightweightSoftKneeFeedbackLikeCompressor.Create;
   FLightweightFeedbackLikeCompressor[Channel].SampleRate := SampleRate;
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

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLightweightFeedbackLikeCompressor[0]);
 FreeAndNil(FLightweightFeedbackLikeCompressor[1]);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmLightweightFeedbackLikeCompressor.Create(Self);
end;

function TLightweightFeedbackLikeCompressorDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FLightweightFeedbackLikeCompressor[0].CharacteristicCurve_dB(Input);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterTimeLabel(
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

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterTimeDisplay(
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

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].MakeUpGain_dB := Value;
   FLightweightFeedbackLikeCompressor[1].MakeUpGain_dB := FLightweightFeedbackLikeCompressor[0].MakeUpGain_dB;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateMakeUp;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateStereo;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateLimit;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ChooseProcess;
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

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].AutoMakeUp := Boolean(round(Value));
   if assigned(FLightweightFeedbackLikeCompressor[1])
    then FLightweightFeedbackLikeCompressor[1].AutoMakeUp := FLightweightFeedbackLikeCompressor[0].AutoMakeUp;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateAutoMakeUpGain;
end;

function TLightweightFeedbackLikeCompressorDataModule.GetLightweightFeedbackLikeCompressor(Index: Integer): TCustomCompressor;
begin
 if Index in [0..Length(FLightweightFeedbackLikeCompressor) - 1]
  then result := FLightweightFeedbackLikeCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].Attack := Value;
   if assigned(FLightweightFeedbackLikeCompressor[1])
    then FLightweightFeedbackLikeCompressor[1].Attack := FLightweightFeedbackLikeCompressor[0].Attack;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateAttack;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].Release := Value;
   if assigned(FLightweightFeedbackLikeCompressor[1])
    then FLightweightFeedbackLikeCompressor[1].Release := FLightweightFeedbackLikeCompressor[0].Release;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateRelease;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].Threshold_dB := Value;
   if assigned(FLightweightFeedbackLikeCompressor[1])
    then FLightweightFeedbackLikeCompressor[1].Threshold_dB := Value;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateThreshold;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].Ratio := Value;
   if assigned(FLightweightFeedbackLikeCompressor[1])
    then FLightweightFeedbackLikeCompressor[1].Ratio := Value;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateRatio;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0]) then
  begin
   FLightweightFeedbackLikeCompressor[0].Knee_dB := Value;
   if assigned(FLightweightFeedbackLikeCompressor[1])
    then FLightweightFeedbackLikeCompressor[1].Knee_dB := Value;
  end;
 if EditorForm is TFmLightweightFeedbackLikeCompressor
  then TFmLightweightFeedbackLikeCompressor(EditorForm).UpdateKnee;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FLightweightFeedbackLikeCompressor[0].ProcessSample64(Inputs[0, Sample]);
   Outputs[1, Sample] := FLightweightFeedbackLikeCompressor[1].ProcessSample64(Inputs[1, Sample]);
  end;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightFeedbackLikeCompressor[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := MakeUpGain * GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhContinousError4(FLightweightFeedbackLikeCompressor[0].ProcessSample64(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhContinousError4(FLightweightFeedbackLikeCompressor[1].ProcessSample64(Inputs[1, Sample]));
  end;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightFeedbackLikeCompressor[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := MakeUpGain * GainReductionFactor;
   Outputs[0, Sample] := FastTanhContinousError4(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhContinousError4(Temp * Inputs[1, Sample]);
  end;
end;

procedure TLightweightFeedbackLikeCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if assigned(FLightweightFeedbackLikeCompressor[0])
  then FLightweightFeedbackLikeCompressor[0].SampleRate := SampleRate;
 if assigned(FLightweightFeedbackLikeCompressor[1])
  then FLightweightFeedbackLikeCompressor[1].SampleRate := SampleRate;
 if EditorForm is TFmLightweightFeedbackLikeCompressor then
  with TFmLightweightFeedbackLikeCompressor(EditorForm) do
   begin
    DialAttack.Min  := max(0.01, 2000 / SampleRate);
    DialRelease.Min := max(0.1, 2000 / SampleRate);
   end;
end;

end.
