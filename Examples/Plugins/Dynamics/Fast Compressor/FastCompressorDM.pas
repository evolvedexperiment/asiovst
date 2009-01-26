unit FastCompressorDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TFastCompressorDataModule = class(TVSTModule)
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
    procedure ParameterMixChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    FFastCompressor : array [0..1] of TFastCompressor;
    FMix            : array [0..1] of Single;
    function GetFastCompressor(Index: Integer): TFastCompressor;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property FastCompressor[Index: Integer]: TFastCompressor read GetFastCompressor;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, FastCompressorGUI, DAV_VSTModuleWithPrograms;

procedure TFastCompressorDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..9, 0..9] of Single = (
    (50, 500, -10, 3, 4, 3, 0, 1, 0, 100),
    (20, 100, -12, 4, 2.5, 6, 0, 0, 0, 100),
    (20,  80, -15, 8, 2, 8, 0, 1, 0, 100),
    (5, 60, -20, 7, 3, 13, 1, 0, 0, 100),
    (8, 64, -30, 12, 5, 17, 0, 0, 0, 100),
    (16, 78, -24, 15, 1.8, 19, 0, 1, 0, 100),
    (1, 20, -14, 5, 3, 8, 0, 1, 0, 100),
    (3, 44, -17, 7, 1, 9, 1, 0, 0, 100),
    (8, 56, -11, 9, 4, 5, 1, 1, 0, 100));
begin
 for Channel := 0 to Length(FFastCompressor) - 1 do
  begin
   FFastCompressor[Channel] := TFastCompressor.Create;
   FFastCompressor[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 1;
 Parameter[1] := 50;
 Parameter[2] := -30;
 Parameter[3] := 6;
 Parameter[4] := 2;
 Parameter[5] := 18;
 Parameter[6] := 0;
 Parameter[7] := 0;
 Parameter[8] := 0;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TFastCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FFastCompressor[0]);
 FreeAndNil(FFastCompressor[1]);
end;

procedure TFastCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmFastCompressor.Create(Self);
end;

function TFastCompressorDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastCompressor[0].CharacteristicCurve_dB(Input);
end;

procedure TFastCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TFastCompressorDataModule.ParameterTimeLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1
  then PreDefined := 'µs' else
 if Val >= 1000
  then PreDefined := 's';
end;

procedure TFastCompressorDataModule.ParameterTimeDisplay(
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

procedure TFastCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].MakeUpGain_dB := Value;
 FFastCompressor[1].MakeUpGain_dB := FFastCompressor[0].MakeUpGain_dB;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateMakeUp;
end;

procedure TFastCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TFastCompressorDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateStereo;
end;

procedure TFastCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateLimit;
end;

procedure TFastCompressorDataModule.ChooseProcess;
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

procedure TFastCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].AutoMakeUp := Boolean(round(Value));
 FFastCompressor[1].AutoMakeUp := FFastCompressor[0].AutoMakeUp;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateAutoMakeUpGain;
end;

function TFastCompressorDataModule.GetFastCompressor(Index: Integer): TFastCompressor;
begin
 if Index in [0..Length(FFastCompressor) - 1]
  then result := FFastCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TFastCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].Attack := Value;
 FFastCompressor[1].Attack := FFastCompressor[0].Attack;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateAttack;
end;

procedure TFastCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].Release := Value;
 FFastCompressor[1].Release := FFastCompressor[0].Release;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateRelease;
end;

procedure TFastCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].Threshold_dB := Value;
 FFastCompressor[1].Threshold_dB := Value;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateThreshold;
end;

procedure TFastCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].Ratio := Value;
 FFastCompressor[1].Ratio := Value;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateRatio;
end;

procedure TFastCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastCompressor[0].Knee_dB := Value;
 FFastCompressor[1].Knee_dB := Value;
 if EditorForm is TFmFastCompressor
  then TFmFastCompressor(EditorForm).UpdateKnee;
end;

procedure TFastCompressorDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFastCompressor[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FFastCompressor[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TFastCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastCompressor[0] do
  begin
   ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TFastCompressorDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhOpt3Term(FFastCompressor[0].ProcessSample(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhOpt3Term(FFastCompressor[1].ProcessSample(Inputs[1, Sample]));
  end;
end;

procedure TFastCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastCompressor[0] do
  begin
   ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TFastCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FFastCompressor[0].SampleRate := SampleRate;
 FFastCompressor[1].SampleRate := SampleRate;
end;

end.