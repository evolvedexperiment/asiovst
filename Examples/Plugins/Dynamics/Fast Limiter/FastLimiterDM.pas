unit FastLimiterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TFastLimiterDataModule = class(TVSTModule)
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
    FFastLimiter : array [0..1] of TFastSoftKneeLimiter;
    function GetFastLimiter(Index: Integer): TFastSoftKneeLimiter;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property FastLimiter[Index: Integer]: TFastSoftKneeLimiter read GetFastLimiter;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, FastLimiterGUI, DAV_VSTModuleWithPrograms;

procedure TFastLimiterDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..8] of Single = (
    (50, 500, -10, 100, 4, 3, 0, 1, 0),
    (20, 100, -12, 100, 2.5, 6, 0, 0, 0),
    (20,  80, -15, 100, 2, 8, 0, 1, 0),
    (5, 60, -20, 100, 3, 13, 1, 0, 0),
    (1, 50, -30, 100, 2, 18, 0, 0, 0),
    (8, 64, -30, 100, 5, 17, 0, 0, 0),
    (16, 78, -24, 100, 1.8, 19, 0, 1, 0),
    (1, 20, -14, 100, 3, 8, 0, 1, 0),
    (3, 44, -17, 100, 1, 9, 1, 0, 0),
    (8, 56, -11, 100, 4, 5, 1, 1, 0));
begin
 for Channel := 0 to Length(FFastLimiter) - 1 do
  begin
   FFastLimiter[Channel] := TFastSoftKneeLimiter.Create;
   FFastLimiter[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 15;
 Parameter[1] := 75;
 Parameter[2] := -10;
 Parameter[3] := 100;
 Parameter[4] := 2;
 Parameter[5] := 6;
 Parameter[6] := 0;
 Parameter[7] := 0;
 Parameter[8] := 0;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TFastLimiterDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FFastLimiter[0]);
 FreeAndNil(FFastLimiter[1]);
end;

procedure TFastLimiterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmFastLimiter.Create(Self);
end;

function TFastLimiterDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastLimiter[0].CharacteristicCurve_dB(Input);
end;

procedure TFastLimiterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TFastLimiterDataModule.ParameterTimeLabel(
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

procedure TFastLimiterDataModule.ParameterTimeDisplay(
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

procedure TFastLimiterDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastLimiterDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastLimiter[0].MakeUpGain_dB := Value;
 FFastLimiter[1].MakeUpGain_dB := FFastLimiter[0].MakeUpGain_dB;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateMakeUp;
end;

procedure TFastLimiterDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastLimiterDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastLimiterDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastLimiterDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TFastLimiterDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateStereo;
end;

procedure TFastLimiterDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateLimit;
end;

procedure TFastLimiterDataModule.ChooseProcess;
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

procedure TFastLimiterDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastLimiter[0].AutoMakeUp := Boolean(round(Value));
 FFastLimiter[1].AutoMakeUp := FFastLimiter[0].AutoMakeUp;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateAutoMakeUpGain;
end;

function TFastLimiterDataModule.GetFastLimiter(Index: Integer): TFastSoftKneeLimiter;
begin
 if Index in [0..Length(FFastLimiter) - 1]
  then result := FFastLimiter[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TFastLimiterDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastLimiter[0].Attack := Value;
 FFastLimiter[1].Attack := FFastLimiter[0].Attack;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateAttack;
end;

procedure TFastLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastLimiter[0].Release := Value;
 FFastLimiter[1].Release := FFastLimiter[0].Release;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateRelease;
end;

procedure TFastLimiterDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastLimiter[0].Threshold_dB := Value;
 FFastLimiter[1].Threshold_dB := Value;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateThreshold;
end;

procedure TFastLimiterDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastLimiter[0].Knee_dB := Value;
 FFastLimiter[1].Knee_dB := Value;
 if EditorForm is TFmFastLimiter
  then TFmFastLimiter(EditorForm).UpdateKnee;
end;

procedure TFastLimiterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFastLimiter[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FFastLimiter[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TFastLimiterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastLimiter[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor * MakeUpGain;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TFastLimiterDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhOpt3Term(FFastLimiter[0].ProcessSample(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhOpt3Term(FFastLimiter[1].ProcessSample(Inputs[1, Sample]));
  end;
end;

procedure TFastLimiterDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastLimiter[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor * MakeUpGain;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TFastLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FFastLimiter[0].SampleRate := SampleRate;
 FFastLimiter[1].SampleRate := SampleRate;
end;

end.
