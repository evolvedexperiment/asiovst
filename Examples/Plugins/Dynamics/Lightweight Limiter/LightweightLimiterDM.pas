unit LightweightLimiterDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TLightweightLimiterDataModule = class(TVSTModule)
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
    FLightweightLimiter : array [0..1] of TLightweightSoftKneeLimiter;
    function GetLightweightLimiter(Index: Integer): TLightweightSoftKneeLimiter;
    procedure ChooseProcess;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property LightweightLimiter[Index: Integer]: TLightweightSoftKneeLimiter read GetLightweightLimiter;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, LightweightLimiterGUI, DAV_VSTModuleWithPrograms;

procedure TLightweightLimiterDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FLightweightLimiter) - 1 do
  begin
   FLightweightLimiter[Channel] := TLightweightSoftKneeLimiter.Create;
   FLightweightLimiter[Channel].SampleRate := SampleRate;
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

procedure TLightweightLimiterDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLightweightLimiter[0]);
 FreeAndNil(FLightweightLimiter[1]);
end;

procedure TLightweightLimiterDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmLightweightLimiter.Create(Self);
end;

function TLightweightLimiterDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FLightweightLimiter[0].CharacteristicCurve_dB(Input);
end;

procedure TLightweightLimiterDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightLimiterDataModule.ParameterTimeLabel(
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

procedure TLightweightLimiterDataModule.ParameterTimeDisplay(
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

procedure TLightweightLimiterDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightLimiter[0].MakeUpGain_dB := Value;
 FLightweightLimiter[1].MakeUpGain_dB := FLightweightLimiter[0].MakeUpGain_dB;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateMakeUp;
end;

procedure TLightweightLimiterDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightLimiterDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TLightweightLimiterDataModule.ParameterStereoChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateStereo;
end;

procedure TLightweightLimiterDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateLimit;
end;

procedure TLightweightLimiterDataModule.ChooseProcess;
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

procedure TLightweightLimiterDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightLimiter[0].AutoMakeUp := Boolean(round(Value));
 FLightweightLimiter[1].AutoMakeUp := FLightweightLimiter[0].AutoMakeUp;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateAutoMakeUpGain;
end;

function TLightweightLimiterDataModule.GetLightweightLimiter(Index: Integer): TLightweightSoftKneeLimiter;
begin
 if Index in [0..Length(FLightweightLimiter) - 1]
  then result := FLightweightLimiter[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightLimiterDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightLimiter[0].Attack := Value;
 FLightweightLimiter[1].Attack := FLightweightLimiter[0].Attack;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateAttack;
end;

procedure TLightweightLimiterDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightLimiter[0].Release := Value;
 FLightweightLimiter[1].Release := FLightweightLimiter[0].Release;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateRelease;
end;

procedure TLightweightLimiterDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightLimiter[0].Threshold_dB := Value;
 FLightweightLimiter[1].Threshold_dB := Value;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateThreshold;
end;

procedure TLightweightLimiterDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightLimiter[0].Knee_dB := Value;
 FLightweightLimiter[1].Knee_dB := Value;
 if EditorForm is TFmLightweightLimiter
  then TFmLightweightLimiter(EditorForm).UpdateKnee;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FLightweightLimiter[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FLightweightLimiter[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightLimiter[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor * MakeUpGain;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessStereoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FastTanhOpt3Term(FLightweightLimiter[0].ProcessSample(Inputs[0, Sample]));
   Outputs[1, Sample] := FastTanhOpt3Term(FLightweightLimiter[1].ProcessSample(Inputs[1, Sample]));
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightLimiter[0] do
  begin
   InputSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor * MakeUpGain;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TLightweightLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FLightweightLimiter[0].SampleRate := SampleRate;
 FLightweightLimiter[1].SampleRate := SampleRate;
end;

end.
