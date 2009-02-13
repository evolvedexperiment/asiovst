unit LightweightGateDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TLightweightGateDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessStereo(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    FLightweightGate : array [0..1] of TLightweightSoftKneeCompressor;
    function GetLightweightGate(Index: Integer): TLightweightSoftKneeCompressor;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property LightweightGate[Index: Integer]: TLightweightSoftKneeCompressor read GetLightweightGate;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, LightweightGateGUI, DAV_VSTModuleWithPrograms;

procedure TLightweightGateDataModule.VSTModuleOpen(Sender: TObject);
var
  Channel : Integer;
const
  CPresets : array [1..10, 0..4] of Single = (
    (5, 50, -50, 0.3, 3),
    (2, 10, -40, 0.2, 6),
    (2,  8, -30, 0.1, 8),
    (1.6, 7.8, -34, 0.04, 19),
    (0.5, 6, -20, 0.15, 13),
    (0.1, 5, -15, 0.2, 18),
    (0.8, 6.4, -10, 0.06, 17),
    (0.1, 2, -44, 0.2, 8),
    (0.3, 4.4, -37, 0.1, 9),
    (0.8, 5.6, -41, 0.1, 5));
begin
 for Channel := 0 to Length(FLightweightGate) - 1 do
  begin
   FLightweightGate[Channel] := TLightweightSoftKneeCompressor.Create;
   FLightweightGate[Channel].SampleRate := SampleRate;
  end;

 Parameter[0] := 1.5;
 Parameter[1] := 7.5;
 Parameter[2] := -10;
 Parameter[3] := 0.2;
 Parameter[4] := 6;

 Programs[0].SetParameters(FParameter);
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
end;

procedure TLightweightGateDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FLightweightGate[0]);
 FreeAndNil(FLightweightGate[1]);
end;

procedure TLightweightGateDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmLightweightGate.Create(Self);
end;

function TLightweightGateDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FLightweightGate[0].CharacteristicCurve_dB(Input);
end;

procedure TLightweightGateDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TLightweightGateDataModule.ParameterTimeLabel(
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

procedure TLightweightGateDataModule.ParameterTimeDisplay(
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

procedure TLightweightGateDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightGateDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TLightweightGateDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

function TLightweightGateDataModule.GetLightweightGate(Index: Integer): TLightweightSoftKneeCompressor;
begin
 if Index in [0..Length(FLightweightGate) - 1]
  then result := FLightweightGate[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TLightweightGateDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightGate[0].Attack := Value;
 FLightweightGate[1].Attack := FLightweightGate[0].Attack;
 if EditorForm is TFmLightweightGate
  then TFmLightweightGate(EditorForm).UpdateAttack;
end;

procedure TLightweightGateDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightGate[0].Release := Value;
 FLightweightGate[1].Release := FLightweightGate[0].Release;
 if EditorForm is TFmLightweightGate
  then TFmLightweightGate(EditorForm).UpdateRelease;
end;

procedure TLightweightGateDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightGate[0].Threshold_dB := Value;
 FLightweightGate[1].Threshold_dB := Value;
 if EditorForm is TFmLightweightGate
  then TFmLightweightGate(EditorForm).UpdateThreshold;
end;

procedure TLightweightGateDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightGate[0].Ratio := Value;
 FLightweightGate[1].Ratio := Value;
 if EditorForm is TFmLightweightGate
  then TFmLightweightGate(EditorForm).UpdateRatio;
end;

procedure TLightweightGateDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FLightweightGate[0].Knee_dB := Value;
 FLightweightGate[1].Knee_dB := Value;
 if EditorForm is TFmLightweightGate
  then TFmLightweightGate(EditorForm).UpdateKnee;
end;

procedure TLightweightGateDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FLightweightGate[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FLightweightGate[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TLightweightGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FLightweightGate[0] do
  begin
   ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TLightweightGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FLightweightGate[0].SampleRate := SampleRate;
 FLightweightGate[1].SampleRate := SampleRate;
end;

end.