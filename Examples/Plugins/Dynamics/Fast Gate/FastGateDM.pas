unit FastGateDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TFastGateDataModule = class(TVSTModule)
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
    FFastGate : array [0..1] of TFastCompressor;
    function GetFastGate(Index: Integer): TFastCompressor;
  public
    function EvaluateCharacteristic(const Input: Single): Single;
    property FastGate[Index: Integer]: TFastCompressor read GetFastGate;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, FastGateGUI, DAV_VSTModuleWithPrograms;

procedure TFastGateDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FFastGate) - 1 do
  begin
   FFastGate[Channel] := TFastCompressor.Create;
   FFastGate[Channel].SampleRate := SampleRate;
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

procedure TFastGateDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FFastGate[0]);
 FreeAndNil(FFastGate[1]);
end;

procedure TFastGateDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmFastGate.Create(Self);
end;

function TFastGateDataModule.EvaluateCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastGate[0].CharacteristicCurve_dB(Input);
end;

procedure TFastGateDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TFastGateDataModule.ParameterTimeLabel(
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

procedure TFastGateDataModule.ParameterTimeDisplay(
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

procedure TFastGateDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastGateDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastGateDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

function TFastGateDataModule.GetFastGate(Index: Integer): TFastCompressor;
begin
 if Index in [0..Length(FFastGate) - 1]
  then result := FFastGate[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TFastGateDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastGate[0].Release := Value;
 FFastGate[1].Release := FFastGate[0].Release;
 if EditorForm is TFmFastGate
  then TFmFastGate(EditorForm).UpdateAttack;
end;

procedure TFastGateDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastGate[0].Attack := Value;
 FFastGate[1].Attack := FFastGate[0].Attack;
 if EditorForm is TFmFastGate
  then TFmFastGate(EditorForm).UpdateRelease;
end;

procedure TFastGateDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastGate[0].Threshold_dB := Value;
 FFastGate[1].Threshold_dB := Value;
 if EditorForm is TFmFastGate
  then TFmFastGate(EditorForm).UpdateThreshold;
end;

procedure TFastGateDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastGate[0].Ratio := Value;
 FFastGate[1].Ratio := Value;
 if EditorForm is TFmFastGate
  then TFmFastGate(EditorForm).UpdateRatio;
end;

procedure TFastGateDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFastGate[0].Knee_dB := Value;
 FFastGate[1].Knee_dB := Value;
 if EditorForm is TFmFastGate
  then TFmFastGate(EditorForm).UpdateKnee;
end;

procedure TFastGateDataModule.VSTModuleProcessStereo(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   Outputs[0, Sample] := FFastGate[0].ProcessSample(Inputs[0, Sample]);
   Outputs[1, Sample] := FFastGate[1].ProcessSample(Inputs[1, Sample]);
  end;
end;

procedure TFastGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FFastGate[0] do
  begin
   ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample]));
   Temp := GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TFastGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FFastGate[0].SampleRate := SampleRate;
 FFastGate[1].SampleRate := SampleRate;
end;

end.
