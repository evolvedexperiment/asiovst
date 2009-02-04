unit FastMultibandCompressorDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics, DAV_DspFilterLinkwitzRiley;

type
  TFastMultibandCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcessMono(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessMonoSoftClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
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
    procedure ParameterLimitChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAutoMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterLowFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMidFreqChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterHighChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FFastMultibandCompressor : array [0..3] of TFastCompressor;
    FLinkwitzRiley           : array [0..1, 0..2] of TLinkwitzRiley;
    function GetFastMultibandCompressor(Index: Integer): TFastCompressor;
    procedure ChooseProcess;
    function GetAutoGain(Index: Integer): Boolean;
    procedure SetAutoGain(Index: Integer; const Value: Boolean);
  public
    function EvaluateLowCharacteristic(const Input: Single): Single;
    function EvaluateLowMidCharacteristic(const Input: Single): Single;
    function EvaluateHighMidCharacteristic(const Input: Single): Single;
    function EvaluateHighCharacteristic(const Input: Single): Single;
    property FastMultibandCompressor[Index: Integer]: TFastCompressor read GetFastMultibandCompressor;
    property AutoGain[Index: Integer]: Boolean read GetAutoGain write SetAutoGain;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, FastMultibandCompressorGUI, DAV_VSTModuleWithPrograms;

procedure TFastMultibandCompressorDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FFastMultibandCompressor) - 1 do
  begin
   FFastMultibandCompressor[Channel] := TFastCompressor.Create;
   FFastMultibandCompressor[Channel].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 0].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0].Order := 1;
   FLinkwitzRiley[Channel, 1] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 1].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 1].Order := 1;
   FLinkwitzRiley[Channel, 2] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 2].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 2].Order := 1;
  end;

 Parameter[ 0] := 300;
 Parameter[ 1] := 800;
 Parameter[ 2] := 3000;
 Parameter[ 3] := 0;
 Parameter[ 4] := 5;
 Parameter[ 5] := 50;
 Parameter[ 6] := -10;
 Parameter[ 7] := 4;
 Parameter[ 8] := 3;
 Parameter[ 9] := 3;
 Parameter[10] := 0;
 Parameter[11] := 5;
 Parameter[12] := 50;
 Parameter[13] := -10;
 Parameter[14] := 4;
 Parameter[15] := 3;
 Parameter[16] := 3;
 Parameter[17] := 0;
 Parameter[18] := 5;
 Parameter[19] := 50;
 Parameter[20] := -10;
 Parameter[21] := 4;
 Parameter[22] := 3;
 Parameter[23] := 3;
 Parameter[24] := 0;
 Parameter[25] := 5;
 Parameter[26] := 50;
 Parameter[27] := -10;
 Parameter[28] := 4;
 Parameter[29] := 3;
 Parameter[30] := 3;
 Parameter[31] := 0;

 Programs[0].SetParameters(FParameter);
(*
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
*)
end;

procedure TFastMultibandCompressorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFastMultibandCompressor) - 1
  do FreeAndNil(FFastMultibandCompressor[Channel]);
end;

procedure TFastMultibandCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmFastMultibandCompressor.Create(Self);
end;

function TFastMultibandCompressorDataModule.EvaluateLowCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastMultibandCompressor[0].CharacteristicCurve_dB(Input);
end;

function TFastMultibandCompressorDataModule.EvaluateLowMidCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastMultibandCompressor[1].CharacteristicCurve_dB(Input);
end;

procedure TFastMultibandCompressorDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val >= 1000
  then PreDefined := 'kHz';
end;

procedure TFastMultibandCompressorDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

function TFastMultibandCompressorDataModule.EvaluateHighMidCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastMultibandCompressor[2].CharacteristicCurve_dB(Input);
end;

function TFastMultibandCompressorDataModule.EvaluateHighCharacteristic(
  const Input: Single): Single;
begin
 result:= FFastMultibandCompressor[3].CharacteristicCurve_dB(Input);
end;

procedure TFastMultibandCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 Value := 100;
end;

procedure TFastMultibandCompressorDataModule.ParameterTimeLabel(
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

procedure TFastMultibandCompressorDataModule.SetAutoGain(Index: Integer;
  const Value: Boolean);
begin
 if Index in [0..Length(FFastMultibandCompressor) - 1] then
  begin
   FFastMultibandCompressor[Index].AutoMakeUp := Value;
   if EditorForm is TFmFastMultibandCompressor then
    with TFmFastMultibandCompressor(EditorForm) do
     case Index of
      0: UpdateLowAutoMakeUpGain;
      1: UpdateLowMidAutoMakeUpGain;
      2: UpdateHighMidAutoMakeUpGain;
      3: UpdateHighAutoMakeUpGain;
     end;
  end else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TFastMultibandCompressorDataModule.ParameterTimeDisplay(
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

procedure TFastMultibandCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastMultibandCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastMultibandCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastMultibandCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TFastMultibandCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TFastMultibandCompressorDataModule.ParameterLowFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFastMultibandCompressor) - 1
  do FLinkwitzRiley[Channel, 0].Frequency := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do UpdateLowFrequency;
end;

procedure TFastMultibandCompressorDataModule.ParameterMidFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFastMultibandCompressor) - 1
  do FLinkwitzRiley[Channel, 1].Frequency := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do UpdateMidFrequency;
end;

procedure TFastMultibandCompressorDataModule.ParameterHighChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFastMultibandCompressor) - 1
  do FLinkwitzRiley[Channel, 2].Frequency := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do UpdateHighFrequency;
end;

procedure TFastMultibandCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmFastMultibandCompressor
  then TFmFastMultibandCompressor(EditorForm).UpdateLimit;
end;

procedure TFastMultibandCompressorDataModule.ChooseProcess;
begin
 case round(Parameter[3]) of
  0 : OnProcess := VSTModuleProcessMono;
  1 : OnProcess := VSTModuleProcessMonoSoftClip;
 end;
 OnProcessReplacing := OnProcess;
end;

function TFastMultibandCompressorDataModule.GetAutoGain(
  Index: Integer): Boolean;
begin
 if Index in [0..Length(FFastMultibandCompressor) - 1]
  then result := FFastMultibandCompressor[Index].AutoMakeUp
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TFastMultibandCompressorDataModule.GetFastMultibandCompressor(Index: Integer): TFastCompressor;
begin
 if Index in [0..Length(FFastMultibandCompressor) - 1]
  then result := FFastMultibandCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TFastMultibandCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 4) div 7;
 FFastMultibandCompressor[Band].Attack := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowAttack;
    1: UpdateLowMidAttack;
    2: UpdateHighMidAttack;
    3: UpdateHighAttack;
   end;
end;

procedure TFastMultibandCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 5) div 7;
 FFastMultibandCompressor[Band].Release := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRelease;
    1: UpdateLowMidRelease;
    2: UpdateHighMidRelease;
    3: UpdateHighRelease;
   end;
end;

procedure TFastMultibandCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 6) div 7;
 FFastMultibandCompressor[Band].Threshold_dB := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowThreshold;
    1: UpdateLowMidThreshold;
    2: UpdateHighMidThreshold;
    3: UpdateHighThreshold;
   end;
end;

procedure TFastMultibandCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 7) div 7;
 FFastMultibandCompressor[Band].Ratio := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRatio;
    1: UpdateLowMidRatio;
    2: UpdateHighMidRatio;
    3: UpdateHighRatio;
   end;
end;

procedure TFastMultibandCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 8) div 7;
 FFastMultibandCompressor[Band].Knee_dB := Value;
 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowKnee;
    1: UpdateLowMidKnee;
    2: UpdateHighMidKnee;
    3: UpdateHighKnee;
   end;
end;

procedure TFastMultibandCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 9) div 7;
 FFastMultibandCompressor[Band].MakeUpGain_dB := Value;

 if EditorForm is TFmFastMultibandCompressor then
  with TFmFastMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowMakeUp;
    1: UpdateLowMidMakeUp;
    2: UpdateHighMidMakeUp;
    3: UpdateHighMakeUp;
   end;
end;

procedure TFastMultibandCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 10) div 7;
end;

procedure TFastMultibandCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : array [0..3] of Single;
  FD     : array [0..1, 0..3] of Single;
const
  CDenorm32 : Single = 1E-12;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split mid
   FLinkwitzRiley[0, 1].ProcessSample(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 3]);
   FLinkwitzRiley[1, 1].ProcessSample(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 3]);

   // split low
   FLinkwitzRiley[0, 0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1, 0].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // split high
   FLinkwitzRiley[0, 2].ProcessSample(FD[0, 3] - CDenorm32, FD[0, 2], FD[0, 3]);
   FLinkwitzRiley[1, 2].ProcessSample(FD[1, 3] - CDenorm32, FD[1, 2], FD[1, 3]);

   // compress & copy gain reduction
   with FFastMultibandCompressor[0] do
    begin
     InputSample(CHalf32 * (FD[0, 0] + FD[1, 0]));
     Temp[0] := GainReductionFactor * MakeUpGain;
    end;
   with FFastMultibandCompressor[1] do
    begin
     InputSample(CHalf32 * (FD[0, 1] + FD[1, 1]));
     Temp[1] := GainReductionFactor * MakeUpGain;
    end;
   with FFastMultibandCompressor[2] do
    begin
     InputSample(CHalf32 * (FD[0, 2] + FD[1, 2]));
     Temp[2] := GainReductionFactor * MakeUpGain;
    end;
   with FFastMultibandCompressor[3] do
    begin
     InputSample(CHalf32 * (FD[0, 3] + FD[1, 3]));
     Temp[3] := GainReductionFactor * MakeUpGain;
    end;


   // gain and combine
   Outputs[0, Sample] := Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2] - Temp[3] * FD[0, 3];
   Outputs[1, Sample] := Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2] - Temp[3] * FD[1, 3];
  end;
end;

procedure TFastMultibandCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : array [0..3] of Single;
  FD     : array [0..1, 0..3] of Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split mid
   FLinkwitzRiley[0, 1].ProcessSample(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 3]);
   FLinkwitzRiley[1, 1].ProcessSample(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 3]);

   // split low
   FLinkwitzRiley[0, 0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1, 0].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // split low
   FLinkwitzRiley[0, 2].ProcessSample(FD[0, 3] - CDenorm32, FD[0, 2], FD[0, 3]);
   FLinkwitzRiley[1, 2].ProcessSample(FD[1, 3] - CDenorm32, FD[1, 2], FD[1, 3]);

   // compress
   FFastMultibandCompressor[0].ProcessSample(CHalf32 * (FD[0, 0] + FD[1, 0]));
   FFastMultibandCompressor[1].ProcessSample(CHalf32 * (FD[0, 1] + FD[1, 1]));
   FFastMultibandCompressor[2].ProcessSample(CHalf32 * (FD[0, 2] + FD[1, 2]));
   FFastMultibandCompressor[3].ProcessSample(CHalf32 * (FD[0, 3] + FD[1, 3]));

   // copy gain reduction
   Temp[0] := FFastMultibandCompressor[0].GainReductionFactor;
   Temp[1] := FFastMultibandCompressor[1].GainReductionFactor;
   Temp[2] := FFastMultibandCompressor[2].GainReductionFactor;
   Temp[3] := FFastMultibandCompressor[3].GainReductionFactor;

   // gain and combine
   Outputs[0, Sample] := FastTanhOpt3Term(Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2] - Temp[3] * FD[0, 3]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2] - Temp[3] * FD[1, 3]);
  end;
end;

procedure TFastMultibandCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FFastMultibandCompressor) - 1 do
  begin
   FFastMultibandCompressor[Channel].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 1].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 2].SampleRate := SampleRate;
  end;
end;

end.
