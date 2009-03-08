﻿unit SmoothMultibandCompressorDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics, DAV_DspFilterLinkwitzRiley;

type
  TSmoothMultibandCompressorDataModule = class(TVSTModule)
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
    procedure ParameterHighChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterFrequencyDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterFrequencyLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FSmoothMultibandCompressor : array [0..2] of TLightweightSoftKneeCompressor;
    FLinkwitzRiley             : array [0..1, 0..1] of TLinkwitzRiley;
    FMidiLearnParameter        : Integer;
    function GetSmoothMultibandCompressor(Index: Integer): TLightweightSoftKneeCompressor;
    procedure ChooseProcess;
    function GetAutoGain(Index: Integer): Boolean;
    procedure SetAutoGain(Index: Integer; const Value: Boolean);
  public
    property SmoothMultibandCompressor[Index: Integer]: TLightweightSoftKneeCompressor read GetSmoothMultibandCompressor;
    property AutoGain[Index: Integer]: Boolean read GetAutoGain write SetAutoGain;
    property MidiLearnParameter: Integer read FMidiLearnParameter write FMidiLearnParameter; 
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, SmoothMultibandCompressorGUI, DAV_VSTModuleWithPrograms;

procedure TSmoothMultibandCompressorDataModule.VSTModuleOpen(Sender: TObject);
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
 for Channel := 0 to Length(FSmoothMultibandCompressor) - 1 do
  begin
   FSmoothMultibandCompressor[Channel] := TLightweightSoftKneeCompressor.Create;
   FSmoothMultibandCompressor[Channel].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 0].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0].Order := 1;
   FLinkwitzRiley[Channel, 1] := TLinkwitzRiley.Create;
   FLinkwitzRiley[Channel, 1].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 1].Order := 1;
  end;

 Parameter[ 0] := 300;
 Parameter[ 1] := 800;
 Parameter[ 2] := 0;
 Parameter[ 3] := 5;
 Parameter[ 4] := 50;
 Parameter[ 5] := -10;
 Parameter[ 6] := 4;
 Parameter[ 7] := 3;
 Parameter[ 8] := 3;
 Parameter[ 9] := 5;
 Parameter[10] := 50;
 Parameter[11] := -10;
 Parameter[12] := 4;
 Parameter[13] := 3;
 Parameter[14] := 3;
 Parameter[15] := 5;
 Parameter[16] := 50;
 Parameter[17] := -10;
 Parameter[18] := 4;
 Parameter[19] := 3;
 Parameter[20] := 3;
 Parameter[21] := 0;

 Programs[0].SetParameters(FParameter);
(*
 for Channel := 1 to numPrograms - 1
  do Programs[Channel].SetParameters(CPresets[Channel]);
*)
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleClose(Sender: TObject);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSmoothMultibandCompressor) - 1
  do FreeAndNil(FSmoothMultibandCompressor[Channel]);
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmSmoothMultibandCompressor.Create(Self);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterFrequencyLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val >= 1000
  then PreDefined := 'kHz';
end;

procedure TSmoothMultibandCompressorDataModule.ParameterFrequencyDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
var
  Val : Single;
begin
 Val := Parameter[Index];
 if Val < 1000
  then PreDefined := FloatToStrF(RoundTo(Val, -2), ffGeneral, 3, 3)
  else PreDefined := FloatToStrF(RoundTo(1E-3 * Val, -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do UpdateOutputGain;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterTimeLabel(
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

procedure TSmoothMultibandCompressorDataModule.SetAutoGain(Index: Integer;
  const Value: Boolean);
begin
 if Index in [0..Length(FSmoothMultibandCompressor) - 1] then
  begin
   FSmoothMultibandCompressor[Index].AutoMakeUp := Value;
   if EditorForm is TFmSmoothMultibandCompressor then
    with TFmSmoothMultibandCompressor(EditorForm) do
     case Index of
      0: UpdateLowAutoMakeUpGain;
      1: UpdateMidAutoMakeUpGain;
      2: UpdateHighAutoMakeUpGain;
     end;
  end else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterTimeDisplay(
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

procedure TSmoothMultibandCompressorDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterLowFreqChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLinkwitzRiley) - 1
  do FLinkwitzRiley[Channel, 0].Frequency := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do UpdateLowFrequency;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterHighChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FLinkwitzRiley) - 1
  do FLinkwitzRiley[Channel, 1].Frequency := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do UpdateHighFrequency;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterLimitChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmSmoothMultibandCompressor
  then TFmSmoothMultibandCompressor(EditorForm).UpdateLimit;
end;

procedure TSmoothMultibandCompressorDataModule.ChooseProcess;
begin
 case round(Parameter[3]) of
  0 : OnProcess := VSTModuleProcessMono;
  1 : OnProcess := VSTModuleProcessMonoSoftClip;
 end;
 OnProcessReplacing := OnProcess;
end;

function TSmoothMultibandCompressorDataModule.GetAutoGain(
  Index: Integer): Boolean;
begin
 if Index in [0..Length(FSmoothMultibandCompressor) - 1]
  then result := FSmoothMultibandCompressor[Index].AutoMakeUp
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

function TSmoothMultibandCompressorDataModule.GetSmoothMultibandCompressor(Index: Integer): TLightweightSoftKneeCompressor;
begin
 if Index in [0..Length(FSmoothMultibandCompressor) - 1]
  then result := FSmoothMultibandCompressor[Index]
  else raise Exception.CreateFmt('Index out of bounds (%d)', [Index]);
end;

procedure TSmoothMultibandCompressorDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 3) div 6;
 FSmoothMultibandCompressor[Band].Attack := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowAttack;
    1: UpdateMidAttack;
    2: UpdateHighAttack;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 4) div 6;
 FSmoothMultibandCompressor[Band].Release := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRelease;
    1: UpdateMidRelease;
    2: UpdateHighRelease;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 5) div 6;
 FSmoothMultibandCompressor[Band].Threshold_dB := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowThreshold;
    1: UpdateMidThreshold;
    2: UpdateHighThreshold;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 6) div 6;
 FSmoothMultibandCompressor[Band].Ratio := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowRatio;
    1: UpdateMidRatio;
    2: UpdateHighRatio;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 7) div 6;
 FSmoothMultibandCompressor[Band].Knee_dB := Value;
 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowKnee;
    1: UpdateMidKnee;
    2: UpdateHighKnee;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
var
  Band : Integer;
begin
 Band := (Index - 8) div 6;
 FSmoothMultibandCompressor[Band].MakeUpGain_dB := Value;

 if EditorForm is TFmSmoothMultibandCompressor then
  with TFmSmoothMultibandCompressor(EditorForm) do
   case Band of
    0: UpdateLowMakeUp;
    1: UpdateMidMakeUp;
    2: UpdateHighMakeUp;
   end;
end;

procedure TSmoothMultibandCompressorDataModule.ParameterAutoMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
//var Band : Integer;
begin
// Band := (Index - 9) div 6;
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleProcessMono(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : array [0..2] of Single;
  FD     : array [0..1, 0..2] of Single;
const
  CDenorm32 : Single = 1E-12;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split mid
   FLinkwitzRiley[0, 1].ProcessSample(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 2]);
   FLinkwitzRiley[1, 1].ProcessSample(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 2]);

   // split low
   FLinkwitzRiley[0, 0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1, 0].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // compress & copy gain reduction
   with FSmoothMultibandCompressor[0] do
    begin
     InputSample(CHalf32 * (FD[0, 0] + FD[1, 0]));
     Temp[0] := GainReductionFactor * MakeUpGain;
    end;
   with FSmoothMultibandCompressor[1] do
    begin
     InputSample(CHalf32 * (FD[0, 1] + FD[1, 1]));
     Temp[1] := GainReductionFactor * MakeUpGain;
    end;
   with FSmoothMultibandCompressor[2] do
    begin
     InputSample(CHalf32 * (FD[0, 2] + FD[1, 2]));
     Temp[2] := GainReductionFactor * MakeUpGain;
    end;


   // gain and combine
   Outputs[0, Sample] := Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2];
   Outputs[1, Sample] := Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2];
  end;
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleProcessMonoSoftClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : array [0..2] of Single;
  FD     : array [0..1, 0..2] of Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  begin
   // split mid
   FLinkwitzRiley[0, 1].ProcessSample(CDenorm32 + Inputs[0, Sample], FD[0, 1], FD[0, 2]);
   FLinkwitzRiley[1, 1].ProcessSample(CDenorm32 + Inputs[1, Sample], FD[1, 1], FD[1, 2]);

   // split low
   FLinkwitzRiley[0, 0].ProcessSample(FD[0, 1] - CDenorm32, FD[0, 0], FD[0, 1]);
   FLinkwitzRiley[1, 0].ProcessSample(FD[1, 1] - CDenorm32, FD[1, 0], FD[1, 1]);

   // compress
   FSmoothMultibandCompressor[0].ProcessSample(CHalf32 * (FD[0, 0] + FD[1, 0]));
   FSmoothMultibandCompressor[1].ProcessSample(CHalf32 * (FD[0, 1] + FD[1, 1]));
   FSmoothMultibandCompressor[2].ProcessSample(CHalf32 * (FD[0, 2] + FD[1, 2]));

   // copy gain reduction
   Temp[0] := FSmoothMultibandCompressor[0].GainReductionFactor;
   Temp[1] := FSmoothMultibandCompressor[1].GainReductionFactor;
   Temp[2] := FSmoothMultibandCompressor[2].GainReductionFactor;

   // gain and combine
   Outputs[0, Sample] := FastTanhOpt3Term(Temp[0] * FD[0, 0] + Temp[1] * FD[0, 1] - Temp[2] * FD[0, 2]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp[0] * FD[1, 0] + Temp[1] * FD[1, 1] - Temp[2] * FD[1, 2]);
  end;
end;

procedure TSmoothMultibandCompressorDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to Length(FSmoothMultibandCompressor) - 1 do
  begin
   FSmoothMultibandCompressor[Channel].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 0].SampleRate := SampleRate;
   FLinkwitzRiley[Channel, 1].SampleRate := SampleRate;
  end;
end;

end.
