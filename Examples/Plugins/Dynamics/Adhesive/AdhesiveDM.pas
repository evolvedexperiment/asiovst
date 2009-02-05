unit AdhesiveDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics, DAV_DspButterworthFilter;

type
  TAdhesiveDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessPeakClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessSC(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessSCPeakClip(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParameteActiveChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterExtSideChsinChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterKneeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMakeUpGainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterMakeUpGainDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterMixChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterPeakClipChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterSideChainChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParameterThresholdDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParameterTimeLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
  private
    FCompressor : TFastCompressor;
    FFilter     : TButterworthHP;
    FMix        : array [0..1] of Single;
    procedure ChooseProcess;
    procedure MixChanged;
  public
    property FastCompressor: TFastCompressor read FCompressor;
  end;

implementation

{$R *.DFM}

uses
  Math, DAV_Approximations, AdhesiveGUI, DAV_VSTModuleWithPrograms;

procedure TAdhesiveDataModule.VSTModuleOpen(Sender: TObject);
var
  Preset : Integer;
const
  CPresets : array [1..10, 0..10] of Single = (
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0),
    (-22, 16, 10, 3, 0.1, 400, 100, 1, 1, 20, 0));
begin
 FCompressor := TFastCompressor.Create;
 FCompressor.SampleRate := SampleRate;
 FFilter := TButterworthHP.Create;
 FFilter.SampleRate := SampleRate;
 FFilter.Order := 3;

 Parameter[ 0] := -22;
 Parameter[ 1] := 16;
 Parameter[ 2] := 10;
 Parameter[ 3] := 3;
 Parameter[ 4] := 0.1;
 Parameter[ 5] := 400;
 Parameter[ 6] := 100;
 Parameter[ 7] := 1;
 Parameter[ 8] := 1;
 Parameter[ 9] := 20;
 Parameter[10] := 0;

 Programs[0].SetParameters(FParameter);
 for Preset := 1 to numPrograms - 1
  do Programs[Preset].SetParameters(CPresets[Preset]);
end;

procedure TAdhesiveDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FCompressor);
end;

procedure TAdhesiveDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TFmAdhesive.Create(Self);
end;

procedure TAdhesiveDataModule.ParameterMixChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 MixChanged;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateMix;
end;

procedure TAdhesiveDataModule.MixChanged;
begin
 FMix[1] := sqrt(0.01 * Parameter[6]);
 FMix[0] := 1 - FMix[1];
 FMix[1] := FMix[1] * dB_to_Amp(Parameter[1]);
end;

procedure TAdhesiveDataModule.ParameterTimeLabel(
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

procedure TAdhesiveDataModule.ParameterTimeDisplay(
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

procedure TAdhesiveDataModule.ParameterMakeUpGainDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TAdhesiveDataModule.ParameterMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 //FCompressor.MakeUpGain_dB := Value;
 MixChanged;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateMakeUp;
end;

procedure TAdhesiveDataModule.ParameterThresholdDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TAdhesiveDataModule.ParameterRatioDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TAdhesiveDataModule.ParameterKneeDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 PreDefined := FloatToStrF(RoundTo(Parameter[Index], -2), ffGeneral, 3, 3);
end;

procedure TAdhesiveDataModule.ParameterOnOffDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 case round(Parameter[Index]) of
  0 : PreDefined := 'Off';
  1 : PreDefined := 'On';
 end;
end;

procedure TAdhesiveDataModule.ParameteActiveChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateOnOff;
end;

procedure TAdhesiveDataModule.ParameterPeakClipChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 ChooseProcess;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdatePeakClip;
end;

procedure TAdhesiveDataModule.ParameterSideChainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FFilter.Frequency := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateSideChainFilter;
end;

procedure TAdhesiveDataModule.ChooseProcess;
begin
 case round(Parameter[7]) of
  0 : OnProcess := VSTModuleProcessBypass;
  1 : case round(Parameter[8]) of
       0 : case round(Parameter[10]) of
            0: OnProcess := VSTModuleProcess;
            1: OnProcess := VSTModuleProcessSC;
           end;
       1 : case round(Parameter[10]) of
            0: OnProcess := VSTModuleProcessPeakClip;
            1: OnProcess := VSTModuleProcessSCPeakClip;
           end;
      end;
 end;
 OnProcessReplacing := OnProcess;
end;

procedure TAdhesiveDataModule.ParameterExtSideChsinChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateExtSideChain;
end;

procedure TAdhesiveDataModule.ParameterAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressor.Attack := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateAttack;
end;

procedure TAdhesiveDataModule.ParameterReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressor.Release := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateRelease;
end;

procedure TAdhesiveDataModule.ParameterThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressor.Threshold_dB := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateThreshold;
end;

procedure TAdhesiveDataModule.ParameterRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressor.Ratio := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateRatio;
end;

procedure TAdhesiveDataModule.ParameterKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 FCompressor.Knee_dB := Value;
 if EditorForm is TFmAdhesive
  then TFmAdhesive(EditorForm).UpdateKnee;
end;

procedure TAdhesiveDataModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TAdhesiveDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
  begin
   ProcessSample(FFilter.ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
   Temp := FMix[0] + FMix[1] * GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TAdhesiveDataModule.VSTModuleProcessPeakClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
  begin
   ProcessSample(FFilter.ProcessSample(CHalf32 * (Inputs[0, Sample] + Inputs[1, Sample])));
   Temp := FMix[0] + FMix[1] * GainReductionFactor;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TAdhesiveDataModule.VSTModuleProcessSC(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
  begin
   ProcessSample(FFilter.ProcessSample(Inputs[2, Sample]));
   Temp := FMix[0] + FMix[1] * GainReductionFactor;
   Outputs[0, Sample] := Temp * Inputs[0, Sample];
   Outputs[1, Sample] := Temp * Inputs[1, Sample];
  end;
end;

procedure TAdhesiveDataModule.VSTModuleProcessSCPeakClip(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  Sample : Integer;
  Temp   : Single;
begin
 for Sample := 0 to SampleFrames - 1 do
  with FCompressor do
  begin
   ProcessSample(FFilter.ProcessSample(Inputs[2, Sample]));
   Temp := FMix[0] + FMix[1] * GainReductionFactor;
   Outputs[0, Sample] := FastTanhOpt3Term(Temp * Inputs[0, Sample]);
   Outputs[1, Sample] := FastTanhOpt3Term(Temp * Inputs[1, Sample]);
  end;
end;

procedure TAdhesiveDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 FCompressor.SampleRate := SampleRate;
end;

end.