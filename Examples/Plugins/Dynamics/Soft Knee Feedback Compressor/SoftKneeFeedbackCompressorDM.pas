unit SoftKneeFeedbackCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSoftKneeFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
    procedure ParamMakeUpGainChange(
      Sender: TObject; const Index: Integer; var Value: Single);
  private
    fSoftKneeFeedbackCompressors : Array [0..1] of TCustomFeedbackCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm, DAV_VSTModuleWithPrograms;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSoftKneeFeedbackCompressors[0] := TSoftKneeFeedbackCompressor.Create;
 fSoftKneeFeedbackCompressors[1] := TSoftKneeFeedbackCompressor.Create;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSoftKneeFeedbackCompressors[0]);
 FreeAndNil(fSoftKneeFeedbackCompressors[1]);
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := -20;
 Parameter[1] :=   6;
 Parameter[2] :=   8;
 Parameter[3] := 500;
 Parameter[4] :=  10;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleEditOpen(
  Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeFeedbackCompressors[0].Threshold_dB := Value;
 fSoftKneeFeedbackCompressors[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSoftKneeFeedbackCompressorDataModule.ParamMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeFeedbackCompressors[0].MakeUpGain_dB := Value;
 fSoftKneeFeedbackCompressors[1].MakeUpGain_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateMakeUpGain;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLRatioChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 fSoftKneeFeedbackCompressors[0].Ratio := 1 / Value;
 fSoftKneeFeedbackCompressors[1].Ratio := 1 / Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRatio;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeFeedbackCompressors[0].Release := Value;
 fSoftKneeFeedbackCompressors[1].Release := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeFeedbackCompressors[0].Attack := Value;
 fSoftKneeFeedbackCompressors[1].Attack := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateAttack;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
    Outputs[0, i] := fSoftKneeFeedbackCompressors[0].ProcessSample(Inputs[0, i]);
    Outputs[1, i] := fSoftKneeFeedbackCompressors[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 fSoftKneeFeedbackCompressors[0].SampleRate := SampleRate;
 fSoftKneeFeedbackCompressors[1].SampleRate := SampleRate;
end;

end.
