unit SoftKneeFeedbackCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSoftKneeFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure ParamMakeUpGainChange( Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleClose(Sender: TObject);
  private
    FSoftKneeFeedbackCompressors : Array [0..1] of TCustomFeedbackCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm, DAV_VSTModuleWithPrograms;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 FSoftKneeFeedbackCompressors[0] := TSoftKneeFeedbackCompressor.Create;
 FSoftKneeFeedbackCompressors[1] := TSoftKneeFeedbackCompressor.Create;

 // Initial Parameters
 Parameter[0] := -20;
 Parameter[1] :=   6;
 Parameter[2] :=   8;
 Parameter[3] := 500;
 Parameter[4] :=  10;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleClose(Sender: TObject);
begin
 FreeAndNil(FSoftKneeFeedbackCompressors[0]);
 FreeAndNil(FSoftKneeFeedbackCompressors[1]);
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleEditOpen(
  Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Threshold_dB := Value;
 if assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateThreshold;
end;

procedure TSoftKneeFeedbackCompressorDataModule.ParamMakeUpGainChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].MakeUpGain_dB := Value;
 if assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].MakeUpGain_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateMakeUpGain;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLRatioChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
  if assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Ratio := 1 / Value;
 if assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Ratio := 1 / Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRatio;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Release := Value;
 if assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Release := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm)
   do UpdateRelease;
end;

procedure TSoftKneeFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].Attack := Value;
 if assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].Attack := Value;
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
   Outputs[0, i] := FSoftKneeFeedbackCompressors[0].ProcessSample64(Inputs[0, i]);
   Outputs[1, i] := FSoftKneeFeedbackCompressors[1].ProcessSample64(Inputs[1, i]);
  end;
end;

procedure TSoftKneeFeedbackCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 if assigned(FSoftKneeFeedbackCompressors[0])
  then FSoftKneeFeedbackCompressors[0].SampleRate := SampleRate;
 if assigned(FSoftKneeFeedbackCompressors[1])
  then FSoftKneeFeedbackCompressors[1].SampleRate := SampleRate;
end;

end.
