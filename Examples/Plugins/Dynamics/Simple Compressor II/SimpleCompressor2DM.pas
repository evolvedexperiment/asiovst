unit SimpleCompressor2DM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSoftKneeCompressorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject;
      const SampleRate: Single);
  private
    fSimpleCompressors : Array [0..1] of TSoftKneeCompressor;
  public
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TSoftKneeCompressorDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleCompressors[0] := TSoftKneeCompressor.Create;
 fSimpleCompressors[1] := TSoftKneeCompressor.Create;
end;

procedure TSoftKneeCompressorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleCompressors[0]);
 FreeAndNil(fSimpleCompressors[1]);
end;

procedure TSoftKneeCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSoftKneeCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Threshold_dB := Value;
 fSimpleCompressors[1].Threshold_dB := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm As TEditorForm)
   do UpdateThreshold;
end;

procedure TSoftKneeCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Ratio := 1 / Value;
 fSimpleCompressors[1].Ratio := 1 / Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm As TEditorForm)
   do UpdateRatio;
end;

procedure TSoftKneeCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Release := Value;
 fSimpleCompressors[1].Release := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm As TEditorForm)
   do UpdateRelease;
end;

procedure TSoftKneeCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Attack := Value;
 fSimpleCompressors[1].Attack := Value;
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm As TEditorForm)
   do UpdateAttack;
end;

procedure TSoftKneeCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
    Outputs[0,i] := fSimpleCompressors[0].ProcessSample(Inputs[0,i]);
    Outputs[1,i] := fSimpleCompressors[1].ProcessSample(Inputs[1,i]);
  end;
end;

procedure TSoftKneeCompressorDataModule.VSTModuleSampleRateChange(
  Sender: TObject; const SampleRate: Single);
begin
 fSimpleCompressors[0].SampleRate := SampleRate;
 fSimpleCompressors[1].SampleRate := SampleRate;
end;

end.
