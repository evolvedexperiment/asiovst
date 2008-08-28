unit SimpleFeedbackCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  DDspDynamics;

type
  TSimpleFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
    procedure VSTModuleOpen(Sender: TObject);
  private
    fSimpleFeedbackCompressors : Array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TSimpleFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Threshold := Value;
 fSimpleFeedbackCompressors[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if DialThreshold.Position <> Round(Value) then
    begin
     DialThreshold.Position := Round(Value);
     UpdateThreshold;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Ratio := 1 / Value;
 fSimpleFeedbackCompressors[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if DialRatio.Position <> Round(100 * Log10(Value)) then
    begin
     DialRatio.Position := Round(100 * Log10(Value));
     UpdateRatio;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Decay := Value;
 fSimpleFeedbackCompressors[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if DialRelease.Position <> Round(Value) then
    begin
     DialRelease.Position := Round(1000 * Log10(Value));
     UpdateRelease;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Attack := Value;
 fSimpleFeedbackCompressors[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if DialAttack.Position <> Round(100 * Log10(Value)) then
    begin
     DialAttack.Position := Round(100 * Log10(Value));
     UpdateAttack;
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleFeedbackCompressors[0] := TSimpleFeedbackCompressor.Create;
 fSimpleFeedbackCompressors[1] := TSimpleFeedbackCompressor.Create;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleFeedbackCompressors[0]);
 FreeAndNil(fSimpleFeedbackCompressors[1]);
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;
 Parameter[1] := 1;
 Parameter[2] := 5;
 Parameter[3] := 40;
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
    Outputs[0, i] := fSimpleFeedbackCompressors[0].ProcessSample(Inputs[0, i]);
    Outputs[1, i] := fSimpleFeedbackCompressors[1].ProcessSample(Inputs[1, i]);
  end;
end;

end.
