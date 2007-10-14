unit SimpleFeedbackCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DDSPBase, DVSTModule, DDynamics;

type
  TSimpleFeedbackCompressorDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fSimpleFeedbackCompressors : Array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TSimpleFeedbackCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Threshold := Value;
 fSimpleFeedbackCompressors[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBThreshold.Position <> Round(Value) then
    begin
     SBThreshold.Position := Round(Value);
     LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Ratio := 1 / Value;
 fSimpleFeedbackCompressors[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRatio.Position <> Round(100 * Log10(Value)) then
    begin
     SBRatio.Position := Round(100 * Log10(Value));
     LbRatioValue.Caption := '1 : ' + FloatToStrF(Value, ffGeneral, 4, 4);
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Decay := Value;
 fSimpleFeedbackCompressors[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRelease.Position <> Round(Value) then
    begin
     SBRelease.Position := Round(1000 * Log10(Value));
     LbReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 4, 5) + ' ms';
    end;
end;

procedure TSimpleFeedbackCompressorDataModule.SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleFeedbackCompressors[0].Attack := Value;
 fSimpleFeedbackCompressors[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBAttack.Position <> Round(100 * Log10(Value)) then
    begin
     SBAttack.Position := Round(100 * Log10(Value));
     LbAttackValue.Caption := FloatToStrF(Value, ffGeneral, 4, 2) + ' ms';
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

procedure TSimpleFeedbackCompressorDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleFeedbackCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
    Outputs[0,i] := fSimpleFeedbackCompressors[0].ProcessSample(Inputs[0,i]);
    Outputs[1,i] := fSimpleFeedbackCompressors[1].ProcessSample(Inputs[1,i]);
  end;
end;

end.
