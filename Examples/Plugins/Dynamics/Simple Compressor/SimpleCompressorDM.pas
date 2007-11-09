unit SimpleCompressorDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, DDspDynamics;

type
  TSimpleCompressorDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
  private
    fSimpleCompressors : Array [0..1] of TSimpleCompressor;
  public
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TSimpleCompressorDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Threshold := Value;
 fSimpleCompressors[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBThreshold.Position <> Round(Value) then
    begin
     SBThreshold.Position := Round(Value);
     LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
    end;
end;

procedure TSimpleCompressorDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Ratio := 1 / Value;
 fSimpleCompressors[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRatio.Position <> Round(100 * Log10(Value)) then
    begin
     SBRatio.Position := Round(100 * Log10(Value));
     LbRatioValue.Caption := '1 : ' + FloatToStrF(Value, ffGeneral, 4, 4);
    end;
end;

procedure TSimpleCompressorDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Decay := Value;
 fSimpleCompressors[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRelease.Position <> Round(Value) then
    begin
     SBRelease.Position := Round(1000 * Log10(Value));
     LbReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 4, 5) + ' ms';
    end;
end;

procedure TSimpleCompressorDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleCompressors[0].Attack := Value;
 fSimpleCompressors[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBAttack.Position <> Round(100 * Log10(Value)) then
    begin
     SBAttack.Position := Round(100 * Log10(Value));
     LbAttackValue.Caption := FloatToStrF(Value, ffGeneral, 4, 2) + ' ms';
    end;
end;

procedure TSimpleCompressorDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleCompressors[0] := TSimpleRMSCompressor.Create;
 fSimpleCompressors[1] := TSimpleRMSCompressor.Create;
end;

procedure TSimpleCompressorDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleCompressors[0]);
 FreeAndNil(fSimpleCompressors[1]);
end;

procedure TSimpleCompressorDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleCompressorDataModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
    Outputs[0,i] := fSimpleCompressors[0].ProcessSample(Inputs[0,i]);
    Outputs[1,i] := fSimpleCompressors[1].ProcessSample(Inputs[1,i]);
  end;
end;

end.
