unit SimpleLimiterDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, DDspDynamics;

type
  TSimpleLimiterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm;
      ParentWindow: Cardinal);
  private
    fSimpleLimiters : Array [0..1] of TSimpleLimiter;
  public
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TSimpleLimiterDataModule.SLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleLimiters[0].Threshold := Value;
 fSimpleLimiters[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBThreshold.Position <> Round(Value) then
    begin
     SBThreshold.Position := Round(Value);
     LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
    end;
end;

procedure TSimpleLimiterDataModule.SLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleLimiters[0].Ratio := 1 / Value;
 fSimpleLimiters[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRatio.Position <> Round(100 * Log10(Value)) then
    begin
     SBRatio.Position := Round(100 * Log10(Value));
     LbRatioValue.Caption := '1 : ' + FloatToStrF(Value, ffGeneral, 4, 4);
    end;
end;

procedure TSimpleLimiterDataModule.SLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleLimiters[0].Decay := Value;
 fSimpleLimiters[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRelease.Position <> Round(Value) then
    begin
     SBRelease.Position := Round(1000 * Log10(Value));
     LbReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 4, 5) + ' ms';
    end;
end;

procedure TSimpleLimiterDataModule.SLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSimpleLimiters[0].Attack := Value;
 fSimpleLimiters[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBAttack.Position <> Round(100 * Log10(Value)) then
    begin
     SBAttack.Position := Round(100 * Log10(Value));
     LbAttackValue.Caption := FloatToStrF(Value, ffGeneral, 4, 2) + ' ms';
    end;
end;

procedure TSimpleLimiterDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSimpleLimiters[0] := TSimpleLimiter.Create;
 fSimpleLimiters[1] := TSimpleLimiter.Create;
end;

procedure TSimpleLimiterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSimpleLimiters[0]);
 FreeAndNil(fSimpleLimiters[1]);
end;

procedure TSimpleLimiterDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
  GUI := TEditorForm.Create(Self);
end;

procedure TSimpleLimiterDataModule.VSTModuleProcess(const Inputs,
  Outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
    Outputs[0,i] := fSimpleLimiters[0].ProcessSample(Inputs[0,i]);
    Outputs[1,i] := fSimpleLimiters[1].ProcessSample(Inputs[1,i]);
  end;
end;

end.
