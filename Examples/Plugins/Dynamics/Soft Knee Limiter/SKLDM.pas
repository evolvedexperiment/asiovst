unit SKLDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule, DDspDynamics;

type
  TSoftKneeLimiterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray; const sampleframes: Integer);
    procedure SKLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TArrayOfDoubleDynArray; const sampleframes: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure SKLSoftKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
  private
    fSoftKneeLimiters : Array [0..1] of TSoftKneeLimiter;
  public
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TSoftKneeLimiterDataModule.SKLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Threshold := Value;
 fSoftKneeLimiters[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBThreshold.Position <> Round(Value) then
    begin
     SBThreshold.Position := Round(Value);
     LbThresholdValue.Caption := IntToStr(SBThreshold.Position) + ' dB';
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLSoftKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].SoftKnee := 20 - Value;
 fSoftKneeLimiters[1].SoftKnee := 20 - Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBSoftKnee.Position <> Round(10 * Value) then
    begin
     SBSoftKnee.Position := Round(10 * Value);
     LbSoftKneeValue.Caption := FloatToStrF(Parameter[4], ffGeneral, 4, 5);
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Ratio := 1 / Value;
 fSoftKneeLimiters[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRatio.Position <> Round(100 * Log10(Value)) then
    begin
     SBRatio.Position := Round(100 * Log10(Value));
     LbRatioValue.Caption := '1 : ' + FloatToStrF(Value, ffGeneral, 4, 4);
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Decay := Value;
 fSoftKneeLimiters[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBRelease.Position <> Round(Value) then
    begin
     SBRelease.Position := Round(1000 * Log10(Value));
     LbReleaseValue.Caption := FloatToStrF(Value, ffGeneral, 4, 5) + ' ms';
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Attack := Value;
 fSoftKneeLimiters[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do
   if SBAttack.Position <> Round(100 * Log10(Value)) then
    begin
     SBAttack.Position := Round(100 * Log10(Value));
     LbAttackValue.Caption := FloatToStrF(Value, ffGeneral, 4, 2) + ' ms';
    end;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleCreate(Sender: TObject);
begin
 fSoftKneeLimiters[0] := TSoftKneeLimiter.Create;
 fSoftKneeLimiters[1] := TSoftKneeLimiter.Create;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fSoftKneeLimiters[0]);
 FreeAndNil(fSoftKneeLimiters[1]);
end;

procedure TSoftKneeLimiterDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
end;

procedure TSoftKneeLimiterDataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
   Outputs[0,i] := fSoftKneeLimiters[0].ProcessSample(Inputs[0,i]);
   Outputs[1,i] := fSoftKneeLimiters[1].ProcessSample(Inputs[1,i]);
  end;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const sampleframes: Integer);
var i : Integer;
begin
 for i := 0 to sampleframes - 1 do
  begin
   Outputs[0,i] := fSoftKneeLimiters[0].ProcessSample(Inputs[0,i]);
   Outputs[1,i] := fSoftKneeLimiters[1].ProcessSample(Inputs[1,i]);
  end;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(fSoftKneeLimiters[0])
  then fSoftKneeLimiters[0].SampleRate := SampleRate;
 if Assigned(fSoftKneeLimiters[1])
  then fSoftKneeLimiters[1].SampleRate := SampleRate;
end;

end.
