unit SKLDM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

type
  TSoftKneeLimiterDataModule = class(TVSTModule)
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLSoftKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure VSTModuleOpen(Sender: TObject);
  private
    fSoftKneeLimiters : Array [0..1] of TSoftKneeLimiter;
  public
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TSoftKneeLimiterDataModule.SKLThresholdChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Threshold := Value;
 fSoftKneeLimiters[1].Threshold := Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialThreshold.Position <> Value then
    begin
     DialThreshold.Position := Value;
     UpdateThreshold;
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLSoftKneeChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].SoftKnee := 2 * (20 - Value);
 fSoftKneeLimiters[1].SoftKnee := 2 * (20 - Value);
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialSoftKnee.Position <> Value then
    begin
     DialSoftKnee.Position := Value;
     UpdateSoftKnee;
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLRatioChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Ratio := 1 / Value;
 fSoftKneeLimiters[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialRatio.Position <> Log10(Value) then
    begin
     DialRatio.Position := Log10(Value);
     UpdateRatio;
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLReleaseChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Decay := Value;
 fSoftKneeLimiters[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialRelease.Position <> Log10(Value) then
    begin
     DialRelease.Position := Log10(Value);
     UpdateRelease;
    end;
end;

procedure TSoftKneeLimiterDataModule.SKLAttackChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fSoftKneeLimiters[0].Attack := Value;
 fSoftKneeLimiters[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialAttack.Position <> Log10(Value) then
    begin
     DialAttack.Position := Log10(Value);
     UpdateAttack;
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

procedure TSoftKneeLimiterDataModule.VSTModuleOpen(Sender: TObject);
begin
 Parameter[0] := 0;
 Parameter[1] := 1;
 Parameter[2] := 5;
 Parameter[3] := 40;
 Parameter[4] := 6;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleProcess(const Inputs, Outputs:
  TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fSoftKneeLimiters[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fSoftKneeLimiters[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TSoftKneeLimiterDataModule.VSTModuleProcessDoubleReplacing(const Inputs,
  Outputs: TDAVArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fSoftKneeLimiters[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fSoftKneeLimiters[1].ProcessSample(Inputs[1, i]);
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
