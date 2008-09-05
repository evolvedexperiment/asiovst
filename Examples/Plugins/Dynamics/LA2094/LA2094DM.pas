unit LA2094DM;

interface

uses 
  Windows, Messages, SysUtils, Classes, Forms, DAVDCommon, DVSTModule,
  DDSPDynamics, DDSPLevelingAmplifier;

type
  TLA2094DataModule = class(TVSTModule)
    procedure SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLOutputChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure SKLSKFBChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
  private
    fLA2094s : Array [0..1] of TLevelingAmplifier;
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TLA2094DataModule.VSTModuleCreate(Sender: TObject);
begin
 fLA2094s[0] := TLevelingAmplifier.Create;
 fLA2094s[1] := TLevelingAmplifier.Create;
end;

procedure TLA2094DataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fLA2094s[0]);
 FreeAndNil(fLA2094s[1]);
end;

procedure TLA2094DataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
end;

procedure TLA2094DataModule.SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s[0].Input_dB := Value;
 fLA2094s[1].Input_dB := Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialInput.Position <> Value then
    begin
     DialInput.Position := Value;
     UpdateInput;
    end;
end;

procedure TLA2094DataModule.SKLOutputChange(
  Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s[0].Output_dB := Value;
 fLA2094s[1].Output_dB := Value;

 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialOutput.Position <> Value then
    begin
     DialOutput.Position := Value;
     UpdateOutput;
    end;
end;

procedure TLA2094DataModule.SKLSKFBChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s[0].SoftKnee := Value / 20;
 fLA2094s[1].SoftKnee := Value / 20;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialKnee.Position <> Value then
    begin
     DialKnee.Position := Value;
     UpdateKnee;
    end;
end;

procedure TLA2094DataModule.SKLRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s[0].Ratio := 1 / Value;
 fLA2094s[1].Ratio := 1 / Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialRatio.Position <> Log10(Value) then
    begin
     DialRatio.Position := Log10(Value);
     UpdateRatio;
    end;
end;

procedure TLA2094DataModule.SKLReleaseChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s[0].Decay := Value;
 fLA2094s[1].Decay := Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialRelease.Position <> Log10(Value) then
    begin
     DialRelease.Position := Log10(Value);
     UpdateRelease;
    end;
end;

procedure TLA2094DataModule.SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s[0].Attack := Value;
 fLA2094s[1].Attack := Value;
 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialAttack.Position <> Log10(Value) then
    begin
     DialAttack.Position := Log10(Value);
     UpdateAttack;
    end;
end;

procedure TLA2094DataModule.VSTModuleProcess(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0,i] := fLA2094s[0].ProcessSample(Inputs[0, i]);
   Outputs[1,i] := fLA2094s[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TLA2094DataModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   Outputs[0, i] := fLA2094s[0].ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fLA2094s[1].ProcessSample(Inputs[1, i]);
  end;
end;

procedure TLA2094DataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(fLA2094s[0])
  then fLA2094s[0].SampleRate := SampleRate;
 if Assigned(fLA2094s[1])
  then fLA2094s[1].SampleRate := SampleRate;
end;

end.
