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
    procedure VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessDoubleReplacingBypass(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure ParamRatioDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamAttackDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamAttackLabel(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure ParamOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fLA2094s : TLevelingAmplifier;
  end;

implementation

{$R *.DFM}

uses
  Math, EditorFrm;

procedure TLA2094DataModule.VSTModuleCreate(Sender: TObject);
begin
 fLA2094s := TLevelingAmplifier.Create;

 Parameter[0] := 0;
 Parameter[1] := 0;
 Parameter[2] := 0;
 Parameter[3] := 1;
 Parameter[4] := 100;
 Parameter[5] := 10;
 Parameter[6] := 5;
end;

procedure TLA2094DataModule.VSTModuleDestroy(Sender: TObject);
begin
 FreeAndNil(fLA2094s);
end;

procedure TLA2094DataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
end;

procedure TLA2094DataModule.SKLInputChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s.Input_dB := Value;

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
 fLA2094s.Output_dB := Value;

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
 fLA2094s.Knee := 0.1 * Value;

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
 fLA2094s.Ratio := 1 / Value;

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
 fLA2094s.Release_ms := Value;

 if Assigned(EditorForm) then
  with EditorForm as TEditorForm do
   if DialRelease.Position <> Log10(Value) then
    begin
     DialRelease.Position := Log10(Value);
     UpdateRelease;
    end;
end;

procedure TLA2094DataModule.ParamOnOffChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if Value < 0.5 then
  begin
   OnProcess := VSTModuleProcess;
   OnProcessReplacing := VSTModuleProcess;
   OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacing;
  end
 else
  begin
   OnProcess := VSTModuleProcessBypass;
   OnProcessReplacing := VSTModuleProcessBypass;
   OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacingBypass;
  end;
end;

procedure TLA2094DataModule.ParamAttackLabel(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1 then PreDefined := 'μs';
end;

procedure TLA2094DataModule.ParamAttackDisplay(
  Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Parameter[Index] < 1
  then PreDefined := FloatToStrF(1E3 * Parameter[Index], ffFixed, 3, 1)
  else PreDefined := FloatToStrF(      Parameter[Index], ffFixed, 3, 1);
end;

procedure TLA2094DataModule.ParamRatioDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 PreDefined := '1:' + PreDefined;
end;

procedure TLA2094DataModule.SKLAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 fLA2094s.Attack_ms := Value;

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
   fLA2094s.Sidechain(Inputs[0, i] + Inputs[1, i]);

   Outputs[0, i] := fLA2094s.ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fLA2094s.ProcessSample(Inputs[1, i]);
  end;
end;

procedure TLA2094DataModule.VSTModuleProcessDoubleReplacing(const Inputs, Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
var
  i : Integer;
begin
 for i := 0 to SampleFrames - 1 do
  begin
   fLA2094s.Sidechain(Inputs[0, i] + Inputs[1, i]);

   Outputs[0, i] := fLA2094s.ProcessSample(Inputs[0, i]);
   Outputs[1, i] := fLA2094s.ProcessSample(Inputs[1, i]);
  end;
end;

procedure TLA2094DataModule.VSTModuleProcessBypass(const Inputs, Outputs: TAVDArrayOfSingleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Single));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Single));
end;

procedure TLA2094DataModule.VSTModuleProcessDoubleReplacingBypass(const Inputs,
  Outputs: TAVDArrayOfDoubleDynArray; const SampleFrames: Integer);
begin
 Move(Inputs[0, 0], Outputs[0, 0], SampleFrames * SizeOf(Double));
 Move(Inputs[1, 0], Outputs[1, 0], SampleFrames * SizeOf(Double));
end;

procedure TLA2094DataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
begin
 if Assigned(fLA2094s)
  then fLA2094s.SampleRate := SampleRate;
end;

end.
