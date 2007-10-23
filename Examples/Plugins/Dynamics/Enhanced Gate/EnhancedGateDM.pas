unit EnhancedGateDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms,
  DAVDCommon, DVSTModule, DDspDynamics;

const NrChannels = 2;

type
  TEnhancedGateDataModule = class(TVSTModule)
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
    procedure VSTModuleCreate(Sender: TObject);
    procedure VSTModuleDestroy(Sender: TObject);
    procedure VSTModuleProcess(const Inputs, Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
    procedure EAGThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure EAGOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure EAGPowerChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGDuckChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGStereoLinkChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGLoCutChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGHiCutChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGSideChainSourceDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
    procedure EAGSideChainSourceChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
    procedure EAGRangeChange(Sender: TObject; const Index: Integer; var Value: Single);
  private
    fEnhancedGates : Array [0..NrChannels-1] of TGate;
  public
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TEnhancedGateDataModule.VSTModuleCreate(Sender: TObject);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i] := TGate.Create;
 Parameter[ 0] :=   1.0;
 Parameter[ 1] := -60.0;
 Parameter[ 2] :=   0.1;
 Parameter[ 3] :=   0.1;
 Parameter[ 4] :=   0.1;
 Parameter[ 5] :=   0.0;
 Parameter[ 6] :=   0.0;
 Parameter[ 7] :=   0.0;
 Parameter[ 8] :=  20.0;
 Parameter[ 9] :=  20.0;
 Parameter[10] :=   1.0;
 Parameter[11] :=   1.0;
 Parameter[12] :=  40.0;
end;

procedure TEnhancedGateDataModule.VSTModuleDestroy(Sender: TObject);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do FreeAndNil(fEnhancedGates[i]);
end;

procedure TEnhancedGateDataModule.EAGPowerChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Boolean(Round(Value)) then
  begin
   OnProcess := VSTModuleProcess;
   OnProcessReplacing := OnProcess;
  end
 else
  begin
   OnProcess := VSTModuleProcessBypass;
   OnProcessReplacing := OnProcess;
  end;
 if assigned(EditorForm) then
  with TEditorForm(EditorForm) do
   if CBOnOff.Checked <> Boolean(Round(Value))
    then CBOnOff.Checked:=Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.EAGOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Boolean(Round(Parameter[index]))
  then PreDefined:='On'
  else PreDefined:='Off';
end;

procedure TEnhancedGateDataModule.EAGThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Threshold := Value;
 if Assigned(EditorForm)
  then (EditorForm As TEditorForm).UpdateThreshold;
end;

procedure TEnhancedGateDataModule.EAGRangeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Range := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do UpdateRange;
end;

procedure TEnhancedGateDataModule.EAGRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Ratio := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do UpdateRatio;
end;

procedure TEnhancedGateDataModule.EAGSideChainSourceChange(Sender: TObject;
  const Index: Integer; var Value: Single);
begin
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm
   do CBSideChain.ItemIndex := Integer(Round(Value));
end;

procedure TEnhancedGateDataModule.EAGSideChainSourceDisplay(Sender: TObject;
  const Index: Integer; var PreDefined: string);
begin
 if Boolean(Round(Parameter[index]))
  then PreDefined:='Ext'
  else PreDefined:='Int';
end;

procedure TEnhancedGateDataModule.EAGLoCutChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].SideChainLowCut := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do UpdateLoCut;
end;

procedure TEnhancedGateDataModule.EAGHiCutChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].SideChainHighCut := 1000 * Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do UpdateHiCut;
end;

procedure TEnhancedGateDataModule.EAGAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Attack := Value;
 if Assigned(EditorForm)
  then TEditorForm(EditorForm).UpdateAttack;
end;

procedure TEnhancedGateDataModule.EAGHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Hold := Value;
 if Assigned(EditorForm)
  then TEditorForm(EditorForm).UpdateHold;
end;

procedure TEnhancedGateDataModule.EAGKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Knee := Value;
 if Assigned(EditorForm) then
  with EditorForm As TEditorForm do UpdateKnee;
end;

procedure TEnhancedGateDataModule.EAGDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
var i : Integer;
begin
 for i := 0 to NrChannels - 1
  do fEnhancedGates[i].Decay := Value;
 if Assigned(EditorForm)
  then TEditorForm(EditorForm).UpdateDecay;
end;

procedure TEnhancedGateDataModule.EAGDuckChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(EditorForm) then
  with TEditorForm(EditorForm) do
   if CBDuck.Checked <> Boolean(Round(Value))
    then CBDuck.Checked:=Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.EAGStereoLinkChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if assigned(EditorForm) then
  with TEditorForm(EditorForm) do
   if CBStereoLink.Checked <> Boolean(Round(Value))
    then CBStereoLink.Checked:=Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.VSTModuleEditOpen(Sender: TObject; var GUI: TForm);
begin
 GUI := TEditorForm.Create(Self);
 with (GUI as TEditorForm) do
  begin
   CBOnOff.Checked:=Boolean(Round(Parameter[0]));
   UpdateThreshold;
   UpdateAttack;
   UpdateHold;
   UpdateDecay;
   UpdateHiCut;
   UpdateLoCut;
   UpdateRatio;
   UpdateKnee;
   UpdateRange;
  end;
end;

procedure TEnhancedGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var i,j : Integer;
begin
 for j := 0 to NrChannels - 1 do
  for i := 0 to sampleframes - 1 do
   begin
    fEnhancedGates[j].InputSideChain(Inputs[j,i]);
    Outputs[j,i] := fEnhancedGates[j].ProcessSample(Inputs[j,i]);
   end;
end;

procedure TEnhancedGateDataModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TArrayOfSingleDynArray; sampleframes: Integer);
var j : Integer;
begin
 for j := 0 to NrChannels - 1
  do Move(Inputs[j,0], Outputs[j,0], sampleframes * SizeOf(Single));
end;

procedure TEnhancedGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var j : Integer;
begin
 for j := 0 to NrChannels - 1
  do fEnhancedGates[j].SampleRate := SampleRate;
end;

end.
