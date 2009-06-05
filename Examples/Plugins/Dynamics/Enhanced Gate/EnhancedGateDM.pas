unit EnhancedGateDM;

interface

uses
  Windows, Messages, SysUtils, Classes, Forms, DAV_Common, DAV_VSTModule,
  DAV_DspDynamics;

const
  CNrChannels = 2;

type
  TEnhancedGateDataModule = class(TVSTModule)
    procedure VSTModuleOpen(Sender: TObject);
    procedure VSTModuleClose(Sender: TObject);
    procedure VSTModuleEditOpen(Sender: TObject; var GUI: TForm; ParentWindow: Cardinal);
    procedure VSTModuleProcess(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleProcessBypass(const Inputs, Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
    procedure VSTModuleSampleRateChange(Sender: TObject; const SampleRate: Single);
    procedure EAGThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
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
    FEnhancedGates : Array [0..CNrChannels - 1] of TAdvancedGate;
    FLevels        : Array [0..CNrChannels - 1] of Single;
  public
    property LevelLeft: Single read FLevels[0];
    property LevelRight: Single read FLevels[1];
  end;

implementation

{$R *.DFM}

uses Math, EditorFrm;

procedure TEnhancedGateDataModule.VSTModuleOpen(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to CNrChannels - 1
  do FEnhancedGates[i] := TAdvancedGate.Create;

 // initial parameters 
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

procedure TEnhancedGateDataModule.VSTModuleClose(Sender: TObject);
var
  i : Integer;
begin
 for i := 0 to CNrChannels - 1
  do FreeAndNil(FEnhancedGates[i]);
end;

procedure TEnhancedGateDataModule.VSTModuleEditOpen(Sender: TObject;
  var GUI: TForm; ParentWindow: Cardinal);
begin
 GUI := TEditorForm.Create(Self);
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
   if CBOnOff.Brightness_Percent > 90 <> Boolean(Round(Value)) then
    if Boolean(Round(Value))
     then CBOnOff.Brightness_Percent := 100
     else CBOnOff.Brightness_Percent := 20;
end;

procedure TEnhancedGateDataModule.EAGOnOffDisplay(Sender: TObject; const Index: Integer; var PreDefined: string);
begin
 if Boolean(Round(Parameter[index]))
  then PreDefined := 'On'
  else PreDefined := 'Off';
end;

procedure TEnhancedGateDataModule.EAGThresholdChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Threshold_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  then (EditorForm As TEditorForm).UpdateThreshold;
end;

procedure TEnhancedGateDataModule.EAGRangeChange(Sender: TObject;
  const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Range_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateRange;
end;

procedure TEnhancedGateDataModule.EAGRatioChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Ratio := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
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
  then PreDefined := 'Ext'
  else PreDefined := 'Int';
end;

procedure TEnhancedGateDataModule.EAGLoCutChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].SideChainLowCut := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateLoCut;
end;

procedure TEnhancedGateDataModule.EAGHiCutChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].SideChainHighCut := 1000 * Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateHiCut;
end;

procedure TEnhancedGateDataModule.EAGAttackChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Attack := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  then TEditorForm(EditorForm).UpdateAttack;
end;

procedure TEnhancedGateDataModule.EAGHoldChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Hold := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  then TEditorForm(EditorForm).UpdateHold;
end;

procedure TEnhancedGateDataModule.EAGKneeChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Knee_dB := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  with EditorForm As TEditorForm do UpdateKnee;
end;

procedure TEnhancedGateDataModule.EAGDecayChange(Sender: TObject; const Index: Integer; var Value: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].Release := Value;

 // update GUI if necessary
 if EditorForm is TEditorForm then
  then TEditorForm(EditorForm).UpdateDecay;
end;

procedure TEnhancedGateDataModule.EAGDuckChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if CBDuck.Checked <> Boolean(Round(Value))
    then CBDuck.Checked := Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.EAGStereoLinkChange(Sender: TObject; const Index: Integer; var Value: Single);
begin
 if EditorForm is TEditorForm then
  with TEditorForm(EditorForm) do
   if CBStereoLink.Checked <> Boolean(Round(Value))
    then CBStereoLink.Checked := Boolean(Round(Value));
end;

procedure TEnhancedGateDataModule.VSTModuleProcess(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  i, j : Integer;
begin
 for j := 0 to CNrChannels - 1 do
  for i := 0 to SampleFrames - 1 do
   begin
    FEnhancedGates[j].InputSample(Inputs[j,i]);
    Outputs[j,i] := FEnhancedGates[j].ProcessSample(Inputs[j, i]);
    FLevels[j] := 0.99 * FLevels[j];
    if abs(Inputs[j, i]) > FLevels[j]
     then FLevels[j] := abs(Inputs[j, i]);
   end;
end;

procedure TEnhancedGateDataModule.VSTModuleProcessBypass(const Inputs,
  Outputs: TDAVArrayOfSingleDynArray; const SampleFrames: Integer);
var
  j : Integer;
begin
 FLevels[0] := 0;
 FLevels[1] := 0;
 for j := 0 to CNrChannels - 1
  do Move(Inputs[j,0], Outputs[j,0], SampleFrames * SizeOf(Single));
end;

procedure TEnhancedGateDataModule.VSTModuleSampleRateChange(Sender: TObject;
  const SampleRate: Single);
var
  Channel : Integer;
begin
 for Channel := 0 to CNrChannels - 1 do
  if assigned(FEnhancedGates[Channel])
   then FEnhancedGates[Channel].SampleRate := SampleRate;
end;

end.
