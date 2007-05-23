{$J-,H+,T-P+,X+,B-,V-,O+,A+,W-,U-,R-,I-,Q-,D-,L-,Y-,C-}
library NoGUIFilter;

{$I ASIOVST.INC}

uses
  Interfaces,
  DVSTEffect,
  DVSTModule,
  FilterModule in 'FilterModule.pas';

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var VSTModule1 : TVSTFilter;
begin
  VSTModule1:=TVSTFilter.Create(nil);
  VSTModule1.Effect^.user:=VSTModule1;
  VSTModule1.AudioMaster:=audioMaster;
  Result := nil;

  with VSTModule1 do
  begin
    Flags := [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing];
    Version := '0.0';
    EffectName := 'Delphi VST Filter';
    ProductName := 'Delphi VST Filter';
    VendorName := 'Delphi VST';
    PlugCategory := cgEffect;
    TailSize := 0;
    CanDos := [plugAsChannelInsert, plugAsSend, _1in1out, _1in2out, _2in1out, _2in2out];
    SampleRate := 44100.0;
    CurrentProgram := 0;
    CurrentProgramName := 'Preset 1';
    KeysRequired := False;
    UniqueID := 'Filt';
    with (Programs.Add) do
    begin
      DisplayName := 'Preset 1';
      VSTModule:=VSTModule1;
    end;
    with (Programs.Add) do
    begin
      DisplayName := 'Preset 2';
      VSTModule:=VSTModule1;
    end;
    with (Programs.Add) do
    begin
      DisplayName := 'Preset 3';
      VSTModule:=VSTModule1;
    end;

    with ParameterProperties.Add do
    begin
      Min := 20.0;
      Max := 20000.0;
      Curve := ctLinear;
      DisplayName := 'Cutoff Frequency';
      Units := 'Hz';
      CurveFactor := 1.0;
      SmoothingFactor := 1.0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 100.0;
      SmallStepFloat := 100.0;
      LargeStepFloat := 1000.0;
      Flags := [];
      MinInteger := 20;
      MaxInteger := 20000;
      StepInteger := 100;
      LargeStepInteger := 1000;
      ShortLabel := 'Cutoff';
      VSTModule := VSTModule1;
      OnParameterChange := VSTFilterParameterProperties0ParameterChange;
    end;
    with ParameterProperties.Add do
    begin
      Min := 0.01;
      Max := 20.0;
      Curve := ctLinear;
      DisplayName := 'Resonance';
      CurveFactor := 1.0;
      SmoothingFactor := 1.0;
      CanBeAutomated := True;
      ReportVST2Properties := False;
      StepFloat := 0.1;
      SmallStepFloat := 0.1;
      LargeStepFloat := 1.0;
      Flags := [];
      MinInteger := 0;
      MaxInteger := 20;
      StepInteger := 1;
      LargeStepInteger := 1;
      ShortLabel := 'Res';
      VSTModule := VSTModule1;
    end;
    OnProcess := VSTModuleProcess;
    OnProcessReplacing := VSTModuleProcess;
    OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacing;
    OnInitialize := VSTModuleInitialize;
  end;
  Result := VSTModule1.Effect;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
