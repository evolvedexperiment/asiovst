library NoGUIFilter;

{$I DAV_Compiler.INC}

uses
  Interfaces,
  DAV_VSTEffect,
  DAV_VSTModule,
  DAV_VSTParameters,
  FilterModule in 'FilterModule.pas';

function main(audioMaster: TAudioMasterCallbackFunc): PVSTEffect; cdecl; export;
var
  VSTModule1 : TVSTFilter;
begin
  VSTModule1 := TVSTFilter.Create(nil);
  VSTModule1.Effect^.user := VSTModule1;
  VSTModule1.AudioMaster := audioMaster;
  Result := VSTModule1.Effect;

  with VSTModule1 do
  try
    Flags := [effFlagsCanReplacing, effFlagsCanDoubleReplacing];
    Version := '1.0';
    EffectName := 'Delphi VST Filter';
    ProductName := 'Delphi VST Filter';
    VendorName := 'Delphi VST';
    PlugCategory := vpcEffect;
    TailSize := 0;
    CanDos := [vcdplugAsChannelInsert, vcdplugAsSend, vcd1in1out, vcd1in2out,
               vcd2in1out, vcd2in2out];
    SampleRate := 44100.0;
    KeysRequired := False;
    UniqueID := 'Filt';
    OnProcess := VSTModuleProcess;
    OnProcessReplacing := VSTModuleProcess;
    OnProcessDoubleReplacing := VSTModuleProcessDoubleReplacing;
    OnOpen := VSTModuleOpen;

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

    with (ParameterProperties.Add) do
     begin
      VSTModule := VSTModule1;
      Min := 20.0;
      Max := 20000.0;
      Curve := ctLinear;
      DisplayName := 'Cutoff Frequency';
      Units := 'Hz';
      CurveFactor := 1000.0;
      SmoothingFactor := 0;
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
      OnParameterChange := VSTFilterParameterChange;
    end;
    with (ParameterProperties.Add) do
     begin
      Min := 0.01;
      Max := 20.0;
      Curve := ctLinear;
      DisplayName := 'Resonance';
      CurveFactor := 1.0;
      SmoothingFactor := 0;
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
    CurrentProgram := 0;
    if Assigned(OnCreate) then OnCreate(VSTModule1);
    if Assigned(OnInitialize) then OnInitialize(VSTModule1);
  except
  end;
end;

exports Main name 'main';
exports Main name 'VSTPluginMain';

begin
end.
