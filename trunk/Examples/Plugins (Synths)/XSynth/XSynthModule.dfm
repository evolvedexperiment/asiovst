object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'X Synth'
  ProductName = 'DAV Synth Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcSynth
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'XSyn'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sweet'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc1'
      DisplayName = 'Osc1 Type'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = '1type'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1TypeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc1'
      DisplayName = 'Osc1 Attack'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc1 At'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1AttackChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc1'
      DisplayName = 'Osc1 Decay'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc1 De'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1DecayChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc1'
      DisplayName = 'Osc1 Release'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc1 Re'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1ReleaseChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc1'
      DisplayName = 'Osc1 Sustain'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc1 Su'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1SustainChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc1'
      DisplayName = 'Osc1 Level'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc1 Le'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1LevelChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc2'
      DisplayName = 'Osc2 Type'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = '2type'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2TypeChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc2'
      DisplayName = 'Osc2 Attack'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc2 At'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2AttackChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc2'
      DisplayName = 'Osc2 Decay'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc2 De'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2DecayChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc2'
      DisplayName = 'Osc2 Release'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc2 Re'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2ReleaseChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc2'
      DisplayName = 'Osc2 Sustain'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc2 Su'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2SustainChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Osc2'
      DisplayName = 'Osc2 Level'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Osc2 Le'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2LevelChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Output Stage'
      DisplayName = 'Drive'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'drive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = VSTSSModuleDriveParameterChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Output Stage'
      DisplayName = 'Cutoff'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Cutoff'
      SmallStepFloat = 100.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleCutoffParameterChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Output Stage'
      DisplayName = 'Resonance'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Res'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = VSTSSModuleResonanceParameterChange
    end
    item
      CurveFactor = 1.000000000000000000
      Category = 'Output Stage'
      DisplayName = 'Output Level'
      Flags = [ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 5
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'level'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleLevelParameterChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Osc1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Osc2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Output Stage'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess32Replacing = VSTModuleProcess32Replacing
  OnProcess64Replacing = VSTModuleProcess64Replacing
  OnProcessMidi = VSTModuleProcessMidi
  Height = 150
  Width = 215
end
