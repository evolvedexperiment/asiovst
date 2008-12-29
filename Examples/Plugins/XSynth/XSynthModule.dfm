object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'X Synth'
  ProductName = 'Sine Synth'
  VendorName = 'VST Plugin Wizard Example'
  PlugCategory = vpcSynth
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
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
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc1 Type'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 10.000000000000000000
      MaxInteger = 10
      ShortLabel = '1type'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1TypeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc1 Attack'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc1 At'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1AttackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc1 Decay'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc1 De'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1DecayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc1 Release'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc1 Re'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1ReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc1 Sustain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc1 Su'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1SustainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc1 Level'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc1 Le'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1LevelChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc2 Type'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 10.000000000000000000
      MaxInteger = 10
      ShortLabel = '2type'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2TypeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc2 Attack'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc2 At'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2AttackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc2 Decay'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc2 De'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2DecayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc2 Release'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc2 Re'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2ReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc2 Sustain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc2 Su'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2SustainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Osc2 Level'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Osc2 Le'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2LevelChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Drive'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 10.000000000000000000
      MaxInteger = 10
      ShortLabel = 'drive'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = VSTSSModuleDriveParameterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Cutoff'
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'Cutoff'
      SmallStepFloat = 100.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleCutoffParameterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Resonance'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ShortLabel = 'Res'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = VSTSSModuleResonanceParameterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output Level'
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 5
      Max = 100.000000000000000000
      ShortLabel = 'level'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleLevelParameterChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnProcessReplacing = VSTModuleProcess
  Left = 207
  Top = 191
  Height = 150
  Width = 215
end
