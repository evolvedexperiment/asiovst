object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  OnDestroy = VSTModuleDestroy
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
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc1 Type'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 10
      StepInteger = 0
      LargeStepInteger = 0
      ShortLabel = '1type'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1TypeChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc1 Attack'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1AttackChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc1 Decay'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1DecayChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc1 Release'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1ReleaseChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc1 Sustain'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1SustainChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc1 Level'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc1LevelChange
    end
    item
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc2 Type'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 10
      StepInteger = 0
      LargeStepInteger = 0
      ShortLabel = '2type'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2TypeChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc2 Attack'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2AttackChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc2 Decay'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2DecayChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc2 Release'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2ReleaseChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc2 Sustain'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2SustainChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Osc2 Level'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      VSTModule = Owner
      OnParameterChange = VSTSSModuleOsc2LevelChange
    end
    item
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Drive'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      MaxInteger = 10
      LargeStepInteger = 2
      ShortLabel = 'drive'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleDriveParameterChange
    end
    item
      Min = 20.000000000000000000
      Max = 20000.000000000000000000
      Curve = ctLinear
      DisplayName = 'Cutoff'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 100.000000000000000000
      SmallStepFloat = 100.000000000000000000
      LargeStepFloat = 1000.000000000000000000
      MinInteger = 20
      MaxInteger = 20000
      StepInteger = 100
      LargeStepInteger = 1000
      ShortLabel = 'Cutoff'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleCutoffParameterChange
    end
    item
      Min = 0.009999999776482582
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Resonance'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 10
      LargeStepInteger = 1
      ShortLabel = 'Res'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleResonanceParameterChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output Level'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 5
      ShortLabel = 'level'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleLevelParameterChange
    end>
  OnOpen = VSTModuleOpen
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnInitialize = VSTModuleInitialize
  Left = 207
  Top = 191
  Height = 150
  Width = 215
end
