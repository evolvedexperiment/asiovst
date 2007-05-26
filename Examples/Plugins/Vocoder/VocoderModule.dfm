object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth, effFlagsExtIsAsync]
  Version = '1.0'
  EffectName = 'Vocoder'
  ProductName = 'Vocoder'
  VendorName = 'VST Plugin Wizard Example'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vcgSynth
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcdMixDryWet, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  KeysRequired = False
  UniqueID = 'Voco'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = -80.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Input Volume'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 5.000000000000000000
      Flags = []
      MinInteger = -80
      MaxInteger = 0
      StepInteger = 3
      LargeStepInteger = 10
      ShortLabel = 'vol in'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleParameterProperties0ParameterChange
    end
    item
      Min = -80.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Synth Volume'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 5.000000000000000000
      Flags = []
      MinInteger = -80
      MaxInteger = 0
      StepInteger = 3
      LargeStepInteger = 10
      ShortLabel = 'vol syn'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleParameterProperties1ParameterChange
    end
    item
      Min = -80.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Vocoder Volume'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 5.000000000000000000
      Flags = []
      MinInteger = -80
      MaxInteger = 0
      StepInteger = 3
      LargeStepInteger = 10
      ShortLabel = 'vol voc'
      VSTModule = Owner
      OnParameterChange = VSTSSModuleParameterProperties2ParameterChange
    end>
  OnEditOpen = VST_EditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnInitialize = VSTModuleInitialize
  Left = 200
  Top = 103
  Height = 150
  Width = 215
end
