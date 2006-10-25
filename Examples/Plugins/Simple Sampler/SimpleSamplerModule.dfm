object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth, effFlagsExtIsAsync]
  Version = '1.0'
  EffectName = 'Simple Sampler'
  ProductName = 'Simple Sampler'
  VendorName = 'VST Plugin Wizard Example'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = cgEffect
  TailSize = 0
  CanDos = [sendVstEvents, sendVstMidiEvent, sendVstTimeInfo, receiveVstEvents, receiveVstMidiEvent, receiveVstTimeInfo, plugAsChannelInsert, plugAsSend, _1in1out, _1in2out, _2in1out, _2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  KeysRequired = False
  UniqueID = 'Fibo'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = 2.000000000000000000
      Max = 32.000000000000000000
      Curve = ctLinear
      DisplayName = 'Order'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      Flags = [ppfUsesIntegerMinMax, ppfUsesIntStep]
      MinInteger = 2
      MaxInteger = 32
      StepInteger = 1
      LargeStepInteger = 2
      ShortLabel = 'Order'
      VSTModule = Owner
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
