object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'Sine Synth'
  ProductName = 'Sine Synth'
  VendorName = 'VST Plugin Wizard Example'
  PlugCategory = vpcSynth
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'SiSy'
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
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      Flags = [kVstParameterUsesIntStep]
      MinInteger = 2
      MaxInteger = 32
      LargeStepInteger = 2
      ShortLabel = 'Order'
      VSTModule = Owner
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnInitialize = VSTModuleInitialize
  Left = 200
  Top = 103
  Height = 150
  Width = 215
end
