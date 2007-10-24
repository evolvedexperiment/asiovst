object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth, effFlagsUnused1]
  Version = '1.0'
  EffectName = 'Simple Sampler'
  ProductName = 'Simple Sampler'
  VendorName = 'VST Plugin Wizard Example'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'Fibo'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <>
  OnEditOpen = VST_EditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnInitialize = VSTModuleInitialize
  Left = 247
  Top = 85
  Height = 150
  Width = 215
end
