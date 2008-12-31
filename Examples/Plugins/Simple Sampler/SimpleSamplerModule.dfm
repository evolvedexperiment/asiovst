object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'Simple Sampler'
  ProductName = 'DAV Synth Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'Fibo'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessMidi = VSTModuleProcessMidi
  OnProcessReplacing = VSTModuleProcess
  Left = 247
  Top = 85
  Height = 150
  Width = 215
end
