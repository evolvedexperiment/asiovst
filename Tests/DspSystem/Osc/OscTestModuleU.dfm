object OscTestModule: TOscTestModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor]
  Version = '0.0'
  EffectName = 'OscTest'
  ProductName = 'OscTest'
  VendorName = 'MyCo'
  PlugCategory = vpcEffect
  CanDos = []
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  ProcessingMode = pmDspQueue
  UniqueID = 'ADDq'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnEditOpen = VSTModuleEditOpen
  Left = 192
  Top = 114
  Height = 150
  Width = 215
  object DspOscSaw1: TDspOscSaw
    SampleRate = 44100.000000000000000000
    Amplitude = 1.000000000000000000
    Frequency = 440.000000000000000000
    Left = 96
    Top = 32
  end
end
