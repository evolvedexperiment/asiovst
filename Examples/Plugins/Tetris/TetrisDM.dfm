object TetrisModule: TTetrisModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor]
  Version = '1.0'
  EffectName = 'Tetris'
  ProductName = 'Tetris'
  VendorName = 'VST Example'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vcgUnknown
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out, vcd2in4out, vcd4in2out, vcd4in4out, vcd4in8out, vcd8in4out, vcd8in8out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = -1
  KeysRequired = True
  UniqueID = 'Tetr'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnEditOpen = VSTModuleEditOpen
  OnEditorKeyDown = VSTModuleEditorKeyDown
  OnCheckKey = VSTModuleCheckKey
  Left = 269
  Top = 51
  Height = 150
  Width = 215
end
