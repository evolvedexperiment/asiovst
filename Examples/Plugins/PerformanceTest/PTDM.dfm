object PerformanceTestModule: TPerformanceTestModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor]
  Version = '1.0'
  EffectName = 'Performance Tester'
  ProductName = 'Delphi ASIO & VST Project'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = []
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'DVPT'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
