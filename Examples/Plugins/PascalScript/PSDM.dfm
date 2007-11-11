object PascalScriptDataModule: TPascalScriptDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsProgramChunks, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Pascal Script Effect Plugin'
  ProductName = 'Delphi ASIO & VST Project'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'PVST'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
      OnLoadChunk = VPSLoadChunk
      OnStoreChunk = VPSStoreChunk
    end>
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
