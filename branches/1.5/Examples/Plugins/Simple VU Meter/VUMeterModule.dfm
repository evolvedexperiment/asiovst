object VSTVUMeterModule: TVSTVUMeterModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'VU Meter'
  ProductName = 'DAV Tools Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'VUMt'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Volume Left'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -90.000000000000000000
      ShortLabel = 'Volume '
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'dB'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Volume Right'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -90.000000000000000000
      ShortLabel = 'Volume '
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'dB'
      VSTModule = Owner
    end>
  ParameterCategories = <>
  OnEditOpen = VSTModuleEditOpen
  OnEditIdle = VSTModuleEditIdle
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  Height = 150
  Width = 215
end
