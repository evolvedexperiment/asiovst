object OverdriveDataModule: TOverdriveDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Overdrive'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Soft Overdrive'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaO'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Soft Overdrive'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Drive'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Drive'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDriveChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Muffle'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Muffle'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterMuffleChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Height = 150
  Width = 215
end
