object LoudnessDataModule: TLoudnessDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Loudness'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Equal Loudness Contours'
  IORatio = 1.000000000000000000
  UniqueID = 'mdal'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Equal Loudness Contours'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Loudness'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Loudnes'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Link'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Link'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 11
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterLinkDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  Height = 150
  Width = 215
end
