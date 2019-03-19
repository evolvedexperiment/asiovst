object SubSynthDataModule: TSubSynthDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda SubSynth'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Sub Bass Synthesizer'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaB'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Sub Bass Synthesizer'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Type'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ShortLabel = 'Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterModeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Level'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Level'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterLevelChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tune'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Tune'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterTuneDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry Mix'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Dry Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDryChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Thresh'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -60.000000000000000000
      MinInteger = -60
      ShortLabel = 'Thresh'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterDisplay = ParameterReleaseDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnResume = VSTModuleResume
  Height = 150
  Width = 215
end
