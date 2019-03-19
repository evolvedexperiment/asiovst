object StereoDataModule: TStereoDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Stereo'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Stereo Simulator'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Stereo Simulator'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Width'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Width'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterWidthChange
      OnCustomParameterLabel = ParameterWidthLabel
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Delay'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Delay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Balance'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Mod'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterModChange
      OnCustomParameterDisplay = ParameterModDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Rate'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'sec'
      VSTModule = Owner
      OnParameterChange = ParameterRateChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  OnSuspend = VSTModuleSuspend
  Height = 150
  Width = 215
end
