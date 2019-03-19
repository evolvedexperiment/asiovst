object ImageDataModule: TImageDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Image'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Stereo Image / MS Matrix'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaI'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Stereo Image / MS Matrix'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mode'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterModeChange
      OnCustomParameterDisplay = ParamModeDisplay
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'S Width'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'S Width'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterSWidthChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'S Pan'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'S Pan'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'L<->R'
      VSTModule = Owner
      OnParameterChange = ParameterSPanChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'M Level'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'M Level'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterMLevelChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'M Pan'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'M Pan'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'L<->R'
      VSTModule = Owner
      OnParameterChange = ParameterMPanChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputGainChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  OnProcess64Replacing = VSTModuleProcessDoubleReplacing
  Height = 150
  Width = 215
end
