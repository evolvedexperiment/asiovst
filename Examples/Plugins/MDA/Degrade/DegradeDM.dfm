object DegradeDataModule: TDegradeDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Degrade'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgramName = 'Degrade'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaC'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Degrade'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Headroom'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Headroo'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Quantize'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Quantiz'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'bits'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Rate'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'S<>S&&H'
      VSTModule = Owner
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'PostFilter'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'PstFilt'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterPostFilterChange
    end
    item
      CurveFactor = 1.000000000000000000
      DisplayName = 'Non-Linear'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Non-Lin'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Odd<>Even'
      VSTModule = Owner
      OnParameterChange = ParameterNonLinearChange
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
      OnParameterChange = ParameterOutputChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcess32Replacing = VSTModuleProcess
  Height = 150
  Width = 215
end
