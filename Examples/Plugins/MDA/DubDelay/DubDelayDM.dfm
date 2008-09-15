object DubDelayDataModule: TDubDelayDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda DubDelay'
  ProductName = 'DubDelay'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Dynamics'
  UniqueID = 'mda'#223
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Dynamics'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -40.000000000000000000
      MinInteger = -40
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Ratio'
      Max = 1.000000000000000000
      ShortLabel = 'Ratio'
      SmoothingFactor = 1.000000000000000000
      Units = ':1'
      VSTModule = Owner
      OnParameterChange = ParameterRatioChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      Max = 40.000000000000000000
      MaxInteger = 40
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Max = 1.000000000000000000
      ShortLabel = 'Attack'
      SmoothingFactor = 1.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Limiter'
      Max = 1.000000000000000000
      ShortLabel = 'Limiter'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterLimiterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gate Threshold'
      Max = 1.000000000000000000
      ShortLabel = 'Gate Th'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGateThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gate Attack'
      Max = 1.000000000000000000
      ShortLabel = 'Gate At'
      SmoothingFactor = 1.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = ParameterGateAttackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gate Release'
      Max = 1.000000000000000000
      ShortLabel = 'Gate Re'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterGateReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mix'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterMixChange
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
