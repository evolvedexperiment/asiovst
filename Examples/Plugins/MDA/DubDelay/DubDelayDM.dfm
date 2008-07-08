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
      Min = -40.000000000000000000
      Curve = ctLinear
      DisplayName = 'Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      MinInteger = -40
      MaxInteger = 0
      LargeStepInteger = 2
      ShortLabel = 'Thresho'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Ratio'
      Units = ':1'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Ratio'
      VSTModule = Owner
      OnParameterChange = ParameterRatioChange
    end
    item
      Max = 40.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      MaxInteger = 40
      ShortLabel = 'Output'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Attack'
      Units = #181's'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Attack'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Release'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Release'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Limiter'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Limiter'
      VSTModule = Owner
      OnParameterChange = ParameterLimiterChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Gate Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Gate Th'
      VSTModule = Owner
      OnParameterChange = ParameterGateThresholdChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Gate Attack'
      Units = #181's'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Gate At'
      VSTModule = Owner
      OnParameterChange = ParameterGateAttackChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Gate Release'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Gate Re'
      VSTModule = Owner
      OnParameterChange = ParameterGateReleaseChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mix'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      ShortLabel = 'Mix'
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
