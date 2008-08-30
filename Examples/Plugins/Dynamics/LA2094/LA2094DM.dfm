object LA2094DataModule: TLA2094DataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Soft Knee Limiter'
  ProductName = 'Soft Knee Limiter'
  VendorName = 'ASIO-VST Delphi Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'SKLi'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Input'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 36.000000000000000000
      MaxInteger = 36
      Min = -36.000000000000000000
      MinInteger = -36
      ShortLabel = 'input'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SKLInputChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 36.000000000000000000
      MaxInteger = 36
      Min = -36.000000000000000000
      MinInteger = -36
      ShortLabel = 'output'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SKLOutputChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 50.000000000000000000
      DisplayName = 'Attack'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 1
      Min = 0.019999999552965160
      ShortLabel = 'Attack'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = SKLAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 20.000000000000000000
      DisplayName = 'Release'
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 5
      Max = 500.000000000000000000
      MaxInteger = 500
      Min = 25.000000000000000000
      MinInteger = 25
      ShortLabel = 'Release'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = SKLReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Ratio'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'ratio'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = SKLRatioChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Soft Knee'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      ShortLabel = 'knee'
      SmallStepFloat = 0.009999999776482582
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = SKLSKFBChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 645
  Top = 83
  Height = 150
  Width = 215
end
