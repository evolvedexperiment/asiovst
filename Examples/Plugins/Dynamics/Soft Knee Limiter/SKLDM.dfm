object SoftKneeLimiterDataModule: TSoftKneeLimiterDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'Soft Knee Limiter'
  ProductName = 'Soft Knee Limiter'
  VendorName = 'ASIO-VST Delphi Project'
  PlugCategory = vpcEffect
  CanDos = []
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'SKLi'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Min = -96.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MinInteger = -96
      MaxInteger = 0
      LargeStepInteger = 1
      ShortLabel = 'thrshld'
      VSTModule = Owner
      OnParameterChange = SKLThresholdChange
    end
    item
      Min = 1.000000000000000000
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Ratio'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 10.000000000000000000
      MinInteger = 1
      ShortLabel = 'ratio'
      VSTModule = Owner
      OnParameterChange = SKLRatioChange
    end
    item
      Min = 0.009999999776482582
      Max = 1000.000000000000000000
      Curve = ctLogarithmic
      DisplayName = 'Attack'
      Units = 'ms'
      CurveFactor = 100000.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      SmallStepFloat = 0.100000001490116100
      Flags = [kVstParameterUsesFloatStep]
      MinInteger = 1
      MaxInteger = 1000
      LargeStepInteger = 0
      ShortLabel = 'Attack'
      VSTModule = Owner
      OnParameterChange = SKLAttackChange
    end
    item
      Min = 5.000000000000000000
      Max = 5000.000000000000000000
      Curve = ctLogarithmic
      DisplayName = 'Release'
      Units = 'ms'
      CurveFactor = 1000.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 5.000000000000000000
      MinInteger = 5
      MaxInteger = 5000
      LargeStepInteger = 5
      ShortLabel = 'Release'
      VSTModule = Owner
      OnParameterChange = SKLReleaseChange
    end
    item
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Soft Knee'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      SmallStepFloat = 0.009999999776482582
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 10
      LargeStepInteger = 1
      ShortLabel = 'knee'
      VSTModule = Owner
      OnParameterChange = SKLSoftKneeChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 452
  Top = 121
  Height = 150
  Width = 215
end
