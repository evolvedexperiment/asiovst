object SimpleLimiterDataModule: TSimpleLimiterDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor]
  Version = '0.0'
  EffectName = 'Simple Limiter'
  ProductName = 'Simple Limiter'
  VendorName = 'ASIO-VST Delphi Project'
  PlugCategory = vpcEffect
  CanDos = []
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'SiGa'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Min = -96.000000000000000000
      Max = 0.000099999997473788
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
      OnParameterChange = SLThresholdChange
    end
    item
      Min = 1.000000000000000000
      Max = 100.000000000000000000
      Curve = ctLogarithmic
      DisplayName = 'Ratio'
      CurveFactor = 100.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 10.000000000000000000
      MinInteger = 1
      ShortLabel = 'ratio'
      VSTModule = Owner
      OnParameterChange = SLRatioChange
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
      OnParameterChange = SLAttackChange
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
      OnParameterChange = SLReleaseChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 239
  Top = 105
  Height = 150
  Width = 215
end
