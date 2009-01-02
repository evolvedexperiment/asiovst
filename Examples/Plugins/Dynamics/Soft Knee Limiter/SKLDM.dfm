object SoftKneeLimiterDataModule: TSoftKneeLimiterDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Soft Knee Limiter'
  ProductName = 'DAV Dynamics Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'SKLi'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -96.000000000000000000
      MinInteger = -96
      ShortLabel = 'Thrshld'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SKLThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Knee'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.050000000745058060
      ShortLabel = 'Knee'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SKLSoftKneeChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      DisplayName = 'Attack'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      MinInteger = 1
      ShortLabel = 'Attack'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = SKLAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Release'
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 5
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 5.000000000000000000
      MinInteger = 5
      ShortLabel = 'Release'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = SKLReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Makeup  Gain'
      LargeStepFloat = 2.000000000000000000
      Max = 40.000000000000000000
      MaxInteger = 40
      ShortLabel = 'Makeup'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = SKLMakeUpGainChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 618
  Top = 117
  Height = 150
  Width = 215
end
