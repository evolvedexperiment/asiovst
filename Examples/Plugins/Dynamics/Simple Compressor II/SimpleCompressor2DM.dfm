object SoftKneeCompressorDataModule: TSoftKneeCompressorDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor]
  Version = '1.0'
  EffectName = 'Simple Compressor II'
  ProductName = 'DAV Dynamics Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'SiGa'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 0.000099999997473788
      MaxInteger = 0
      Min = -96.000000000000000000
      MinInteger = -96
      ShortLabel = 'thrshld'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SLThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Ratio'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'ratio'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = SLRatioChange
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
      OnParameterChange = SLAttackChange
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
      OnParameterChange = SLReleaseChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 239
  Top = 105
  Height = 150
  Width = 215
end
