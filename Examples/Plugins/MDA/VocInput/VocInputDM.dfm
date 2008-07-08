object VocInputDataModule: TVocInputDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda VocInput'
  ProductName = 'VocInput'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Vocoder Carrier Signal'
  UniqueID = 'mda3'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Vocoder Carrier Signal'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 2.000000000000000000
      Curve = ctLinear
      DisplayName = 'Tracking'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 2
      LargeStepInteger = 1
      ShortLabel = 'Trackin'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterTrackingDisplay
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Pitch'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Pitch'
      VSTModule = Owner
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Breath'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Breath'
      VSTModule = Owner
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'S Thresh'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'SThresh'
      VSTModule = Owner
    end
    item
      Min = 45.000000000000000000
      Max = 93.000000000000000000
      Curve = ctLinear
      DisplayName = 'Max Freq'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MinInteger = 45
      MaxInteger = 93
      LargeStepInteger = 1
      ShortLabel = 'MaxFreq'
      VSTModule = Owner
      OnCustomParameterDisplay = VocInputDataModuleParameterProperties4CustomParameterDisplay
    end>
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
