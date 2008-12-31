object VocInputDataModule: TVocInputDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda VocInput'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Vocoder Carrier Signal'
  IORatio = 1.000000000000000000
  UniqueID = 'mda3'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Vocoder Carrier Signal'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tracking'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ShortLabel = 'Trackin'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterTrackingDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Pitch'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Pitch'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Breath'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Breath'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'S Thresh'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'SThresh'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Max Freq'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 93.000000000000000000
      MaxInteger = 93
      Min = 45.000000000000000000
      MinInteger = 45
      ShortLabel = 'MaxFreq'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterMaxFrequencyDisplay
    end>
  OnOpen = VSTModuleOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
