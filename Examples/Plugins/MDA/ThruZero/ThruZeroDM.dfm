object ThruZeroDataModule: TThruZeroDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda ThruZero'
  ProductName = 'ThruZero'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Thru-Zero Flanger'
  UniqueID = 'mdaZ'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Thru-Zero Flanger'
      VSTModule = Owner
    end
    item
      DisplayName = 'Phase Canceller'
      VSTModule = Owner
    end
    item
      DisplayName = 'Chorus Doubler'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mad Modulator'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      Max = 1.000000000000000000
      ShortLabel = 'Rate'
      SmoothingFactor = 1.000000000000000000
      Units = 's'
      VSTModule = Owner
      OnParameterChange = ParameterRateChange
      OnCustomParameterDisplay = ParameterRateDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Depth'
      Max = 1.000000000000000000
      ShortLabel = 'Depth'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterDepthDisplay
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
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'DepthMod'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'DepthMd'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDepthModChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Feedback'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Feedbck'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end>
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
