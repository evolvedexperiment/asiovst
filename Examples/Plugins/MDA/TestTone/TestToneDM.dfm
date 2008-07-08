object TestToneDataModule: TTestToneDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda TestTone'
  ProductName = 'TestTone'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Signal Generator'
  UniqueID = 'mdaT'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Signal Generator'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 8.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mode'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 8
      LargeStepInteger = 1
      ShortLabel = 'Mode'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterModeDisplay
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Level'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Level'
      VSTModule = Owner
    end
    item
      Max = 2.000000000000000000
      Curve = ctLinear
      DisplayName = 'Channel'
      Units = 'L <> R'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 2
      LargeStepInteger = 1
      ShortLabel = 'Channel'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterChannelDisplay
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'F1'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'F1'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'F2'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'F2'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Thru'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Thru'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Sweep'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Sweep'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = '0dB ='
      Units = 'dBFS'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = '0dB ='
      VSTModule = Owner
    end>
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
