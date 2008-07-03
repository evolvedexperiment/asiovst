object LoudnessDataModule: TLoudnessDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'mda Loudness'
  ProductName = 'Loudness'
  VendorName = '1.0'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Equal Loudness Contours'
  UniqueID = 'mdal'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Equal Loudness Contours'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Loudness'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Loudnes'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Output'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Link'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 1
      StepInteger = 11
      LargeStepInteger = 1
      ShortLabel = 'Link'
      VSTModule = Owner
      OnCustomParameterDisplay = LoudnessDataModuleParameterProperties2CustomParameterDisplay
    end>
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
