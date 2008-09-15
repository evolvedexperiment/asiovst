object LoudnessDataModule: TLoudnessDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
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
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Loudness'
      Max = 1.000000000000000000
      ShortLabel = 'Loudnes'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      Max = 1.000000000000000000
      ShortLabel = 'Output'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Link'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Link'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 11
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterLinkDisplay
    end>
  OnParameterChange = VSTModuleParameterChange
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
