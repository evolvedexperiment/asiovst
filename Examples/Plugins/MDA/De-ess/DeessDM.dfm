object DeessDataModule: TDeessDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'mda De-ess'
  ProductName = 'De-ess'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'De-esser'
  UniqueID = 'mdaS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'De-esser'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = -60.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      MinInteger = -60
      MaxInteger = 0
      LargeStepInteger = 2
      ShortLabel = 'Thres'
      VSTModule = Owner
      OnParameterChange = ParamEnvelopeChange
    end
    item
      Min = 1000.000000000000000000
      Max = 12000.000000000000000000
      Curve = ctFrequencyScale
      DisplayName = 'Frequency'
      Units = 'Hz'
      CurveFactor = 12.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 10.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 100.000000000000000000
      MinInteger = 1000
      MaxInteger = 12000
      LargeStepInteger = 100
      ShortLabel = 'Freq'
      VSTModule = Owner
      OnParameterChange = ParamFilterChange
    end
    item
      Min = -20.000000000000000000
      Max = 20.000000000000000000
      Curve = ctLinear
      DisplayName = 'HF Drive'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      MinInteger = -20
      MaxInteger = 20
      LargeStepInteger = 2
      ShortLabel = 'HFDrive'
      VSTModule = Owner
      OnParameterChange = ParamHFDriveChange
    end>
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
