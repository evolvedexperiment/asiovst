object mdaLimiterDataModule: TmdaLimiterDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono]
  Version = '0.0'
  EffectName = 'Limiter'
  ProductName = 'mdaLimiter'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'mdaL'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -40.000000000000000000
      MinInteger = -40
      ShortLabel = 'Thres'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -40.000000000000000000
      MinInteger = -40
      ShortLabel = 'Trim'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = OutputTrimChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Att'
      SmoothingFactor = 1.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = AttackChange
      OnCustomParameterDisplay = AttackDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Rel'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ReleaseChange
      OnCustomParameterDisplay = ReleaseDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Knee'
      Max = 1.000000000000000000
      ShortLabel = 'Knee'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = KneeChange
      OnCustomParameterDisplay = KneeDisplay
    end>
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
