object LimiterDataModule: TLimiterDataModule
  OldCreateOrder = False
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
      Min = -40.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MinInteger = -40
      MaxInteger = 0
      ShortLabel = 'Thres'
      VSTModule = Owner
      OnParameterChange = ThresholdChange
    end
    item
      Min = -40.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MinInteger = -40
      MaxInteger = 0
      ShortLabel = 'Trim'
      VSTModule = Owner
      OnParameterChange = OutputTrimChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Attack'
      Units = #181's'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Att'
      VSTModule = Owner
      OnParameterChange = AttackChange
      OnCustomParameterDisplay = AttackDisplay
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Release'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Rel'
      VSTModule = Owner
      OnParameterChange = ReleaseChange
      OnCustomParameterDisplay = ReleaseDisplay
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Knee'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Knee'
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
