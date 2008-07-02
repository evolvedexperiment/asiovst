object DegradeDataModule: TDegradeDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'mda Degrade'
  ProductName = 'Degrade'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'mdaC'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Headroom'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Headroo'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Quantize'
      Units = 'bits'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Quantiz'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Rate'
      Units = 'S<>S&&H'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Rate'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'PostFilter'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'PstFilt'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Non-Linear'
      Units = 'Odd<>Even'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Non-Lin'
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
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
