object DegradeDataModule: TDegradeDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'mda Degrade'
  ProductName = 'Degrade'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Degrade'
  UniqueID = 'mdaC'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Degrade'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Headroom'
      Max = 1.000000000000000000
      ShortLabel = 'Headroo'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Quantize'
      Max = 1.000000000000000000
      ShortLabel = 'Quantiz'
      SmoothingFactor = 1.000000000000000000
      Units = 'bits'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      Max = 1.000000000000000000
      ShortLabel = 'Rate'
      SmoothingFactor = 1.000000000000000000
      Units = 'S<>S&&H'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'PostFilter'
      Max = 1.000000000000000000
      ShortLabel = 'PstFilt'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterPostFilterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Non-Linear'
      Max = 1.000000000000000000
      ShortLabel = 'Non-Lin'
      SmoothingFactor = 1.000000000000000000
      Units = 'Odd<>Even'
      VSTModule = Owner
      OnParameterChange = ParameterNonLinearChange
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
      OnParameterChange = ParameterOutputChange
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
