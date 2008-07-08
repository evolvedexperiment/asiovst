object TransientDataModule: TTransientDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Transient'
  ProductName = 'Transient'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Transient Processor'
  UniqueID = 'mdaK'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Transient Processor'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Attack'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Attack'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Release'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Release'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
    end
    item
      Min = -20.000000000000000000
      Max = 20.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      MinInteger = -20
      MaxInteger = 20
      ShortLabel = 'Output'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Filter'
      Units = 'Lo <> Hi'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Filter'
      VSTModule = Owner
      OnParameterChange = ParameterFilterChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Attack Hold'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'AttHold'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChangeHold
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Release Hold'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'RelHold'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChangeHold
    end>
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
