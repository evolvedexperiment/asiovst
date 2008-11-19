object mdaAmbienceDataModule: TmdaAmbienceDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'mda Ambience'
  ProductName = 'Ambience'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Small Space Ambience'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaA'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Small Space Ambience'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Size'
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ShortLabel = 'Size'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'm'
      VSTModule = Owner
      OnParameterChange = ParamSizeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'HF Damp'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'HF'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamHFDampChange
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
      OnParameterChange = ParamMixChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 3
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'Output'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamOutputChange
    end>
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSuspend = VSTModuleSuspend
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
