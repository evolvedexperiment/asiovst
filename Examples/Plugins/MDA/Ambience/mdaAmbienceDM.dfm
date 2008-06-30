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
  UniqueID = 'mdaA'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Small Space Ambience'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Size'
      Units = 'm'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 10
      ShortLabel = 'Size'
      VSTModule = Owner
      OnParameterChange = ParamSizeChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'HF Damp'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      ShortLabel = 'HF'
      VSTModule = Owner
      OnParameterChange = ParamHFDampChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mix'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Mix'
      VSTModule = Owner
      OnParameterChange = ParamMixChange
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
      LargeStepFloat = 2.000000000000000000
      MinInteger = -20
      MaxInteger = 20
      LargeStepInteger = 3
      ShortLabel = 'Output'
      VSTModule = Owner
      OnParameterChange = ParamOutputChange
    end>
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
