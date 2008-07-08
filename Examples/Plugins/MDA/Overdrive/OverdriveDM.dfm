object OverdriveDataModule: TOverdriveDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'mda Overdrive'
  ProductName = 'Overdrive'
  VendorName = '1.0'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Soft Overdrive'
  UniqueID = 'mdaO'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Soft Overdrive'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Drive'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Drive'
      VSTModule = Owner
      OnParameterChange = ParameterDriveChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Muffle'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Muffle'
      VSTModule = Owner
      OnParameterChange = ParameterMuffleChange
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
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      MinInteger = -20
      MaxInteger = 20
      LargeStepInteger = 2
      ShortLabel = 'Output'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
    end>
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
