object TrackerDataModule: TTrackerDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Tracker'
  ProductName = 'Tracker'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Pitch Tracker'
  UniqueID = 'mdaJ'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Pitch Tracker'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mode'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 4.000000000000000000
      MaxInteger = 4
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterModeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dynamics'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Dynamic'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mix'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Mix'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Glide'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Transpose'
      Max = 1.000000000000000000
      ShortLabel = 'Transpo'
      SmoothingFactor = 1.000000000000000000
      Units = 'semi'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Maximum'
      Max = 1.000000000000000000
      ShortLabel = 'Maximum'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Trigger'
      Max = 1.000000000000000000
      ShortLabel = 'Trigger'
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
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
