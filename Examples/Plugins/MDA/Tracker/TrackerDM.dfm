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
      Max = 4.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mode'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 4
      LargeStepInteger = 1
      ShortLabel = 'Mode'
      VSTModule = Owner
      OnCustomParameterDisplay = TrackerDataModuleParameterProperties0CustomParameterDisplay
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Dynamics'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Dynamic'
      VSTModule = Owner
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mix'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Mix'
      VSTModule = Owner
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Glide'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Glide'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Transpose'
      Units = 'semi'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Transpo'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Maximum'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Maximum'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Trigger'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Trigger'
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
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
