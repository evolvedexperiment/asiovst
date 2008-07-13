object SubSynthDataModule: TSubSynthDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda SubSynth'
  ProductName = 'SubSynth'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Sub Bass Synthesizer'
  UniqueID = 'mdaB'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Sub Bass Synthesizer'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Type'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ShortLabel = 'Type'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterModeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Level'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Level'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterLevelChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tune'
      Max = 1.000000000000000000
      ShortLabel = 'Tune'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry Mix'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Dry Mix'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDryChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Thresh'
      LargeStepFloat = 2.000000000000000000
      MaxInteger = 0
      Min = -60.000000000000000000
      MinInteger = -60
      ShortLabel = 'Thresh'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
    end>
  OnParameterChange = VSTModuleParameterChange
  OnResume = VSTModuleResume
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
