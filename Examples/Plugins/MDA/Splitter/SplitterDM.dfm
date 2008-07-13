object SplitterDataModule: TSplitterDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Splitter'
  ProductName = 'Splitter'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Frequency/Level Splitter'
  UniqueID = 'mda7'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Frequency/Level Splitter'
      VSTModule = Owner
    end
    item
      DisplayName = 'Pass Peaks Only'
      VSTModule = Owner
    end
    item
      DisplayName = 'Stereo Crossover'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mode'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
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
      DisplayName = 'Frequency'
      Max = 1.000000000000000000
      ShortLabel = 'Freq'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterFrequencyDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Frequency Mode'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ShortLabel = 'Freq'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterFreqLevelModeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Level'
      LargeStepFloat = 2.000000000000000000
      MaxInteger = 0
      Min = -40.000000000000000000
      MinInteger = -40
      ShortLabel = 'Level'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Level Mode'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ShortLabel = 'Level M'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterFreqLevelModeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Envelope'
      Max = 1.000000000000000000
      ShortLabel = 'Env'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterEnvelopeChange
      OnCustomParameterDisplay = ParameterEnvelopeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end>
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
