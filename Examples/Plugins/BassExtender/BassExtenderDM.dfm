object BassExtenderModule: TBassExtenderModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Bass Extender'
  ProductName = 'Bass Extender'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'BASS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Crossover Only'
      VSTModule = Owner
    end
    item
      DisplayName = 'Try this'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mid only'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'Split Frequency'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      MaxInteger = 20000
      MinInteger = 2
      ShortLabel = 'Freq'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 10.000000000000000000
      StepInteger = 10
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
      OnCustomParameterLabel = ParamFreqLabel
      OnCustomParameterDisplay = ParamFreqDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Split Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 4.000000000000000000
      LargeStepInteger = 4
      MaxInteger = 32
      MinInteger = 1
      ShortLabel = 'Order'
      SmallStepFloat = 2.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      VSTModule = Owner
      OnParameterChange = ParamOrderChange
      OnCustomParameterDisplay = ParamSplitOrderDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Divider'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      ShortLabel = 'Divider'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamDividerChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Shape'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      ShortLabel = 'Shape'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamShapeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      MaxInteger = 0
      MinInteger = -60
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Ratio'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      MaxInteger = 1000
      MinInteger = 1
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamRatioChange
      OnCustomParameterDisplay = ParamRatioDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000000.000000000000000000
      DisplayName = 'Attack'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      MaxInteger = 1000000
      MinInteger = 1
      ShortLabel = 'Attack'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = ParamAttackChange
      OnCustomParameterLabel = ParamAttackLabel
      OnCustomParameterDisplay = ParamAttackDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'Release'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      MaxInteger = 10000
      MinInteger = 1
      ShortLabel = 'Release'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParamReleaseChange
      OnCustomParameterLabel = ParamReleaseLabel
      OnCustomParameterDisplay = ParamReleaseDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compression Mix'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      ShortLabel = 'CompMix'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamCompressionMixChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Balance'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      MinInteger = -100
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamBalanceChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mode'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      MaxInteger = 3
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamModeChange
      OnCustomParameterDisplay = ParamModeDisplay
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess32
  OnProcessDoubleReplacing = VSTModuleProcess64
  OnProcessReplacing = VSTModuleProcess32
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
