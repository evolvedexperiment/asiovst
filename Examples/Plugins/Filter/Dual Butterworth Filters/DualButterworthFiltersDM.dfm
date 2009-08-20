object DualButterworthFiltersModule: TDualButterworthFiltersModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'DualButterworthFilters'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  TailSize = 131072
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'LiRi'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'Lowpass Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 2.000000000000000000
      MinInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'LP Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterLowpassFrequencyChange
      OnCustomParameterLabel = ParameterFrequencyLabel
      OnCustomParameterDisplay = ParameterFrequencyDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Lowpass Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'LP Ord.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB/Oct'
      VSTModule = Owner
      OnParameterChange = ParameterLowpassOrderChange
      OnCustomParameterDisplay = ParameterOrderDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Highpass Frequency'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'HP Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterHighpassFrequencyChange
      OnCustomParameterLabel = ParameterFrequencyLabel
      OnCustomParameterDisplay = ParameterFrequencyDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Highpass Order'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'HP Ord.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB/Oct'
      VSTModule = Owner
      OnParameterChange = ParameterHighpassOrderChange
      OnCustomParameterDisplay = ParameterOrderDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Type'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ReportVST2Properties = True
      ShortLabel = 'Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterTypeChange
      OnCustomParameterDisplay = ParameterTypeDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcessBandpass
  OnProcessReplacing = VSTModuleProcessBandpass
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 286
  Top = 77
  Height = 150
  Width = 215
end
