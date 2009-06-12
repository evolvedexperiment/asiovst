object Chebyshev2HPModule: TChebyshev2HPModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Chebyshev2 Lowpass Filter'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'CbcL'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 100.000000000000000000
      LargeStepInteger = 100
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq'
      SmallStepFloat = 10.000000000000000000
      StepFloat = 50.000000000000000000
      StepInteger = 50
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stopband'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = -0.009999999776482582
      MaxInteger = 0
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Stopbnd'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamStopbandChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Order'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      ReportVST2Properties = True
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOrderChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 778
  Top = 75
  Height = 150
  Width = 215
end
