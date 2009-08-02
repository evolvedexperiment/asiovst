object DitherNoiseshaperModule: TDitherNoiseshaperModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Dither & Noiseshaper Plugin'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'DiNo'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bit Depth'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Bit Dep'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Bit'
      VSTModule = Owner
      OnParameterChange = ParameterBitDepthChange
      OnCustomParameterDisplay = ParameterBitDepthDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Limit'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Limit'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterLimitChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dither Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 4.000000000000000000
      MaxInteger = 4
      ReportVST2Properties = True
      ShortLabel = 'Dither '
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterDitherTypeChange
      OnCustomParameterDisplay = ParameterDitherTypeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dither Amplitude'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 4.000000000000000000
      MaxInteger = 4
      ReportVST2Properties = True
      ShortLabel = 'Dither'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Steps'
      VSTModule = Owner
      OnParameterChange = ParameterDitherChangeAmplitude
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Noiseshaper Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 1
      Max = 12.000000000000000000
      MaxInteger = 12
      ReportVST2Properties = True
      ShortLabel = 'Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterNoiseshaperTypeChange
      OnCustomParameterDisplay = ParameterNoiseshaperTypeDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end