object LA4029DataModule: TLA4029DataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'LA-4029 Leveling Amplifier'
  ProductName = 'DAV Dynamics Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = '4029'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'On/Off'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'On/Off'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOnOffChange
      OnCustomParameterDisplay = ParamOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Input'
      Flags = [kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'input'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SKLInputChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Output'
      Flags = [kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = SKLOutputChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 50.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Attack'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = 0.200000002980232200
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 0.100000001490116100
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = SKLAttackChange
      OnCustomParameterLabel = ParamAttackLabel
      OnCustomParameterDisplay = ParamAttackDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 20.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Release'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 5.000000000000000000
      LargeStepInteger = 5
      Max = 500.000000000000000000
      MaxInteger = 500
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = SKLReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Ratio'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'ratio'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = SKLRatioChange
      OnCustomParameterDisplay = ParamRatioDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Soft Knee'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'knee'
      SmallStepFloat = 0.009999999776482582
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = SKLSKFBChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'I/O'
      DisplayName = 'Mix'
      Flags = [kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamMixChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'VU-Meter'
      DisplayName = 'VUMeterDisplay'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'VUMeter'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamVUMeterChange
      OnCustomParameterDisplay = ParamVUMeterDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'VU-Meter'
      DisplayName = 'VUMeterSpeed'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'FallOff'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParamVUSpeedChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 20.000000000000000000
      Category = 'Sidechain Filter'
      DisplayName = 'Highpass Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'HP Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamHPFreqChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Sidechain Filter'
      DisplayName = 'Highpass Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 4.000000000000000000
      MaxInteger = 4
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'HP Ord.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamHPOrderChange
      OnCustomParameterDisplay = ParamHPOrderDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'I/O'
      VSTModule = Owner
    end
    item
      DisplayName = 'Time Constants'
      VSTModule = Owner
    end
    item
      DisplayName = 'Characteristic'
      VSTModule = Owner
    end
    item
      DisplayName = 'VU-Meter'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sidechain Filter'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  OnSoftBypass = VSTModuleSoftBypass
  Left = 739
  Top = 81
  Height = 150
  Width = 215
end
