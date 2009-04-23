object MBCDataModule: TMBCDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Multiband Compressor'
  ProductName = 'DAV Dynamics Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'MBCo'
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
      Category = 'Low'
      DisplayName = 'Low Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 2
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'LowGain'
      SmallStepFloat = 0.009999999776482582
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMLowGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'LowFreq'
      SmallStepFloat = 10.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = MBCDMLowFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 4.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'LowOrd'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      VSTModule = Owner
      OnParameterChange = MBCDCLowOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Threshold'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -48.000000000000000000
      MinInteger = -48
      ReportVST2Properties = True
      ShortLabel = 'LowThrs'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMLowThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Ratio'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'LoRatio'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = MBCDMLowRatioChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Attack'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'LowAtt'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMLowAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'Low'
      DisplayName = 'Low Release'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'LowRel'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMLowReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 2
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'MidGain'
      SmallStepFloat = 0.009999999776482582
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMMidGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Threshold'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -48.000000000000000000
      MinInteger = -48
      ReportVST2Properties = True
      ShortLabel = 'MidThrs'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMMidThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Ratio'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'MidRtio'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = MBCDMMidRatioChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Attack'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'MidAtt'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMMidAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'Mid'
      DisplayName = 'Mid Release'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'MidRel'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMMidReleaseChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'High'
      DisplayName = 'High Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'HighFrq'
      SmallStepFloat = 10.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = MBCDMHighFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'High'
      DisplayName = 'High Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 4.000000000000000000
      LargeStepInteger = 4
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'HighOrd'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      VSTModule = Owner
      OnParameterChange = MBCDCHighOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'High'
      DisplayName = 'High Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 2
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'HighGai'
      SmallStepFloat = 0.009999999776482582
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMHighGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'High'
      DisplayName = 'High Threshold'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -48.000000000000000000
      MinInteger = -48
      ReportVST2Properties = True
      ShortLabel = 'HighTrh'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMHighThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      Category = 'High'
      DisplayName = 'High Ratio'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'HiRatio'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = MBCDMHighRatioChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'High'
      DisplayName = 'High Attack'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'HighAtt'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMHighAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      Category = 'High'
      DisplayName = 'High Release'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'HighRel'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMHighReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Limiter'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Limiter'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterLimiterChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Low'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mid'
      VSTModule = Owner
    end
    item
      DisplayName = 'High'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
