object TrackPlugModule: TTrackPlugModule
  OldCreateOrder = False
  Version = '1.0'
  EffectName = 'TrackPlug'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'TrPl'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'DC Filter Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 200.000000000000000000
      MaxInteger = 200
      Min = 0.019999999552965160
      ReportVST2Properties = True
      ShortLabel = 'DC Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterDCFilterChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'DC Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'DC Ord.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterDcFilterChangeOrder
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'EQ Filter 1 Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ1Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 1 Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ1Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'EQ Filter 1 Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ1 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 1 Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ1Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'EQ Filter 2 Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ2Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 2 Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ2Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'EQ Filter 2 Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ2 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 2 Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ2Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'EQ Filter 3 Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ3Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 3 Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ3Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'EQ Filter 3 Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ3 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 3 Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ3Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'EQ Filter 4 Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ4Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 4 Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ4Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'EQ Filter 4 Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ4 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 4 Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ4Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'EQ Filter 5 Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'EQ5Freq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 5 Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'EQ5Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterEqFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'EQ Filter 5 Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.100000001490116100
      ShortLabel = 'EQ5 BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Oct'
      VSTModule = Owner
      OnParameterChange = ParameterFilterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'EQ Filter 5 Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 7.000000000000000000
      MaxInteger = 7
      ReportVST2Properties = True
      ShortLabel = 'EQ5Type'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterEqTypeChange
      OnCustomParameterDisplay = ParameterEqTypeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Attack'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Release'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 0.050000000745058060
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Threshold'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
      OnCustomParameterDisplay = ParameterThresholdDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Ratio'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 1
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterRatioChange
      OnCustomParameterDisplay = ParameterRatioDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Knee'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterKneeChange
      OnCustomParameterDisplay = ParameterKneeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      DisplayName = 'Compressor Attack'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'Attack'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorAttackChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      DisplayName = 'Compressor Release'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 5000
      Min = 0.050000000745058060
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compressor Threshold'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = -90.000000000000000000
      MinInteger = -90
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorThresholdChange
      OnCustomParameterDisplay = ParameterThresholdDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10000.000000000000000000
      DisplayName = 'Compressor Ratio'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.009999999776482582
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCompressorRatioChange
      OnCustomParameterDisplay = ParameterRatioDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compressor Knee'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      ReportVST2Properties = True
      ShortLabel = 'Knee'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorKneeChange
      OnCustomParameterDisplay = ParameterKneeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compressor MakeUp Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 60.000000000000000000
      MaxInteger = 60
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorMakeUpGainChange
      OnCustomParameterDisplay = ParameterMakeUpGainDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compressor Auto Make Up Gain'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Auto Ma'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterCompressorAutoMakeUpGainChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Compressor Mix'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterCompressorMixChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Time Constants'
      VSTModule = Owner
    end
    item
      DisplayName = 'Characteristic'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 286
  Top = 77
  Height = 150
  Width = 215
end
