object PartyDelayDataModule: TPartyDelayDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Version = '1.0'
  EffectName = 'Party Delay'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'Pr-D'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'On/Off'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'On/Off'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOnOffChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Pan'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Pan'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterPanChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -54.000000000000000000
      MinInteger = -54
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Polarity'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Pol'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPolarityChange
      OnCustomParameterDisplay = ParameterPolarityDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 2000.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Delay'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 2000.000000000000000000
      MaxInteger = 2000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Delay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Feedback'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Fbk'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterFeedbackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Filter Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 8.000000000000000000
      MaxInteger = 8
      ReportVST2Properties = True
      ShortLabel = 'FiltTyp'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterChange
      OnCustomParameterDisplay = ParameterFilterDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Filter Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFilterFreqChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Filter Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Filter Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Frequency Shift'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'FreqSft'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyShiftChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 99999.992187500000000000
      Category = 'Band 1'
      DisplayName = 'Shift Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.001000000047497451
      ReportVST2Properties = True
      ShortLabel = 'SftFreq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterShiftFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Shift Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'SftOrd.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterShiftOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Drive'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Drive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDriveChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 1'
      DisplayName = 'Balance'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterBalanceChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'On/Off'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'On/Off'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOnOffChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Pan'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Pan'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterPanChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -54.000000000000000000
      MinInteger = -54
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Polarity'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Pol'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPolarityChange
      OnCustomParameterDisplay = ParameterPolarityDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 2000.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Delay'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 2000.000000000000000000
      MaxInteger = 2000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Delay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Feedback'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Fbk'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterFeedbackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Filter Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 8.000000000000000000
      MaxInteger = 8
      ReportVST2Properties = True
      ShortLabel = 'FiltTyp'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterChange
      OnCustomParameterDisplay = ParameterFilterDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Filter Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFilterFreqChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Filter Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Filter Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Frequency Shift'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'FreqSft'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyShiftChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 99999.992187500000000000
      Category = 'Band 2'
      DisplayName = 'Shift Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.001000000047497451
      ReportVST2Properties = True
      ShortLabel = 'SftFreq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterShiftFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Shift Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'SftOrd.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterShiftOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Drive'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Drive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDriveChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 2'
      DisplayName = 'Balance'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterBalanceChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'On/Off'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'On/Off'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOnOffChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Pan'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Pan'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterPanChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -54.000000000000000000
      MinInteger = -54
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Polarity'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Pol'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPolarityChange
      OnCustomParameterDisplay = ParameterPolarityDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 2000.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Delay'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 2000.000000000000000000
      MaxInteger = 2000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Delay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Feedback'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Fbk'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterFeedbackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Filter Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 8.000000000000000000
      MaxInteger = 8
      ReportVST2Properties = True
      ShortLabel = 'FiltTyp'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterChange
      OnCustomParameterDisplay = ParameterFilterDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Filter Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFilterFreqChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Filter Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Filter Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Frequency Shift'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'FreqSft'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyShiftChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 99999.992187500000000000
      Category = 'Band 3'
      DisplayName = 'Shift Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.001000000047497451
      ReportVST2Properties = True
      ShortLabel = 'SftFreq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterShiftFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Shift Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'SftOrd.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterShiftOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Drive'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Drive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDriveChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 3'
      DisplayName = 'Balance'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterBalanceChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'On/Off'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'On/Off'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOnOffChange
      OnCustomParameterDisplay = ParameterOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Pan'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Pan'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterPanChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -54.000000000000000000
      MinInteger = -54
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Polarity'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Pol'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterPolarityChange
      OnCustomParameterDisplay = ParameterPolarityDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 2000.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Delay'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 2000.000000000000000000
      MaxInteger = 2000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Delay'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Feedback'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Fbk'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterFeedbackChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Filter Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 8.000000000000000000
      MaxInteger = 8
      ReportVST2Properties = True
      ShortLabel = 'FiltTyp'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterChange
      OnCustomParameterDisplay = ParameterFilterDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Filter Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterFilterFreqChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Filter Gain'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ReportVST2Properties = True
      ShortLabel = 'Gain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterFilterGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Filter Bandwidth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 0.009999999776482582
      ReportVST2Properties = True
      ShortLabel = 'BW'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterBandwidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Frequency Shift'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'FreqSft'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFrequencyShiftChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 99999.992187500000000000
      Category = 'Band 4'
      DisplayName = 'Shift Frequency'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 0.001000000047497451
      ReportVST2Properties = True
      ShortLabel = 'SftFreq'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterShiftFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Shift Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'SftOrd.'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterShiftOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Drive'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Drive'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDriveChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Band 4'
      DisplayName = 'Balance'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ReportVST2Properties = True
      ShortLabel = 'Balance'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterBalanceChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Band 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Band 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Band 3'
      VSTModule = Owner
    end
    item
      DisplayName = 'Band 4'
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
