object Audio2MidiTriggerModule: TAudio2MidiTriggerModule
  OldCreateOrder = False
  Version = '1.0'
  EffectName = 'Audio to Midi Trigger'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 1
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'A2MT'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -54.000000000000000000
      MinInteger = -54
      ReportVST2Properties = True
      ShortLabel = 'Thr.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gain'
      Flags = [ppfParameterIsSwitch, ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
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
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Interval'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 0.001000000047497451
      ReportVST2Properties = True
      ShortLabel = 'Interv.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterIntervalChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'MidiNote'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 127.000000000000000000
      MaxInteger = 127
      ReportVST2Properties = True
      ShortLabel = 'Note'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterMidiNoteChange
      OnCustomParameterDisplay = ParameterMidiNoteDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Velocity Shift'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      Max = 127.000000000000000000
      MaxInteger = 127
      Min = -127.000000000000000000
      MinInteger = -127
      ReportVST2Properties = True
      ShortLabel = 'VShift'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterVelocityShiftChange
    end>
  ParameterCategories = <>
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