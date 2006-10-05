object VSTDecimator: TVSTDecimator
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Decimator'
  ProductName = 'Decimator'
  VendorName = 'Tobybear & Christian'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = cgEffect
  TailSize = 0
  CanDos = [plugAsChannelInsert, plugAsSend, _2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Preset 1'
  KeysRequired = False
  UniqueID = 'TBDM'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 3'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = 20.000000000000000000
      Max = 44100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Samplerate'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 100.000000000000000000
      SmallStepFloat = 100.000000000000000000
      LargeStepFloat = 1000.000000000000000000
      Flags = []
      MinInteger = 20
      MaxInteger = 44100
      StepInteger = 100
      LargeStepInteger = 1000
      ShortLabel = 'rate'
      VSTModule = Owner
    end
    item
      Min = 1.000000000000000000
      Max = 24.000000000000000000
      Curve = ctLinear
      DisplayName = 'Bits'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      Flags = [ppfUsesIntegerMinMax, ppfUsesIntStep]
      MinInteger = 1
      MaxInteger = 24
      StepInteger = 1
      LargeStepInteger = 2
      ShortLabel = 'bits'
      VSTModule = Owner
    end
    item
      Min = 20.000000000000000000
      Max = 20000.000000000000000000
      Curve = ctLinear
      DisplayName = 'Cutoff Frequency'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 100.000000000000000000
      SmallStepFloat = 100.000000000000000000
      LargeStepFloat = 1000.000000000000000000
      Flags = []
      MinInteger = 20
      MaxInteger = 20000
      StepInteger = 100
      LargeStepInteger = 1000
      ShortLabel = 'cut'
      VSTModule = Owner
    end
    item
      Min = 0.100000001490116100
      Max = 8.000000000000000000
      Curve = ctLinear
      DisplayName = 'Resonance'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 8
      StepInteger = 0
      LargeStepInteger = 0
      ShortLabel = 'res'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'FilterType'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Wet/Dry Mix'
      Units = '&'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 10.000000000000000000
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 1
      LargeStepInteger = 10
      ShortLabel = 'mix'
      VSTModule = Owner
    end
    item
      Min = -24.000000000000000000
      Max = 6.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output Volume'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 3.000000000000000000
      Flags = []
      MinInteger = -24
      MaxInteger = 6
      StepInteger = 1
      LargeStepInteger = 3
      ShortLabel = 'vol'
      VSTModule = Owner
    end>
  OnEditOpen = VSTModuleEditOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessMidi = VSTModuleProcessMidi
  OnInitialize = VSTModuleInitialize
  Left = 243
  Top = 103
  Height = 152
  Width = 219
end
