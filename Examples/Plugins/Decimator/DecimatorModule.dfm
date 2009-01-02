object VSTDecimator: TVSTDecimator
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Decimator'
  ProductName = 'Decimator'
  VendorName = 'Tobybear & Christian'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Preset 1'
  IORatio = 1.000000000000000000
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
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Samplerate'
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 44100.000000000000000000
      MaxInteger = 44100
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'rate'
      SmallStepFloat = 100.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterSampleRateChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bits'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'bits'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterBitsChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Cutoff Frequency'
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'cut'
      SmallStepFloat = 100.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterCutoffChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Resonance'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 8.000000000000000000
      MaxInteger = 8
      Min = 0.100000001490116100
      ShortLabel = 'res'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterResonanceChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'FilterType'
      Flags = [kVstParameterIsSwitch]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'FilterT'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterFilterTypeChange
      OnCustomParameterDisplay = ParameterFilterTypeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Wet/Dry Mix'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'mix'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '&'
      VSTModule = Owner
      OnParameterChange = ParameterWetDryMixChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output Volume'
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -24.000000000000000000
      MinInteger = -24
      ShortLabel = 'vol'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputVolumeChange
    end>
  OnOpen = VSTModuleOpen
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessMidi = VSTModuleProcessMidi
  OnProcessReplacing = VSTModuleProcess
  Left = 243
  Top = 103
  Height = 152
  Width = 219
end
