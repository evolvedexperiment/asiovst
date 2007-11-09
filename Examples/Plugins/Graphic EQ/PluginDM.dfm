object PluginDataModule: TPluginDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Graphic EQ'
  ProductName = 'Graphic EQ'
  VendorName = 'Delphi VST Example'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'GrEQ'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 20 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L20Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 40 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L40Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 80 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L80Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 160 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L160Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 320 Hz '
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L320Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 640 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L640Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 1250 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L1.2kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 2500 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L2.5kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 5 kHz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L5kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 10 kHz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L10kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Left 20 kHz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'L20kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 20 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R20Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 40 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R40Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 80 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R80Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 160 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R160Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 320 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R320Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 640 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R640Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 1250 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R1k2Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 2500 Hz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R2k5Hz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 5 kHz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R5kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 10 kHz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R10kHz'
      VSTModule = Owner
    end
    item
      Min = -15.000000000000000000
      Max = 15.000000000000000000
      Curve = ctLinear
      DisplayName = 'Right 20 kHz'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      MinInteger = 15
      MaxInteger = 15
      LargeStepInteger = 3
      ShortLabel = 'R20kHz'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnParameterChange = VSTModuleParameterChange
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcessLR
  OnProcessReplacing = VSTModuleProcessLR
  Left = 838
  Top = 136
  Height = 150
  Width = 215
end
