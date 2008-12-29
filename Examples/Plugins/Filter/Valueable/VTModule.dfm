object VTVSTModule: TVTVSTModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Valve Tone '#39'63'
  ProductName = 'DAV Filter Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'VT63'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bass Gain'
      LargeStepFloat = 2.000000000000000000
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'LowGain'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamLowGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Treble Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'HiGain'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamHiGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bass Bypass'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'LowByps'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamLowBypassChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Treble Bypass'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'HiByps'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamHiBypassChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Drive'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 4.000000000000000000
      MaxInteger = 4
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Drive'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamDriveChange
      OnCustomParameterDisplay = ParamDriveDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Channel'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Channel'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamChannelChange
      OnCustomParameterDisplay = ParamChannelDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 12.000000000000000000
      MaxInteger = 12
      Min = -12.000000000000000000
      MinInteger = -12
      ShortLabel = 'OutGain'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamOutGainChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTEditOpen
  OnProcess = VSTModuleProcessStereo
  OnProcessReplacing = VSTModuleProcessStereo
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
