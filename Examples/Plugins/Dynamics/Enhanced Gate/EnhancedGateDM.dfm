object EnhancedGateDataModule: TEnhancedGateDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Simple Gate'
  ProductName = 'Enhanced Audio Gate'
  VendorName = 'ASIO-VST Delphi Project'
  PlugCategory = vpcEffect
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out, vcdBypass]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'EAGa'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Power'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Power'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGPowerChange
      OnCustomParameterDisplay = EAGOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ShortLabel = 'Thrshld'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = EAGThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100000.000000000000000000
      DisplayName = 'Attack'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 300
      Min = 0.009999999776482582
      MinInteger = -200
      ShortLabel = 'Attack'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = EAGAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 250.000000000000000000
      DisplayName = 'Hold'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 2.500000000000000000
      MaxInteger = 24
      Min = 0.009999999776482582
      MinInteger = -200
      ShortLabel = 'Hold'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = EAGHoldChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Decay'
      LargeStepFloat = 100.000000000000000000
      Max = 5000.000000000000000000
      MaxInteger = 370
      Min = 5.000000000000000000
      MinInteger = 70
      ShortLabel = 'Decay'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 10.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = EAGDecayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Duck'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Duck'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGDuckChange
      OnCustomParameterDisplay = EAGOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stereo Link'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Link'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGStereoLinkChange
      OnCustomParameterDisplay = EAGOnOffDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Side Chain Source'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'SCSrc'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = EAGSideChainSourceChange
      OnCustomParameterDisplay = EAGSideChainSourceDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 200.000000000000000000
      DisplayName = 'Lo Cut'
      LargeStepFloat = 20.000000000000000000
      LargeStepInteger = 20
      Max = 4000.000000000000000000
      MaxInteger = 4000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'LoCut'
      SmallStepFloat = 20.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = EAGLoCutChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Hi Cut'
      LargeStepFloat = 20.000000000000000000
      LargeStepInteger = 20
      Max = 20.000000000000000000
      MaxInteger = 20000
      Min = 0.200000002980232200
      MinInteger = 200
      ShortLabel = 'HiCut'
      SmallStepFloat = 20.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 20.000000000000000000
      StepInteger = 20
      Units = 'kHz'
      VSTModule = Owner
      OnParameterChange = EAGHiCutChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Ratio'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Ratio'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.500000000000000000
      VSTModule = Owner
      OnParameterChange = EAGRatioChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Knee'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Knee'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.200000002980232200
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = EAGKneeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Range'
      LargeStepFloat = 10.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ShortLabel = 'Range'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = EAGRangeChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcessBypass
  OnProcessReplacing = VSTModuleProcessBypass
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 552
  Top = 84
  Height = 150
  Width = 215
end
