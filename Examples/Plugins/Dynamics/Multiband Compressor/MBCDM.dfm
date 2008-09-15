object MBCDataModule: TMBCDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor]
  Version = '0.0'
  EffectName = 'Multiband Compressor'
  ProductName = 'Multiband Compressor'
  VendorName = 'Delphi ASIO & VST'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'MBCo'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Low Gain'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 2
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ShortLabel = 'LowGain'
      SmallStepFloat = 0.009999999776482582
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMLowGainChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'Low Frequency'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'LowFreq'
      SmallStepFloat = 10.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = MBCDMLowFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Low Order'
      Flags = [kVstParameterUsesIntegerMinMax]
      LargeStepFloat = 4.000000000000000000
      Max = 10.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'LowOrd'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      VSTModule = Owner
      OnParameterChange = MBCDCLowOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Low Threshold'
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -48.000000000000000000
      MinInteger = -48
      ShortLabel = 'LowThrs'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMLowThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      DisplayName = 'Low Ratio'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'LoRatio'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = MBCDMLowRatioChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Low Attack'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'LowAtt'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMLowAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Low Release'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'LowRel'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMLowReleaseChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mid Gain'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 2
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ShortLabel = 'MidGain'
      SmallStepFloat = 0.009999999776482582
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMMidGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mid Threshold'
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -48.000000000000000000
      MinInteger = -48
      ShortLabel = 'MidThrs'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMMidThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      DisplayName = 'Mid Ratio'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'MidRtio'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = MBCDMMidRatioChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Mid Attack'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'MidAtt'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMMidAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Mid Release'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'MidRel'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMMidReleaseChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'High Frequency'
      Flags = [kVstParameterUsesFloatStep]
      LargeStepFloat = 1000.000000000000000000
      LargeStepInteger = 1000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ShortLabel = 'HighFrq'
      SmallStepFloat = 10.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 100.000000000000000000
      StepInteger = 100
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = MBCDMHighFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'High Order'
      Flags = [kVstParameterUsesIntegerMinMax]
      LargeStepFloat = 4.000000000000000000
      LargeStepInteger = 4
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'HighOrd'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 2.000000000000000000
      StepInteger = 2
      VSTModule = Owner
      OnParameterChange = MBCDCHighOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'High Gain'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 2
      Max = 15.000000000000000000
      MaxInteger = 15
      Min = -15.000000000000000000
      MinInteger = -15
      ShortLabel = 'HighGai'
      SmallStepFloat = 0.009999999776482582
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMHighGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'High Threshold'
      LargeStepFloat = 3.000000000000000000
      LargeStepInteger = 3
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -48.000000000000000000
      MinInteger = -48
      ShortLabel = 'HighTrh'
      SmallStepFloat = 0.100000001490116100
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = MBCDMHighThresholdChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 10.000000000000000000
      DisplayName = 'High Ratio'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'HiRatio'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = MBCDMHighRatioChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'High Attack'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'HighAtt'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMHighAttackChange
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'High Release'
      LargeStepFloat = 10.000000000000000000
      Max = 1000.000000000000000000
      MaxInteger = 1000
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'HighRel'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = MBCDMHighReleaseChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
