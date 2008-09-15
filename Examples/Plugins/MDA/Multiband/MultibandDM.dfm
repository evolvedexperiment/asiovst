object MultibandDataModule: TMultibandDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'mda Multiband'
  ProductName = 'Multiband'
  VendorName = '1.0'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Multi-Band Compressor'
  UniqueID = 'mdaM'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Multi-Band Compressor'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Listen'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ShortLabel = 'Listen'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterOutputDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'L <> M'
      Max = 1.000000000000000000
      ShortLabel = 'L <> M'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'M <> H'
      Max = 1.000000000000000000
      ShortLabel = 'M <> H'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'L Comp'
      Max = 1.000000000000000000
      ShortLabel = 'L Comp'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'M Comp'
      Max = 1.000000000000000000
      ShortLabel = 'M Comp'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'H Comp'
      Max = 1.000000000000000000
      ShortLabel = 'H Comp'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'L Out'
      Max = 1.000000000000000000
      ShortLabel = 'L Out'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterGainDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'M Out'
      Max = 1.000000000000000000
      ShortLabel = 'M Out'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterGainDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'H Out'
      Max = 1.000000000000000000
      ShortLabel = 'H Out'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterGainDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Max = 1.000000000000000000
      ShortLabel = 'Attack'
      SmoothingFactor = 1.000000000000000000
      Units = #181's'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stereo'
      Max = 1.000000000000000000
      ShortLabel = 'Stereo'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Process'
      Max = 1.000000000000000000
      ShortLabel = 'Process'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
