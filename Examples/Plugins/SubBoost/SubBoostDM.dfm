object SubBoostDataModule: TSubBoostDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'SubBoost'
  ProductName = 'SubBoost'
  VendorName = 'Delphi ASIO & VST Packages'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Sub Bass Synthesizer'
  IORatio = 1.000000000000000000
  UniqueID = 'mdaB'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Sub Bass Synthesizer'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Type'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 3.000000000000000000
      MaxInteger = 3
      ShortLabel = 'Type'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterModeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Level'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Level'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterLevelChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tune'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 400.000000000000000000
      MaxInteger = 400
      Min = 10.000000000000000000
      MinInteger = 10
      ShortLabel = 'Tune'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParameterTuneChange
      OnCustomParameterDisplay = ParameterTuneDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry Mix'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Dry Mix'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDryChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Thresh'
      LargeStepFloat = 2.000000000000000000
      Max = -0.000000000100000001
      MaxInteger = 0
      Min = -60.000000000000000000
      MinInteger = -60
      ShortLabel = 'Thresh'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterDisplay = ParameterReleaseDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Order'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 64.000000000000000000
      MaxInteger = 64
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOrderChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 223
  Top = 79
  Height = 150
  Width = 215
end
