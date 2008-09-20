object AdvancedClipperDataModule: TAdvancedClipperDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Advanced Clipper'
  ProductName = 'AdvancedClipper'
  VendorName = 'Delphi ASIO & VST Packages'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'AdCr'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Bypass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Light'
      VSTModule = Owner
    end
    item
      DisplayName = 'Normal'
      VSTModule = Owner
    end
    item
      DisplayName = 'More'
      VSTModule = Owner
    end
    item
      DisplayName = 'Even More'
      VSTModule = Owner
    end
    item
      DisplayName = 'True Bypass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Clip Art!'
      VSTModule = Owner
    end
    item
      DisplayName = 'Rippler'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Input Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -6.000000000000000000
      MinInteger = -6
      ShortLabel = 'InGain'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamInputGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stage 1: Oversampling Factor'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Factor'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParaOSFactor1Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stage 1: Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 64.000000000000000000
      MaxInteger = 64
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamFilterOrder1Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stage 1: Transition Bandwidth'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Bw'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamBW1Change
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stage 2: Oversampling Factor'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Factor'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOSFactor2Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stage 2: Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 64.000000000000000000
      MaxInteger = 64
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamFilterOrder2Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stage 2: Transition Bandwidth'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Bw'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamBW2Change
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -6.000000000000000000
      MinInteger = -6
      ShortLabel = 'OutGain'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamOutputGainChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
