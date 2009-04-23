object AdvancedClipperDataModule: TAdvancedClipperDataModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Advanced Clipper'
  ProductName = 'DAV effect examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
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
      Category = 'Input Section'
      DisplayName = 'Input Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -6.000000000000000000
      MinInteger = -6
      ShortLabel = 'InGain'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamInputGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Stage 1'
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
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParaOSFactor1Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Stage 1'
      DisplayName = 'Stage 1: Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 64.000000000000000000
      MaxInteger = 64
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamFilterOrder1Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Stage 1'
      DisplayName = 'Stage 1: Transition Bandwidth'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Bw'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamBW1Change
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Stage 2'
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
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamOSFactor2Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Stage 2'
      DisplayName = 'Stage 2: Filter Order'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 64.000000000000000000
      MaxInteger = 64
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamFilterOrder2Change
      OnCustomParameterDisplay = ParamRoundDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Stage 2'
      DisplayName = 'Stage 2: Transition Bandwidth'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ShortLabel = 'Bw'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamBW2Change
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Output Section'
      DisplayName = 'Output Gain'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -6.000000000000000000
      MinInteger = -6
      ShortLabel = 'OutGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamOutputGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Output Section'
      DisplayName = 'Hard Clip'
      Flags = [kVstParameterIsSwitch, kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Hrd Clp'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamHardClipChange
      OnCustomParameterDisplay = ParamHardClipDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Input Section'
      VSTModule = Owner
    end
    item
      DisplayName = 'Stage 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Stage 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Output Section'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
