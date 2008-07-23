object fReeverbVST: TfReeverbVST
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'fReeverb'
  ProductName = 'fReeverb'
  VendorName = 'fReeverb'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'fRee'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry'
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Dry'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDryChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Wet'
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Wet'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterWetChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Width'
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Width'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterWidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'RoomSize'
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      ShortLabel = 'RoomSiz'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterRoomSizeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'FreeZe'
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      ShortLabel = 'FreeZe'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterFreezeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stretch'
      LargeStepInteger = 0
      Max = 20.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Stretch'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterStretchChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Damp'
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Damp'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDampChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'NumAllPasses'
      LargeStepInteger = 0
      Max = 16.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ShortLabel = 'NumAllP'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterNumAllpassesChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'NumCombs'
      LargeStepInteger = 0
      Max = 16.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ShortLabel = 'NumComb'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterNumCombsChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcessReplacing
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
