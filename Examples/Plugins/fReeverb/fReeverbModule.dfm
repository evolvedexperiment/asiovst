object fReeverbVST: TfReeverbVST
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'fReeverb'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'fRee'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Small Room'
      VSTModule = Owner
    end
    item
      DisplayName = 'Large Hall'
      VSTModule = Owner
    end
    item
      DisplayName = 'Random 23'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Dry'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDryChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Wet'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Wet'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterWetChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Width'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Width'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterWidthChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'RoomSize'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      ShortLabel = 'RoomSiz'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterRoomSizeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'FreeZe'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      ShortLabel = 'FreeZe'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterFreezeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stretch'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 20.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Stretch'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterStretchChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Damp'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 100.000000000000000000
      MaxInteger = 0
      ShortLabel = 'Damp'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDampChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'NumAllPasses'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 16.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ShortLabel = 'NumAllP'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterNumAllpassesChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'NumCombs'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 16.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ShortLabel = 'NumComb'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      VSTModule = Owner
      OnParameterChange = ParameterNumCombsChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcessReplacing
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
