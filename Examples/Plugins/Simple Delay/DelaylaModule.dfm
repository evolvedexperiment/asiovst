object SimpleDelayVST: TSimpleDelayVST
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Simple Delay'
  ProductName = 'Simple Delay'
  VendorName = 'VST Wizard Example'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  IORatio = 1.000000000000000000
  UniqueID = 'dlay'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Delay Length'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 44100.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'Length'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'Samples'
      VSTModule = Owner
      OnParameterChange = SDDelayLengthChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
