object StkReverbModule: TStkReverbModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Stk Reverb'
  ProductName = 'DAV Stk Examples'
  VendorName = 'Delphi ASIO & VST Projects'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default Network Reverb'
  IORatio = 1.000000000000000000
  UniqueID = 'STKR'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default Network Reverb'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default JC Reverb'
      VSTModule = Owner
    end
    item
      DisplayName = 'Default Blended Reverb'
      VSTModule = Owner
    end
    item
      DisplayName = 'Short'
      VSTModule = Owner
    end
    item
      DisplayName = 'Long'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'T60'
      LargeStepFloat = 2.000000000000000000
      Max = 5000.000000000000000000
      Min = 10.000000000000000000
      ShortLabel = 'T60'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParamT60Change
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mix'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Mix'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamMixChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Algorithm'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'Algo'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamAlgorithmChange
      OnCustomParameterDisplay = ParamAlgorithmDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcessNetwork
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacingNetwork
  OnProcessReplacing = VSTModuleProcessNetwork
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 284
  Top = 121
  Height = 150
  Width = 215
end
