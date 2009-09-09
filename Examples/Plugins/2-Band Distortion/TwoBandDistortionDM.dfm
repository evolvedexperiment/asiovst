object TwoBandDistortionDataModule: TTwoBandDistortionDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'TwoBand Distortion'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'CODI'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      Category = 'Crossover'
      DisplayName = 'Frequency'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 20.000000000000000000
      MinInteger = 20
      ReportVST2Properties = True
      ShortLabel = 'Freq.'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamFrequencyChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Crossover'
      DisplayName = 'Order'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 16.000000000000000000
      MaxInteger = 16
      Min = 2.000000000000000000
      MinInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'Order'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterOrderChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Distortion'
      DisplayName = 'Low Distortion'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesFloatStep, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'LoDist'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamLowDistChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Distortion'
      DisplayName = 'High Distortion'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesFloatStep, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 100.000000000000000000
      ReportVST2Properties = True
      ShortLabel = 'HiDist'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamHighDistChange
    end>
  ParameterCategories = <
    item
      DisplayName = 'Crossover'
      VSTModule = Owner
    end
    item
      DisplayName = 'Distortion'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
