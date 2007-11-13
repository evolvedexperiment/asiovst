object SimpleGateDataModule: TSimpleGateDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor]
  Version = '0.0'
  EffectName = 'Simple Gate'
  ProductName = 'Simple Gate'
  VendorName = 'ASIO-VST Delphi Project'
  PlugCategory = vpcEffect
  CanDos = []
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'SiGa'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Min = -96.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MinInteger = -96
      MaxInteger = 0
      LargeStepInteger = 1
      ShortLabel = 'thrshld'
      VSTModule = Owner
      OnParameterChange = SGDMThresholdChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
