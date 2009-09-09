object ASIOVSTModule: TASIOVSTModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'ASIO Extender'
  ProductName = 'DAV Tools Examples'
  VendorName = 'Delphi ASIO & VST Project'
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  IORatio = 1.000000000000000000
  UniqueID = 'ASIO'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ASIO Driver'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 0.500000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'ASIO Dr'
      SmallStepFloat = 0.009999999776482582
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = ASIODriverChange
      OnCustomParameterDisplay = ASIODriverDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnBlockSizeChange = VSTModuleBlockSizeChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 251
  Top = 331
  Height = 156
  Width = 285
end
