object ASIOVSTModule: TASIOVSTModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'ASIO Extender'
  ProductName = 'ASIO Extender'
  VendorName = 'ASIO/VST Project'
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  UniqueID = 'ASIO'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'ASIO Driver'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      SmallStepFloat = 0.009999999776482582
      LargeStepFloat = 0.500000000000000000
      MaxInteger = 1
      LargeStepInteger = 1
      VSTModule = Owner
      OnParameterChange = ASIODriverChange
      OnCustomParameterDisplay = ASIODriverDisplay
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 251
  Top = 331
  Height = 156
  Width = 285
end
