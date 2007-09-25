object ASIOVSTModule: TASIOVSTModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VST2ModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'ASIO Extender'
  ProductName = 'ASIO Extender'
  VendorName = 'ASIO/VST Project'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vpcUnknown
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  KeysRequired = False
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
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 0.100000001490116100
      SmallStepFloat = 0.009999999776482582
      LargeStepFloat = 0.500000000000000000
      Flags = []
      MinInteger = 0
      MaxInteger = 1
      StepInteger = 1
      LargeStepInteger = 1
      VSTModule = Owner
      OnParameterChange = ASIODriverChange
      OnCustomParameterDisplay = ASIODriverDisplay
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VST_EditOpen
  OnProcess = VST2ModuleProcess
  OnProcessReplacing = VST2ModuleProcess
  Left = 251
  Top = 331
  Height = 156
  Width = 285
end
