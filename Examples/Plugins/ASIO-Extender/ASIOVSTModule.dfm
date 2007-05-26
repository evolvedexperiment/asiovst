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
  PlugCategory = vcgUnknown
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
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnCustomParameterDisplay = ASIOVSTModuleParameterProperties0CustomParameterDisplay
    end>
  OnProcess = VST2ModuleProcess
  OnProcessReplacing = VST2ModuleProcess
  Left = 281
  Top = 208
  Height = 270
  Width = 480
end
