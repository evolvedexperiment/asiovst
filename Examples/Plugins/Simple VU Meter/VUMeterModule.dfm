object VSTVUMeterModule: TVSTVUMeterModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'VU Meter'
  ProductName = 'VU Meter'
  VendorName = 'ASIO-VST Project'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vpcEffect
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = -1
  KeysRequired = False
  UniqueID = 'VUMt'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Min = -90.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Volume Left'
      Units = 'dB'
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
    end
    item
      Min = -90.000000000000000000
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Volume Right'
      Units = 'dB'
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
    end>
  OnEditOpen = VST_EditOpen
  OnEditIdle = VSTModuleEditIdle
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 243
  Top = 108
  Height = 150
  Width = 215
end
