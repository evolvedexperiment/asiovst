object VSTOpAmp: TVSTOpAmp
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Simple OpAmp Simulation'
  ProductName = 'Simple OpAmp Simulation'
  VendorName = 'Christian Budde'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vcgEffect
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  KeysRequired = False
  UniqueID = 'NoLi'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = 1.000000000000000000
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Gain'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 2.000000000000000000
      Flags = []
      MinInteger = 1
      MaxInteger = 10
      StepInteger = 1
      LargeStepInteger = 2
      ShortLabel = 'Gain'
      VSTModule = Owner
    end>
  OnEditOpen = VST_EditOpen
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnInitialize = VSTModuleInitialize
  Left = 243
  Top = 103
  Height = 150
  Width = 215
end
