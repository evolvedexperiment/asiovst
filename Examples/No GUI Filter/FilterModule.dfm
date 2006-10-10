object VSTFilter: TVSTFilter
  OldCreateOrder = False
  Flags = [effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Delphi VST Filter'
  ProductName = 'Delphi VST Filter'
  VendorName = 'Delphi VST'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = cgEffect
  TailSize = 0
  CanDos = [plugAsChannelInsert, plugAsSend, _1in1out, _1in2out, _2in1out, _2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Preset 1'
  KeysRequired = False
  UniqueID = 'Filt'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Preset 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Preset 3'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Min = 20.000000000000000000
      Max = 20000.000000000000000000
      Curve = ctLinear
      DisplayName = 'Cutoff Frequency'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 100.000000000000000000
      SmallStepFloat = 100.000000000000000000
      LargeStepFloat = 1000.000000000000000000
      Flags = []
      MinInteger = 20
      MaxInteger = 20000
      StepInteger = 100
      LargeStepInteger = 1000
      ShortLabel = 'Cutoff'
      VSTModule = Owner
      OnParameterChange = VSTFilterParameterProperties0ParameterChange
    end
    item
      Min = 0.009999999776482582
      Max = 10.000000000000000000
      Curve = ctLinear
      DisplayName = 'Resonance'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      StepFloat = 0.100000001490116100
      SmallStepFloat = 0.100000001490116100
      LargeStepFloat = 1.000000000000000000
      Flags = []
      MinInteger = 0
      MaxInteger = 10
      StepInteger = 1
      LargeStepInteger = 1
      ShortLabel = 'Res'
      VSTModule = Owner
    end>
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnInitialize = VSTModuleInitialize
  Left = 243
  Top = 103
  Height = 150
  Width = 215
end
