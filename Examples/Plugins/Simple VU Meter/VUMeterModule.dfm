object VSTVUMeterModule: TVSTVUMeterModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'VU Meter'
  ProductName = 'VU Meter'
  VendorName = 'ASIO-VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'VUMt'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Volume Left'
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -90.000000000000000000
      ShortLabel = 'Volume '
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Volume Right'
      LargeStepInteger = 0
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -90.000000000000000000
      ShortLabel = 'Volume '
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = 'dB'
      VSTModule = Owner
    end>
  OnEditOpen = VSTModuleEditOpen
  OnEditIdle = VSTModuleEditIdle
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 209
  Top = 120
  Height = 150
  Width = 215
end
