object ChebyshevWaveshaperDataModule: TChebyshevWaveshaperDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Chebyshev Waveshaper'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'DCWS'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Volume'
      LargeStepFloat = 10.000000000000000000
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -30.000000000000000000
      MinInteger = -30
      ShortLabel = 'Volume'
      SmallStepFloat = 0.100000001490116100
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamVolumeChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDouble
  OnProcessReplacing = VSTModuleProcess
  Left = 286
  Top = 81
  Height = 150
  Width = 215
end
