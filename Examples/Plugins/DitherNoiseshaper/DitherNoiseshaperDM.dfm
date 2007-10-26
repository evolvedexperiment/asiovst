object DitherNoiseshaperModule: TDitherNoiseshaperModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
  Version = '1.0'
  EffectName = 'Dither & Noiseshaper Plugin'
  ProductName = 'ASIO & VST for Delphi project'
  VendorName = 'http://sourceforge.net/projects/delphiasiovst/'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'DiNo'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Min = 2.000000000000000000
      Max = 32.000000000000000000
      Curve = ctLinear
      DisplayName = 'Bit Depth'
      Units = 'Bit'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      MinInteger = 2
      MaxInteger = 32
      LargeStepInteger = 1
      VSTModule = Owner
      OnParameterChange = DNBitDepthChange
      OnCustomParameterDisplay = DNBitDepthDisplay
    end
    item
      Max = 7.000000000000000000
      Curve = ctLinear
      DisplayName = 'Type'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      MaxInteger = 7
      LargeStepInteger = 1
      VSTModule = Owner
      OnParameterChange = DNTypeChange
      OnCustomParameterDisplay = DNTypeDisplay
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
