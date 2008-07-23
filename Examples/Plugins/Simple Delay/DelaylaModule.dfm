object SimpleDelayVST: TSimpleDelayVST
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Simple Delay'
  ProductName = 'Simple Delay'
  VendorName = 'VST Wizard Example'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  UniqueID = 'dlay'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Delay Length'
      LargeStepInteger = 0
      Max = 44100.000000000000000000
      MaxInteger = 0
      Min = 1.000000000000000000
      ShortLabel = 'Delay L'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = 'Samples'
      VSTModule = Owner
      OnParameterChange = SDDelayLengthChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
