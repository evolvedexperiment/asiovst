object SimpleDelayVST: TSimpleDelayVST
  OldCreateOrder = False
  OnCreate = VST2ModuleCreate
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
      Min = 1.000000000000000000
      Max = 44100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Delay Length'
      Units = 'Samples'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = SDDelayLengthChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VST2ModuleProcess
  OnProcessReplacing = VST2ModuleProcess
  Left = 248
  Top = 106
  Height = 150
  Width = 215
end
