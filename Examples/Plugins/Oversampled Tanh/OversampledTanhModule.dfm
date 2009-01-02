object OversampledTanhModule: TOversampledTanhModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '0.0'
  EffectName = 'Oversampled Tanh'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  BlockSize = 16
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  ProcessingMode = pmBlockSave
  BlockModeSize = 16
  InitialDelay = 16
  IORatio = 1.000000000000000000
  UniqueID = 'Down'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Number of Coeffs'
      LargeStepFloat = 2.000000000000000000
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'CoefCnt'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamCoeffsChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Transition'
      LargeStepFloat = 0.500000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      ShortLabel = 'Transit'
      SmallStepFloat = 0.050000000745058060
      StepFloat = 0.100000001490116100
      VSTModule = Owner
      OnParameterChange = ParamTransitionChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTEditOpen
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 296
  Top = 130
  Height = 150
  Width = 215
end
