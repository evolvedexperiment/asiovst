object RoundPanDataModule: TRoundPanDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda RoundPan'
  ProductName = 'RoundPan'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Round Panner'
  UniqueID = 'mdaP'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Round Panner'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Pan'
      Units = 'deg'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Pan'
      VSTModule = Owner
      OnParameterChange = ParameterRateChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Auto'
      Units = 'deg/sec'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Auto'
      VSTModule = Owner
      OnParameterChange = ParameterAutoChange
    end>
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
