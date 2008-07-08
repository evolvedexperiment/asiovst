object StereoDataModule: TStereoDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Stereo'
  ProductName = 'Stereo'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Stereo Simulator'
  UniqueID = 'mdaS'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Stereo Simulator'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Width'
      Max = 1.000000000000000000
      ShortLabel = 'Width'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterWidthChange
      OnCustomParameterLabel = ParameterWidthLabel
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Delay'
      Max = 1.000000000000000000
      ShortLabel = 'Delay'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterDelayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Balance'
      Max = 1.000000000000000000
      ShortLabel = 'Balance'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod'
      Max = 1.000000000000000000
      ShortLabel = 'Mod'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterModChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Rate'
      Max = 1.000000000000000000
      ShortLabel = 'Rate'
      SmoothingFactor = 1.000000000000000000
      Units = 'sec'
      VSTModule = Owner
      OnParameterChange = ParameterRateChange
    end>
  OnParameterChange = VSTModuleParameterChange
  OnSuspend = VSTModuleSuspend
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
