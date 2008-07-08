object DelayDataModule: TDelayDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = []
  Version = '1.0'
  EffectName = 'mda Delay'
  ProductName = 'Delay'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Delay'
  UniqueID = 'mday'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Delay'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'L Delay'
      Max = 1.000000000000000000
      ShortLabel = 'L Delay'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'R Delay'
      Max = 1.000000000000000000
      ShortLabel = 'R Delay'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Feedback'
      Max = 1.000000000000000000
      ShortLabel = 'Feedbac'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fb Tone'
      Max = 1.000000000000000000
      ShortLabel = 'Fb Tone'
      SmoothingFactor = 1.000000000000000000
      Units = 'Lo <> Hi'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'FX Mix'
      Max = 1.000000000000000000
      ShortLabel = 'FX Mix'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      Max = 1.000000000000000000
      ShortLabel = 'Output'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end>
  OnParameterChange = VSTModuleParameterChange
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
