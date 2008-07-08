object LeslieDataModule: TLeslieDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = []
  Version = '1.0'
  EffectName = 'mda Leslie'
  ProductName = 'Leslie'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Leslie Simulator'
  UniqueID = 'mdaH'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Leslie Simulator'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Speed'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Speed'
      VSTModule = Owner
      OnCustomParameterDisplay = ParamSpeedDisplay
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Low Width'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'LoWidth'
      VSTModule = Owner
      OnParameterChange = ParameterLowWidthChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Low Throb'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'LoThrob'
      VSTModule = Owner
      OnParameterChange = ParameterLowThrobChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'High Width'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'HiWidth'
      VSTModule = Owner
      OnParameterChange = ParameterHighWidthChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'High Depth'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'HiDepth'
      VSTModule = Owner
      OnParameterChange = ParameterHighDepthChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'High Throb'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'HiThrob'
      VSTModule = Owner
      OnParameterChange = ParameterHighThrobChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'X-Over'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'X-Over'
      VSTModule = Owner
      OnParameterChange = ParameterXOverChange
    end
    item
      Min = -20.000000000000000000
      Max = 20.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MinInteger = -20
      MaxInteger = 20
      ShortLabel = 'Output'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
    end
    item
      Max = 200.000000000000000000
      Curve = ctLinear
      DisplayName = 'Speed'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MaxInteger = 200
      ShortLabel = 'Speed'
      VSTModule = Owner
      OnParameterChange = ParameterSpeedChange
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
