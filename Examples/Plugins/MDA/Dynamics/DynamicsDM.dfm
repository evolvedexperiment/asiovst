object DynamicsDataModule: TDynamicsDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Dynamics'
  ProductName = 'Dynamics'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Dynamics'
  UniqueID = 'mdaN'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Dynamics'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      Max = 1.000000000000000000
      ShortLabel = 'Thresho'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = Parameter0Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Ratio'
      Max = 1.000000000000000000
      ShortLabel = 'Ratio'
      SmoothingFactor = 1.000000000000000000
      Units = ':1'
      VSTModule = Owner
      OnCustomParameterDisplay = Parameter1Display
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
      OnCustomParameterDisplay = Parameter2Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Max = 1.000000000000000000
      ShortLabel = 'Attack'
      SmoothingFactor = 1.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = ParameterAttackChange
      OnCustomParameterDisplay = Parameter3Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterDisplay = Parameter4Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Limiter'
      Max = 1.000000000000000000
      ShortLabel = 'Limiter'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = Parameter5Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gate Threshold'
      Max = 1.000000000000000000
      ShortLabel = 'GateThr'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = Parameter6Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gate Attack'
      Max = 1.000000000000000000
      ShortLabel = 'GateAtt'
      SmoothingFactor = 1.000000000000000000
      Units = #181's'
      VSTModule = Owner
      OnParameterChange = ParameterGateChange
      OnCustomParameterDisplay = Parameter7Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Gate Release'
      Max = 1.000000000000000000
      ShortLabel = 'GateRel'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterGateChangeRelease
      OnCustomParameterDisplay = Parameter8Display
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mix'
      Max = 1.000000000000000000
      ShortLabel = 'Mix'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnCustomParameterDisplay = Parameter9Display
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
