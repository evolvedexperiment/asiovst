object LookaheadLimiterDataModule: TLookaheadLimiterDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0.2'
  EffectName = 'Lookahead Limiter'
  ProductName = 'DAV Dynamic Examples'
  VendorName = 'Delphi ASIO & VST Project'
  VersionRelease = 2
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  InitialDelay = 64
  IORatio = 1.000000000000000000
  UniqueID = 'DVLL'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Light Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Moderate Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Independent'
      VSTModule = Owner
    end
    item
      DisplayName = 'Pumpkin'
      VSTModule = Owner
    end
    item
      DisplayName = 'More Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Heavy Compression'
      VSTModule = Owner
    end
    item
      DisplayName = 'Percussive'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Input'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 24.000000000000000000
      MaxInteger = 24
      Min = -6.000000000000000000
      MinInteger = -6
      ReportVST2Properties = True
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterInputChange
      OnCustomParameterDisplay = ParameterInputDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      Category = 'Characteristic'
      DisplayName = 'Output'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 2
      Max = 6.000000000000000000
      MaxInteger = 6
      Min = -24.000000000000000000
      MinInteger = -24
      ReportVST2Properties = True
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterOutputChange
      OnCustomParameterDisplay = ParameterOutputDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000000.000000000000000000
      Category = 'Time Constants'
      DisplayName = 'Release'
      Flags = [ppfParameterUsesFloatStep, ppfParameterSupportsDisplayIndex, ppfParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 3000.000000000000000000
      MaxInteger = 3000
      Min = 0.003000000026077032
      ReportVST2Properties = True
      ShortLabel = 'Release'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterReleaseChange
      OnCustomParameterLabel = ParameterTimeLabel
      OnCustomParameterDisplay = ParameterTimeDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Processing Mode'
      Flags = [ppfParameterUsesIntegerMinMax, ppfParameterUsesIntStep, ppfParameterSupportsDisplayIndex]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 2.000000000000000000
      MaxInteger = 2
      ReportVST2Properties = True
      ShortLabel = 'Mode'
      SmallStepFloat = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterProcessingModeChange
      OnCustomParameterDisplay = ParameterProcessingModeDisplay
    end>
  ParameterCategories = <
    item
      DisplayName = 'Characteristic'
      VSTModule = Owner
    end
    item
      DisplayName = 'Time Constants'
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcessStereo
  OnProcessReplacing = VSTModuleProcessStereo
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 461
  Top = 183
  Height = 150
  Width = 215
end
