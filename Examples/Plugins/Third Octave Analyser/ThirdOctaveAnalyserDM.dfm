object ThirdOctaveAnalyserModule: TThirdOctaveAnalyserModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Third Octave Analyser'
  ProductName = 'DAV Analyser Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numOutputs = 0
  CurrentProgram = -1
  IORatio = 1.000000000000000000
  UniqueID = 'TOAV'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLogarithmic
      CurveFactor = 100.000000000000000000
      DisplayName = 'Smooth'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex, kVstParameterSupportsDisplayCategory]
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      MinInteger = 1
      ReportVST2Properties = True
      ShortLabel = 'Smooth'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterSmoothChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fullscale Gain'
      LargeStepFloat = 2.000000000000000000
      Max = 120.000000000000000000
      MaxInteger = 120
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'FSGain'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterFullscaleGainChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcessNormal
  Left = 286
  Top = 81
  Height = 150
  Width = 215
end
