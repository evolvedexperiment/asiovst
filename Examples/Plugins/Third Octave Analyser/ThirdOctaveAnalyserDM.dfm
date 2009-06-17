object ThirdOctaveAnalyserModule: TThirdOctaveAnalyserModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'Third Octave Analyser'
  ProductName = 'DAV Analyser Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  numInputs = 1
  numOutputs = 0
  CurrentProgram = 0
  CurrentProgramName = 'Slow'
  IORatio = 1.000000000000000000
  UniqueID = 'TOAV'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Slow'
      VSTModule = Owner
    end
    item
      DisplayName = 'Medium'
      VSTModule = Owner
    end
    item
      DisplayName = 'Fast'
      VSTModule = Owner
    end>
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
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Downsampling'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'DS'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterDownsamplingChange
      OnCustomParameterDisplay = ParameterDownsamplingDisplay
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcessNormal
  OnProcessReplacing = VSTModuleProcessNormal
  Left = 286
  Top = 81
  Height = 150
  Width = 215
end
