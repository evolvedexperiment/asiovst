object VSTHRTF3DModule: TVSTHRTF3DModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'HRTF 3D'
  VendorName = 'Christian Budde'
  PlugCategory = vpcEffect
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  InitialDelay = 64
  IORatio = 1.000000000000000000
  UniqueID = 'HR3D'
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
      DisplayName = 'Azimuth'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 360.000000000000000000
      MaxInteger = 360
      ShortLabel = 'Azmth'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = #176
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Polar'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 140.000000000000000000
      MaxInteger = 140
      ShortLabel = 'Polar'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = #176
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Radius'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 2.000000000000000000
      MaxInteger = 2
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Rad'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'm'
      VSTModule = Owner
    end>
  ParameterCategories = <>
  OnOpen = VST2ModuleOpen
  OnClose = VST2ModuleClose
  OnEditOpen = VST_EditOpen
  OnParameterChange = VST2ModuleParameterChange
  OnProcess = VST2ModuleProcess
  OnProcessReplacing = VST2ModuleProcess
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 273
  Top = 72
  Height = 150
  Width = 215
end
