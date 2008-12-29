object SpinBugLiteModule: TSpinBugLiteModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'SpinBug Lite'
  ProductName = 'DAV Effect Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Mono Spin'
  IORatio = 1.000000000000000000
  UniqueID = 'SpnB'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Mono Spin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Space Spin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Stereo Spin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Distance Spin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Special Spin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Old Spin'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      CC = 0
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Coefficients'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 32.000000000000000000
      MaxInteger = 32
      Min = 1.000000000000000000
      ShortLabel = 'Coeffic'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = SBMCoefficientsChange
      OnCustomParameterDisplay = SBMCoefficientsDisplay
    end
    item
      CC = 0
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Process Type'
      Flags = [kVstParameterUsesIntegerMinMax, kVstParameterUsesIntStep]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 10.000000000000000000
      MaxInteger = 10
      Min = 1.000000000000000000
      MinInteger = 1
      ShortLabel = 'Process'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = SBMProcessTypeChange
      OnCustomParameterDisplay = SBMProcessTypeDisplay
    end
    item
      CC = 0
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'LFO Speed'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 13.000000000000000000
      MaxInteger = 13
      ShortLabel = 'LFO Spe'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = SBMLFOSpeedChange
    end
    item
      CC = 0
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'TBW'
      LargeStepFloat = 2.000000000000000000
      LargeStepInteger = 0
      Max = 0.499999910593032800
      MaxInteger = 0
      Min = 0.000000100000001169
      ShortLabel = 'TBW'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 0
      Units = 'Rad'
      VSTModule = Owner
      OnParameterChange = SBMTBWChange
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTEditOpen
  OnProcess = VSTModuleProcessMono
  OnProcessReplacing = VSTModuleProcessMono
  OnSampleRateChange = VSTModuleSampleRateChange
  Left = 285
  Top = 161
  Height = 194
  Width = 241
end
