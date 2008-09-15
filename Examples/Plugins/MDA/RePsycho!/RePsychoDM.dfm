object RePsychoDataModule: TRePsychoDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda RePsycho'
  ProductName = 'RePsycho'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Re-PsYcHo!'
  UniqueID = 'mdaY'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Re-PsYcHo!'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tune'
      Max = 1.000000000000000000
      ShortLabel = 'Tune'
      SmoothingFactor = 1.000000000000000000
      Units = 'semi'
      VSTModule = Owner
      OnParameterChange = ParameterTuneChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fine'
      Max = 1.000000000000000000
      ShortLabel = 'Fine'
      SmoothingFactor = 1.000000000000000000
      Units = 'cent'
      VSTModule = Owner
      OnParameterChange = ParameterFineChanged
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Decay'
      Max = 50.000000000000000000
      MaxInteger = 50
      Min = -50.000000000000000000
      MinInteger = -50
      ShortLabel = 'Decay'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDecayChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Threshold'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -30.000000000000000000
      MinInteger = -30
      ShortLabel = 'Thresho'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hold'
      Max = 1.000000000000000000
      ShortLabel = 'Hold'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnParameterChange = ParameterHoldChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mix'
      LargeStepFloat = 10.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Mix'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterMixChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Quality'
      Flags = [kVstParameterIsSwitch]
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Quality'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterQualityDisplay
    end>
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
