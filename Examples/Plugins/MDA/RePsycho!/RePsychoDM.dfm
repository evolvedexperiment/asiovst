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
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Tune'
      Units = 'semi'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Tune'
      VSTModule = Owner
      OnParameterChange = ParameterTuneChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Fine'
      Units = 'cent'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Fine'
      VSTModule = Owner
      OnParameterChange = ParameterFineChanged
    end
    item
      Min = -50.000000000000000000
      Max = 50.000000000000000000
      Curve = ctLinear
      DisplayName = 'Decay'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MinInteger = -50
      MaxInteger = 50
      ShortLabel = 'Decay'
      VSTModule = Owner
      OnParameterChange = ParameterDecayChange
    end
    item
      Min = -30.000000000000000000
      Curve = ctLinear
      DisplayName = 'Threshold'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 0.500000000000000000
      LargeStepFloat = 2.000000000000000000
      MinInteger = -30
      MaxInteger = 0
      ShortLabel = 'Thresho'
      VSTModule = Owner
      OnParameterChange = ParameterThresholdChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Hold'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Hold'
      VSTModule = Owner
      OnParameterChange = ParameterHoldChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mix'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 0.100000001490116100
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 10.000000000000000000
      ShortLabel = 'Mix'
      VSTModule = Owner
      OnParameterChange = ParameterMixChange
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Quality'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      Flags = [kVstParameterIsSwitch]
      MaxInteger = 1
      LargeStepInteger = 1
      ShortLabel = 'Quality'
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
