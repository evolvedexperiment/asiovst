object ComboDataModule: TComboDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Combo'
  ProductName = 'Combo'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Amp & Speaker Simulator'
  UniqueID = 'mdaX'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Amp & Speaker Simulator'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 6.000000000000000000
      Curve = ctLinear
      DisplayName = 'Model'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      SmallStepFloat = 1.000000000000000000
      LargeStepFloat = 1.000000000000000000
      MaxInteger = 6
      LargeStepInteger = 1
      ShortLabel = 'select'
      VSTModule = Owner
      OnParameterChange = ParamModelChange
      OnCustomParameterDisplay = ParamModelDisplay
    end
    item
      Min = -100.000000000000000000
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Drive'
      Units = 'S <> H'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MinInteger = -100
      ShortLabel = 'drive'
      VSTModule = Owner
      OnParameterChange = ParamDriveChange
    end
    item
      Min = -100.000000000000000000
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'Bias'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      MinInteger = -100
      ShortLabel = 'bias'
      VSTModule = Owner
      OnParameterChange = ParamBiasChange
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
      ShortLabel = 'output'
      VSTModule = Owner
      OnParameterChange = ParamOutputChanged
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Process'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'stereo'
      VSTModule = Owner
      OnParameterChange = ParamProcessChange
      OnCustomParameterDisplay = ParamProcessDisplay
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'HPF Frequency'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'hpfFreq'
      VSTModule = Owner
      OnParameterChange = ParamHPFFreqChange
    end
    item
      Max = 100.000000000000000000
      Curve = ctLinear
      DisplayName = 'HPF Resonance'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'hpfReso'
      VSTModule = Owner
      OnParameterChange = ParamHPFResonanceChange
    end>
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
