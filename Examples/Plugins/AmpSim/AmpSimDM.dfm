object ComboDataModule: TComboDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsCanDoubleReplacing]
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
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Model'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 6.000000000000000000
      MaxInteger = 6
      ShortLabel = 'select'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamModelChange
      OnCustomParameterDisplay = ParamModelDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Drive'
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'drive'
      SmoothingFactor = 1.000000000000000000
      Units = 'S <> H'
      VSTModule = Owner
      OnParameterChange = ParamDriveChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Bias'
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'bias'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamBiasChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'output'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParamOutputChanged
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Process'
      Max = 1.000000000000000000
      ShortLabel = 'stereo'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParamProcessChange
      OnCustomParameterDisplay = ParamProcessDisplay
    end
    item
      Curve = ctLogarithmic
      CurveFactor = 1000.000000000000000000
      DisplayName = 'HPF Frequency'
      LargeStepFloat = 10.000000000000000000
      Max = 20000.000000000000000000
      MaxInteger = 20000
      Min = 2.000000000000000000
      MinInteger = 2
      ShortLabel = 'hpfFreq'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnParameterChange = ParamHPFFreqChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'HPF Resonance'
      Max = 100.000000000000000000
      ShortLabel = 'hpfReso'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParamHPFResonanceChange
    end>
  OnOpen = VSTModuleOpen
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessDoubleReplacing = VSTModuleProcessDoubleReplacing
  OnProcessReplacing = VSTModuleProcess
  OnSuspend = VSTModuleSuspend
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
