object RezFilterDataModule: TRezFilterDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda RezFilter'
  ProductName = 'RezFilter'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Resonant Filter'
  UniqueID = 'mdaF'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Resonant Filter'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Frequency'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Frequen'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Resonance'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'Resonan'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      LargeStepFloat = 2.000000000000000000
      Max = 20.000000000000000000
      MaxInteger = 20
      Min = -20.000000000000000000
      MinInteger = -20
      ShortLabel = 'Output'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterGainChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Env->VCF'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'Env->VC'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Max = 1.000000000000000000
      ShortLabel = 'Attack'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterAttackDisplay
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
      OnCustomParameterDisplay = ParameterReleaseDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'LFO->VCF'
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      MinInteger = -100
      ShortLabel = 'LFO->VC'
      SmoothingFactor = 1.000000000000000000
      Units = 'S+H<>Sin'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'LFO Rate'
      Max = 1.000000000000000000
      ShortLabel = 'LFO Rat'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterLFORateDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Trigger'
      Max = 1.000000000000000000
      ShortLabel = 'Trigger'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterTriggerDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Max. Frequency'
      LargeStepFloat = 2.000000000000000000
      Max = 100.000000000000000000
      ShortLabel = 'MaxFreq'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end>
  OnParameterChange = VSTModuleParameterChange
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
