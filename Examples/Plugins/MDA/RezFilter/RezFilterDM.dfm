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
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Frequency'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Frequen'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Resonance'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Resonan'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Output'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Output'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Env->VCF'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Env->VC'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Attack'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Attack'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Release'
      Units = 'ms'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Release'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'LFO->VCF'
      Units = 'S+H<>Sin'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'LFO->VC'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'LFO Rate'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'LFO Rat'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Trigger'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Trigger'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Max. Frequency'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'MaxFreq'
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
