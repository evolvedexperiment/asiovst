object BeatBoxDataModule: TBeatBoxDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'mda BeatBox'
  ProductName = 'BeatBox'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'BeatBox - Drum Replacer'
  UniqueID = 'mdaG'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'BeatBox - Drum Replacer'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hat Threshold'
      Max = 1.000000000000000000
      ShortLabel = 'HatThrs'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hat Rate'
      Max = 1.000000000000000000
      ShortLabel = 'HatRate'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hat Mix'
      Max = 1.000000000000000000
      ShortLabel = 'HatMix'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Kick Threshold'
      Max = 1.000000000000000000
      ShortLabel = 'KikThrs'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Kick Trigger'
      Max = 1.000000000000000000
      ShortLabel = 'KikTrig'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Kick Mix'
      Max = 1.000000000000000000
      ShortLabel = 'KikMix'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Snare Threshold'
      Max = 1.000000000000000000
      ShortLabel = 'SnrThrs'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Snare Trigger'
      Max = 1.000000000000000000
      ShortLabel = 'SnrTrig'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Snare Mix'
      Max = 1.000000000000000000
      ShortLabel = 'SnrMix'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dynamics'
      Max = 1.000000000000000000
      ShortLabel = 'Dynamic'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterDynamicsChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Record'
      Max = 1.000000000000000000
      ShortLabel = 'Record'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Thru Mix'
      Max = 1.000000000000000000
      ShortLabel = 'ThruMix'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = ParameterMixChange
    end>
  OnParameterChange = VSTModuleParameterChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
