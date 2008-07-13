object JX10DataModule: TJX10DataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'mda JX10'
  ProductName = 'JX10'
  VendorName = 'mda'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent]
  SampleRate = 44100.000000000000000000
  numInputs = 0
  CurrentProgram = 0
  CurrentProgramName = 'Init'
  UniqueID = 'MDAj'
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
      DisplayName = 'OSC Mix'
      Max = 1.000000000000000000
      ShortLabel = 'OSC Mix'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OSC Tune'
      Max = 1.000000000000000000
      ShortLabel = 'OSC Tun'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OSC Fine'
      Max = 1.000000000000000000
      ShortLabel = 'OSC Fin'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide'
      Max = 1.000000000000000000
      ShortLabel = 'Glide'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide Rate'
      Max = 1.000000000000000000
      ShortLabel = 'Gld Rat'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide Bend'
      Max = 1.000000000000000000
      ShortLabel = 'Gld Ben'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Frequency'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Fre'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Resonance'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Res'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Envelope'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Env'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF LFO'
      Max = 1.000000000000000000
      ShortLabel = 'VCF LFO'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Velocity'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Vel'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Attack'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Att'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Decay'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Dec'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Sustain'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Sus'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Release'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Rel'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Attack'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Att'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Decay'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Dec'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Sustain'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Sus'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Release'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Rel'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'LFO Rate'
      Max = 1.000000000000000000
      ShortLabel = 'LFO Rat'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Vibrato'
      Max = 1.000000000000000000
      ShortLabel = 'Vibrato'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Noise'
      Max = 1.000000000000000000
      ShortLabel = 'Noise'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Octave'
      Max = 1.000000000000000000
      ShortLabel = 'Octave'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Tuning'
      Max = 1.000000000000000000
      ShortLabel = 'Tuning'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end>
  OnResume = VSTModuleResume
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnOutputProperties = VSTModuleOutputProperties
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
