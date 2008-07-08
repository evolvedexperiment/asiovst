object DX10DataModule: TDX10DataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '0.0'
  EffectName = 'mda DX10'
  ProductName = 'DX10'
  VendorName = 'mda'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend]
  SampleRate = 44100.000000000000000000
  numInputs = 0
  CurrentProgram = 0
  CurrentProgramName = 'Bright E.Piano'
  UniqueID = 'MDAx'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Bright E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Jazz E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'E.Piano Pad'
      VSTModule = Owner
    end
    item
      DisplayName = 'Fuzzy E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Chime'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harpsichord'
      VSTModule = Owner
    end
    item
      DisplayName = 'Funk Clav'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sitar'
      VSTModule = Owner
    end
    item
      DisplayName = 'Chiff Organ'
      VSTModule = Owner
    end
    item
      DisplayName = 'Tinkle'
      VSTModule = Owner
    end
    item
      DisplayName = 'Space Pad'
      VSTModule = Owner
    end
    item
      DisplayName = 'Koto'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harp'
      VSTModule = Owner
    end
    item
      DisplayName = 'Jazz Guitar'
      VSTModule = Owner
    end
    item
      DisplayName = 'Steel Drum'
      VSTModule = Owner
    end
    item
      DisplayName = 'Log Drum'
      VSTModule = Owner
    end
    item
      DisplayName = 'Trumpet'
      VSTModule = Owner
    end
    item
      DisplayName = 'Horn'
      VSTModule = Owner
    end
    item
      DisplayName = 'Reed 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Reed 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Violin'
      VSTModule = Owner
    end
    item
      DisplayName = 'Chunky Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'E.Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Clunk Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Thick Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sine Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Square Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Upright Bass 1'
      VSTModule = Owner
    end
    item
      DisplayName = 'Upright Bass 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Harmonics'
      VSTModule = Owner
    end
    item
      DisplayName = 'Scratch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Syn Tom'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Attack'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Attack'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Decay'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Decay'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Release'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Release'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Coarse'
      Units = 'ratio'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Coarse'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Fine'
      Units = 'ratio'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Fine'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mod Init'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Mod Ini'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mod Dec'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Mod Dec'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mod Sus'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Mod Sus'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mod Rel'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Mod Rel'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mod Vel'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Mod Vel'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Vibrato'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Vibrato'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Octave'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Octave'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'FineTune'
      Units = 'cents'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'FineTun'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Waveform'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Wavefrm'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Mod Thru'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'ModThru'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'LFO Rate'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'LFORate'
      VSTModule = Owner
    end>
  Left = 218
  Top = 81
  Height = 150
  Width = 194
end
