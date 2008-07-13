object DX10DataModule: TDX10DataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
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
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Attack'
      Max = 1.000000000000000000
      ShortLabel = 'Attack'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Decay'
      Max = 1.000000000000000000
      ShortLabel = 'Decay'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Release'
      Max = 1.000000000000000000
      ShortLabel = 'Release'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Coarse'
      Max = 1.000000000000000000
      ShortLabel = 'Coarse'
      SmoothingFactor = 1.000000000000000000
      Units = 'ratio'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fine'
      Max = 1.000000000000000000
      ShortLabel = 'Fine'
      SmoothingFactor = 1.000000000000000000
      Units = 'ratio'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Init'
      Max = 1.000000000000000000
      ShortLabel = 'Mod Ini'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Dec'
      Max = 1.000000000000000000
      ShortLabel = 'Mod Dec'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Sus'
      Max = 1.000000000000000000
      ShortLabel = 'Mod Sus'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Rel'
      Max = 1.000000000000000000
      ShortLabel = 'Mod Rel'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Vel'
      Max = 1.000000000000000000
      ShortLabel = 'Mod Vel'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Vibrato'
      Max = 1.000000000000000000
      ShortLabel = 'Vibrato'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
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
      DisplayName = 'FineTune'
      Max = 1.000000000000000000
      ShortLabel = 'FineTun'
      SmoothingFactor = 1.000000000000000000
      Units = 'cents'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Waveform'
      Max = 1.000000000000000000
      ShortLabel = 'Wavefrm'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod Thru'
      Max = 1.000000000000000000
      ShortLabel = 'ModThru'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'LFO Rate'
      Max = 1.000000000000000000
      ShortLabel = 'LFORate'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end>
  OnResume = VSTModuleResume
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 218
  Top = 81
  Height = 150
  Width = 194
end
