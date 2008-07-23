object EPianoDataModule: TEPianoDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'mda E-Piano'
  ProductName = 'E-Piano'
  VendorName = 'mda'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent]
  SampleRate = 44100.000000000000000000
  numInputs = 0
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  UniqueID = 'MDAe'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end
    item
      DisplayName = 'Bright'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mellow'
      VSTModule = Owner
    end
    item
      DisplayName = 'Autopan'
      VSTModule = Owner
    end
    item
      DisplayName = 'Tremolo'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end
    item
      DisplayName = 'Init'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Envelope Decay'
      Max = 1.000000000000000000
      ShortLabel = 'Envelop'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Envelope Release'
      Max = 1.000000000000000000
      ShortLabel = 'Envelop'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hardness'
      Max = 1.000000000000000000
      ShortLabel = 'Hardnes'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Treble Boost'
      Max = 1.000000000000000000
      ShortLabel = 'Treble '
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Modulation'
      Max = 1.000000000000000000
      ShortLabel = 'Modulat'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
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
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Velocity Sense'
      Max = 1.000000000000000000
      ShortLabel = 'Velocit'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stereo Width'
      Max = 1.000000000000000000
      ShortLabel = 'Modulat'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Polyphony'
      Max = 1.000000000000000000
      ShortLabel = 'Polypho'
      SmoothingFactor = 1.000000000000000000
      Units = 'voices'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Fine Tuning'
      Max = 1.000000000000000000
      ShortLabel = 'Fine Tu'
      SmoothingFactor = 1.000000000000000000
      Units = 'cents'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Random Tuning'
      Max = 1.000000000000000000
      ShortLabel = 'Random '
      SmoothingFactor = 1.000000000000000000
      Units = 'cents'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Overdrive'
      Max = 1.000000000000000000
      ShortLabel = 'Overdri'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end>
  OnSampleRateChange = VSTModuleSampleRateChange
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnOutputProperties = VSTModuleOutputProperties
  Left = 218
  Top = 81
  Height = 150
  Width = 215
end
