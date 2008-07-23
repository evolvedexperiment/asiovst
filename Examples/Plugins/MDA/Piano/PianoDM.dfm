object PianoDataModule: TPianoDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  OnDestroy = VSTModuleDestroy
  Flags = [effFlagsIsSynth]
  Version = '0.0'
  EffectName = 'mda Piano'
  ProductName = 'Piano'
  VendorName = '1.0'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent]
  SampleRate = 44100.000000000000000000
  numInputs = 0
  CurrentProgram = 0
  CurrentProgramName = 'mda Piano'
  UniqueID = 'MDAp'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'mda Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Plain Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Compressed Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Dance Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Concert Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Dark Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'School Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Broken Piano'
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
      DisplayName = 'Hardness Offset'
      Max = 1.000000000000000000
      ShortLabel = 'Hardnes'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Velocity to Hardness'
      Max = 1.000000000000000000
      ShortLabel = 'Velocit'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Muffling Filter'
      Max = 1.000000000000000000
      ShortLabel = 'Mufflin'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Velocity to Muffling'
      Max = 1.000000000000000000
      ShortLabel = 'Velocit'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Velocity to Sensitivity'
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
      ShortLabel = 'Stereo '
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
      Units = 'centes'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Random Tuning'
      Max = 1.000000000000000000
      ShortLabel = 'Random '
      SmoothingFactor = 1.000000000000000000
      Units = 'centes'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Stretch Tuning'
      Max = 1.000000000000000000
      ShortLabel = 'Stretch'
      SmoothingFactor = 1.000000000000000000
      Units = 'centes'
      VSTModule = Owner
    end>
  OnParameterChange = VSTModuleParameterChange
  OnResume = VSTModuleResume
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnOutputProperties = VSTModuleOutputProperties
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
