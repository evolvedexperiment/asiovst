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
  CurrentProgramName = '5th Sweep Pad'
  UniqueID = 'MDAj'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = '5th Sweep Pad'
      VSTModule = Owner
    end
    item
      DisplayName = 'Echo Pad [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Space Chimes [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Solid Backing'
      VSTModule = Owner
    end
    item
      DisplayName = 'Velocity Backing [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Rubber Backing [ZF]'
      VSTModule = Owner
    end
    item
      DisplayName = '808 State Lead'
      VSTModule = Owner
    end
    item
      DisplayName = 'Mono Glide'
      VSTModule = Owner
    end
    item
      DisplayName = 'Detuned Techno Lead'
      VSTModule = Owner
    end
    item
      DisplayName = 'Hard Lead [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Bubble'
      VSTModule = Owner
    end
    item
      DisplayName = 'Monosynth'
      VSTModule = Owner
    end
    item
      DisplayName = 'Moogcury Lite'
      VSTModule = Owner
    end
    item
      DisplayName = 'Gangsta Whine'
      VSTModule = Owner
    end
    item
      DisplayName = 'Higher Synth [ZF]'
      VSTModule = Owner
    end
    item
      DisplayName = '303 Saw Bass'
      VSTModule = Owner
    end
    item
      DisplayName = '303 Square Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Analog Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Analog Bass 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Low Pulses'
      VSTModule = Owner
    end
    item
      DisplayName = 'Sine Infra-Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Wobble Bass [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Squelch Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Rubber Bass [ZF]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Pick Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Fretless Bass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Whistler'
      VSTModule = Owner
    end
    item
      DisplayName = 'Very Soft Pad'
      VSTModule = Owner
    end
    item
      DisplayName = 'Pizzicato'
      VSTModule = Owner
    end
    item
      DisplayName = 'Synth Strings'
      VSTModule = Owner
    end
    item
      DisplayName = 'Synth Strings 2'
      VSTModule = Owner
    end
    item
      DisplayName = 'Leslie Organ'
      VSTModule = Owner
    end
    item
      DisplayName = 'Click Organ'
      VSTModule = Owner
    end
    item
      DisplayName = 'Hard Organ'
      VSTModule = Owner
    end
    item
      DisplayName = 'Bass Clarinet'
      VSTModule = Owner
    end
    item
      DisplayName = 'Trumpet'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft Horn'
      VSTModule = Owner
    end
    item
      DisplayName = 'Brass Section'
      VSTModule = Owner
    end
    item
      DisplayName = 'Synth Brass'
      VSTModule = Owner
    end
    item
      DisplayName = 'Detuned Syn Brass [ZF]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Power PWM'
      VSTModule = Owner
    end
    item
      DisplayName = 'Water Velocity [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Ghost [SA]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Soft E.Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Thumb Piano'
      VSTModule = Owner
    end
    item
      DisplayName = 'Steel Drums [ZF]'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Empty Patch'
      VSTModule = Owner
    end
    item
      DisplayName = 'Car Horn'
      VSTModule = Owner
    end
    item
      DisplayName = 'Helicopter'
      VSTModule = Owner
    end
    item
      DisplayName = 'Arctic Wind'
      VSTModule = Owner
    end
    item
      DisplayName = 'Thip'
      VSTModule = Owner
    end
    item
      DisplayName = 'Synth Tom'
      VSTModule = Owner
    end
    item
      DisplayName = 'Squelchy Frog'
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
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OSC Tune'
      Max = 1.000000000000000000
      ShortLabel = 'OSC Tun'
      SmoothingFactor = 1.000000000000000000
      Units = 'semi'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'OSC Fine'
      Max = 1.000000000000000000
      ShortLabel = 'OSC Fin'
      SmoothingFactor = 1.000000000000000000
      Units = 'cent'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide'
      Max = 1.000000000000000000
      ShortLabel = 'Glide'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide Rate'
      Max = 1.000000000000000000
      ShortLabel = 'Gld Rat'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Glide Bend'
      Max = 1.000000000000000000
      ShortLabel = 'Gld Ben'
      SmoothingFactor = 1.000000000000000000
      Units = 'semi'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Frequency'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Fre'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Resonance'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Res'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Envelope'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Env'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF LFO'
      Max = 1.000000000000000000
      ShortLabel = 'VCF LFO'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Velocity'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Vel'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Attack'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Att'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
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
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'VCF Release'
      Max = 1.000000000000000000
      ShortLabel = 'VCF Rel'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Attack'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Att'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Decay'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Dec'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Sustain'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Sus'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'ENV Release'
      Max = 1.000000000000000000
      ShortLabel = 'ENV Rel'
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
      DisplayName = 'Noise'
      Max = 1.000000000000000000
      ShortLabel = 'Noise'
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
      DisplayName = 'Tuning'
      Max = 1.000000000000000000
      ShortLabel = 'Tuning'
      SmoothingFactor = 1.000000000000000000
      Units = 'cent'
      VSTModule = Owner
    end>
  OnParameterChange = VSTModuleParameterChange
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
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
