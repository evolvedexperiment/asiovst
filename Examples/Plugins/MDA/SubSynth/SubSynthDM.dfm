object SubSynthDataModule: TSubSynthDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda SubSynth'
  ProductName = 'SubSynth'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Sub Bass Synthesizer'
  UniqueID = 'mdaB'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Sub Bass Synthesizer'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Type'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Type'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Level'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Level'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Tune'
      Units = 'Hz'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Tune'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Dry Mix'
      Units = '%'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Dry Mix'
      VSTModule = Owner
    end
    item
      Max = 1.000000000000000000
      Curve = ctLinear
      DisplayName = 'Thresh'
      Units = 'dB'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      ShortLabel = 'Thresh'
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
    end>
  OnOpen = VSTModuleOpen
  OnParameterChange = VSTModuleParameterChange
  OnResume = VSTModuleResume
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
