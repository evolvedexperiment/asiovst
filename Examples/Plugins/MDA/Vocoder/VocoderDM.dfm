object VocoderDataModule: TVocoderDataModule
  OldCreateOrder = False
  OnCreate = VSTModuleCreate
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda Vocoder'
  ProductName = 'Vocoder'
  VendorName = 'mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Vocoder'
  UniqueID = 'mdav'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Vocoder'
      VSTModule = Owner
    end
    item
      DisplayName = '16 Band Vocoder'
      VSTModule = Owner
    end
    item
      DisplayName = 'Old Vocoder'
      VSTModule = Owner
    end
    item
      DisplayName = 'Choral Vocoder'
      VSTModule = Owner
    end
    item
      DisplayName = 'Pad Vocoder'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mod In'
      Max = 1.000000000000000000
      ShortLabel = 'Mod In'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
      OnParameterChange = ParameterModInChange
      OnCustomParameterDisplay = ParameterModInDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Output'
      Max = 1.000000000000000000
      ShortLabel = 'Output'
      SmoothingFactor = 1.000000000000000000
      Units = 'dB'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hi Thru'
      Max = 1.000000000000000000
      ShortLabel = 'Hi Thru'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Hi Band'
      Max = 1.000000000000000000
      ShortLabel = 'Hi Band'
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
      OnParameterChange = ParameterHiBandChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Envelope'
      Max = 1.000000000000000000
      ShortLabel = 'Envelop'
      SmoothingFactor = 1.000000000000000000
      Units = 'ms'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Filter Q'
      Max = 1.000000000000000000
      ShortLabel = 'Filter '
      SmoothingFactor = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Mid Freq'
      Max = 1.000000000000000000
      ShortLabel = 'Mid Fre'
      SmoothingFactor = 1.000000000000000000
      Units = 'Hz'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Quality'
      Max = 1.000000000000000000
      ShortLabel = 'Quality'
      SmoothingFactor = 1.000000000000000000
      VSTModule = Owner
    end>
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
