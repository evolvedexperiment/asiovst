object TalkBoxDataModule: TTalkBoxDataModule
  OldCreateOrder = False
  Flags = [effFlagsCanMono, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'mda TalkBox'
  ProductName = 'DAV mda'
  VendorName = 'Delphi ASIO & VST Project / mda'
  PlugCategory = vpcEffect
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Talkbox'
  IORatio = 1.000000000000000000
  UniqueID = 'mda&'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Talkbox'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Wet'
      LargeStepFloat = 2.000000000000000000
      Max = 200.000000000000000000
      MaxInteger = 200
      ShortLabel = 'Wet'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Dry'
      LargeStepFloat = 2.000000000000000000
      Max = 200.000000000000000000
      MaxInteger = 200
      ShortLabel = 'Dry'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      Units = '%'
      VSTModule = Owner
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Carrier'
      LargeStepFloat = 1.000000000000000000
      LargeStepInteger = 1
      Max = 1.000000000000000000
      MaxInteger = 1
      ShortLabel = 'Carrier'
      SmallStepFloat = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
      OnCustomParameterDisplay = ParameterCarrierDisplay
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Quality'
      LargeStepFloat = 2.000000000000000000
      Max = 1.000000000000000000
      ShortLabel = 'Quality'
      SmallStepFloat = 0.500000000000000000
      SmoothingFactor = 1.000000000000000000
      StepFloat = 1.000000000000000000
      VSTModule = Owner
    end>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnProcess = VSTModuleProcess
  OnProcessReplacing = VSTModuleProcess
  OnResume = VSTModuleResume
  OnSuspend = VSTModuleSuspend
  Left = 188
  Top = 77
  Height = 150
  Width = 215
end
