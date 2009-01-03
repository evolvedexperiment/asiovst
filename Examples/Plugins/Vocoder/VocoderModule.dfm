object VSTSSModule: TVSTSSModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanMono, effFlagsCanReplacing, effFlagsIsSynth]
  Version = '1.0'
  EffectName = 'Vocoder'
  ProductName = 'DAV Synth Examples'
  VendorName = 'Delphi ASIO & VST Project'
  PlugCategory = vpcSynth
  CanDos = [vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcdMixDryWet, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = 0
  CurrentProgramName = 'Default'
  IORatio = 1.000000000000000000
  UniqueID = 'Voco'
  ShellPlugins = <>
  Programs = <
    item
      DisplayName = 'Default'
      VSTModule = Owner
    end>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Input Volume'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 5.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'vol in'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 3
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = VocInputVolumeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Synth Volume'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 5.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'vol syn'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 3
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = VocSynthVolumeChange
    end
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Vocoder Volume'
      Flags = [kVstParameterUsesFloatStep, kVstParameterSupportsDisplayIndex]
      LargeStepFloat = 5.000000000000000000
      Max = 1.000000000000000000
      MaxInteger = 0
      Min = -80.000000000000000000
      MinInteger = -80
      ReportVST2Properties = True
      ShortLabel = 'vol voc'
      SmallStepFloat = 0.500000000000000000
      StepFloat = 1.000000000000000000
      StepInteger = 3
      Units = 'dB'
      VSTModule = Owner
      OnParameterChange = VocVocoderVolumeChange
    end>
  ParameterCategories = <>
  OnOpen = VSTModuleOpen
  OnClose = VSTModuleClose
  OnEditOpen = VSTModuleEditOpen
  OnProcess = VSTModuleProcess
  OnProcessMidi = VSTModuleProcessMidi
  OnProcessReplacing = VSTModuleProcess
  Left = 200
  Top = 103
  Height = 150
  Width = 215
end
