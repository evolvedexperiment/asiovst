object MIDIModule: TMIDIModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'MIDI Plugin'
  ProductName = 'MIDI Plugin'
  VendorName = 'Delphi VST'
  VersionMajor = 1
  VersionMinor = 0
  VersionRelease = 0
  PlugCategory = vcgSynth
  TailSize = 0
  CanDos = [vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  numCategories = 1
  CurrentProgram = -1
  KeysRequired = False
  UniqueID = 'MIDI'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Min = -24.000000000000000000
      Max = 24.000000000000000000
      Curve = ctLinear
      DisplayName = 'Transpose'
      Units = 'semitone'
      CurveFactor = 1.000000000000000000
      SmoothingFactor = 1.000000000000000000
      CanBeAutomated = True
      ReportVST2Properties = False
      Flags = []
      MinInteger = 0
      MaxInteger = 0
      StepInteger = 0
      LargeStepInteger = 0
      VSTModule = Owner
      OnParameterChange = MIDIModuleParameterProperties0ParameterChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcessMidi = VSTModuleProcessMidi
  Left = 243
  Top = 103
  Height = 150
  Width = 215
end
