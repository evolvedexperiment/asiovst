object MIDIModule: TMIDIModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'MIDI Plugin'
  ProductName = 'MIDI Plugin'
  VendorName = 'Delphi VST'
  VendorVersion = 1
  PlugCategory = cgUnknown
  TailSize = 0
  CanDos = [sendVstEvents, sendVstMidiEvent, receiveVstEvents, receiveVstMidiEvent, plugAsChannelInsert, plugAsSend, _2in2out]
  SampleRate = 44100.000000000000000000
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
