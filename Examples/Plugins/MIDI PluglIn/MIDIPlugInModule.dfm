object MIDIModule: TMIDIModule
  OldCreateOrder = False
  Flags = [effFlagsHasEditor, effFlagsCanReplacing]
  Version = '1.0'
  EffectName = 'MIDI Plugin'
  ProductName = 'MIDI Plugin'
  VendorName = 'Delphi VST'
  CanDos = [vcdSendVstEvents, vcdSendVstMidiEvent, vcdReceiveVstEvents, vcdReceiveVstMidiEvent, vcdPlugAsChannelInsert, vcdPlugAsSend, vcd1in1out, vcd1in2out, vcd2in1out, vcd2in2out]
  SampleRate = 44100.000000000000000000
  CurrentProgram = -1
  UniqueID = 'MIDI'
  ShellPlugins = <>
  Programs = <>
  ParameterProperties = <
    item
      Curve = ctLinear
      CurveFactor = 1.000000000000000000
      DisplayName = 'Transpose'
      LargeStepInteger = 0
      Max = 24.000000000000000000
      MaxInteger = 0
      Min = -24.000000000000000000
      ShortLabel = 'Transpo'
      SmoothingFactor = 1.000000000000000000
      StepInteger = 0
      Units = 'semitone'
      VSTModule = Owner
      OnParameterChange = ParamTransposeChange
    end>
  OnEditOpen = VSTModuleEditOpen
  OnProcessMidi = VSTModuleProcessMidi
  Left = 243
  Top = 103
  Height = 150
  Width = 215
end
