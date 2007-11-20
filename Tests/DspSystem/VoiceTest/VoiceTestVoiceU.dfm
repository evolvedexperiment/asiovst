object VoiceTestVoice: TVoiceTestVoice
  OldCreateOrder = False
  OnCreate = DspVoiceCreate
  DspDirectProcessItem = DspOscSine1
  VoiceProcessingMode = pmDspQueue
  Left = 249
  Top = 116
  Height = 150
  Width = 215
  object DspOscSine1: TDspOscSine
    SampleRate = 44100.000000000000000000
    Amplitude = 1.000000000000000000
    Frequency = 440.000000000000000000
    Left = 40
    Top = 32
  end
end
