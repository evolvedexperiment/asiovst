object FmVstPresetDesigner: TFmVstPresetDesigner
  Left = 218
  Top = 77
  Caption = 'VST Preset Designer'
  ClientHeight = 116
  ClientWidth = 255
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 255
    Height = 29
    Caption = 'ToolBar'
    TabOrder = 0
    Visible = False
  end
  object VSTPanel: TPanel
    Left = 0
    Top = 29
    Width = 255
    Height = 87
    Align = alClient
    BevelOuter = bvNone
    Caption = '(empty)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6695441
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 1
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Program Files\Audio\Plugins\VST\=Delphi='
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Plugin'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 16
    Top = 8
  end
  object MainMenu: TMainMenu
    Left = 48
    Top = 8
    object MIVstPlugin: TMenuItem
      Caption = '&VST Plugin'
      object MiOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MiOpenClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object N4: TMenuItem
        Caption = '-'
        Visible = False
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiPrograms: TMenuItem
      Caption = '&Programs'
      object MiLoad: TMenuItem
        Caption = '&Load...'
      end
      object MiSave: TMenuItem
        Caption = '&Save...'
        OnClick = MiSaveClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiRandomize: TMenuItem
        Caption = '&Randomize!'
        OnClick = MiRandomizeClick
      end
      object MIShuffle: TMenuItem
        Caption = 'S&huffle!'
        OnClick = MIShuffleClick
      end
      object MiDesign: TMenuItem
        Caption = 'Design...'
        OnClick = MiDesignClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
    end
    object MiPreview: TMenuItem
      Caption = 'Pre&view'
      object MiOpenMIDI: TMenuItem
        Caption = 'Open &MIDI...'
        OnClick = MiOpenMIDIClick
      end
      object MiOpenAudio: TMenuItem
        Caption = '&Open Audio...'
        OnClick = MiOpenAudioClick
      end
      object N5: TMenuItem
        Caption = '-'
      end
      object MiPlayPreview: TMenuItem
        Caption = 'Preview!'
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.dll'
    Filter = 'VST Plugin (*.dll)|*.dll'
    Title = 'Select a VST Plugin'
    Left = 80
    Top = 8
  end
  object SaveDialog: TSaveDialog
    DefaultExt = '.fxb'
    Filter = 'VST Bank (*.fxb)|*.fxb|VST Preset (*.fxp)|*.fxp'
    Title = 'Save as preset/bank'
    Left = 176
    Top = 8
  end
  object OpenAudio: TOpenDialog
    DefaultExt = '.wav'
    Filter = 
      'MP3 File (*.mp3)|*.mp3|Wave File (*.wav)|*.wav|AIFF File (*.aiff' +
      ')|*.aiff|AU File (*.au)|*.au'
    Title = 'Select an Audio  File'
    Left = 112
    Top = 8
  end
  object OpenMidi: TOpenDialog
    DefaultExt = '.mid'
    Filter = 'MIDI (*.mid)|*.mid'
    Title = 'Select a MIDI file'
    Left = 144
    Top = 8
  end
end
