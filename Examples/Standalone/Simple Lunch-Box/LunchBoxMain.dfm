object FormLunchBox: TFormLunchBox
  Left = 310
  Top = 208
  BorderStyle = bsSingle
  Caption = 'Simple Lunch Box'
  ClientHeight = 333
  ClientWidth = 429
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  Menu = MainMenu
  OldCreateOrder = False
  OnActivate = FormActivate
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnDeactivate = FormDeactivate
  OnKeyDown = FormKeyDown
  OnKeyUp = FormKeyUp
  PixelsPerInch = 96
  TextHeight = 16
  object LabelQuantize: TLabel
    Left = 318
    Top = 148
    Width = 51
    Height = 16
    Caption = 'Quant.:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 429
    Height = 24
    ButtonHeight = 19
    Caption = 'ToolBar'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    object ToolButton2: TToolButton
      Left = 0
      Top = 0
      Width = 8
      Caption = 'ToolButton2'
      ImageIndex = 0
      Style = tbsSeparator
    end
    object LabelKit: TLabel
      Left = 8
      Top = 0
      Width = 19
      Height = 19
      Caption = 'kit:'
      Layout = tlCenter
    end
    object ComboBoxKit: TComboBox
      Left = 27
      Top = 0
      Width = 84
      Height = 21
      Style = csDropDownList
      ItemIndex = 0
      TabOrder = 0
      Text = 'Acoustic'
      OnChange = ComboBoxKitChange
      OnKeyDown = FormKeyDown
      OnKeyUp = FormKeyUp
      Items.Strings = (
        'Acoustic'
        'Detroit'
        '80'#39's POP'
        'Sci-Fi'
        'Tabla'
        'Vinatge')
    end
    object ToolButton1: TToolButton
      Left = 111
      Top = 0
      Width = 8
      Caption = 'ToolButton1'
      Style = tbsSeparator
    end
    object LabelBPM: TLabel
      Left = 119
      Top = 0
      Width = 29
      Height = 19
      Caption = 'BPM:'
      Layout = tlCenter
    end
    object SpinEditTempo: TSpinEdit
      Left = 148
      Top = 0
      Width = 53
      Height = 19
      MaxValue = 200
      MinValue = 50
      TabOrder = 1
      Value = 120
      OnChange = SpinEditTempoChange
      OnKeyDown = FormKeyDown
      OnKeyUp = FormKeyUp
    end
    object ToolButton4: TToolButton
      Left = 201
      Top = 0
      Width = 7
      Caption = 'ToolButton4'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object LabelBar: TLabel
      Left = 208
      Top = 0
      Width = 25
      Height = 19
      Caption = 'Bar:'
      Layout = tlCenter
    end
    object SpinEditBar: TSpinEdit
      Left = 233
      Top = 0
      Width = 41
      Height = 19
      MaxValue = 16
      MinValue = 1
      TabOrder = 2
      Value = 1
      OnChange = SpinEditBarChange
      OnKeyDown = FormKeyDown
      OnKeyUp = FormKeyUp
    end
    object ToolButton3: TToolButton
      Left = 274
      Top = 0
      Width = 8
      Caption = 'ToolButton3'
      ImageIndex = 1
      Style = tbsSeparator
    end
    object LabelStyle: TLabel
      Left = 282
      Top = 0
      Width = 34
      Height = 19
      Caption = 'Style:'
      Layout = tlCenter
    end
    object ComboBoxStyle: TComboBox
      Left = 316
      Top = 0
      Width = 68
      Height = 21
      Style = csDropDownList
      Enabled = False
      ItemIndex = 1
      TabOrder = 3
      Text = 'Flat'
      OnChange = ComboBoxKitChange
      OnKeyDown = FormKeyDown
      OnKeyUp = FormKeyUp
      Items.Strings = (
        'Human'
        'Flat')
    end
  end
  object Button7: TButton
    Left = 8
    Top = 30
    Width = 90
    Height = 90
    Caption = '&Q'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button8: TButton
    Tag = 1
    Left = 111
    Top = 30
    Width = 90
    Height = 90
    Caption = '&W'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 8
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button9: TButton
    Tag = 2
    Left = 214
    Top = 29
    Width = 90
    Height = 90
    Caption = '&E'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 9
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button4: TButton
    Tag = 3
    Left = 8
    Top = 133
    Width = 90
    Height = 90
    Caption = '&A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 4
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button5: TButton
    Tag = 4
    Left = 111
    Top = 133
    Width = 90
    Height = 90
    Caption = '&S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button6: TButton
    Tag = 5
    Left = 214
    Top = 133
    Width = 90
    Height = 90
    Caption = '&D'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button1: TButton
    Tag = 6
    Left = 8
    Top = 236
    Width = 90
    Height = 90
    Caption = '&Y'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 1
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button2: TButton
    Tag = 7
    Left = 111
    Top = 236
    Width = 90
    Height = 90
    Caption = '&X'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 2
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object Button3: TButton
    Tag = 8
    Left = 214
    Top = 236
    Width = 90
    Height = 90
    Caption = '&C'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -48
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 3
    OnClick = DrumPadClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonMouseDown
  end
  object CheckBoxMetronome: TCheckBox
    Left = 316
    Top = 30
    Width = 103
    Height = 17
    Caption = '&Metronome'
    Checked = True
    State = cbChecked
    TabOrder = 10
    OnClick = CheckBoxMetronomeClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
  end
  object CheckBoxOverdrive: TCheckBox
    Left = 316
    Top = 96
    Width = 103
    Height = 17
    Caption = 'Overdr&ive'
    TabOrder = 11
    OnClick = CheckBoxMetronomeClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
  end
  object CheckBoxDelay: TCheckBox
    Left = 316
    Top = 73
    Width = 65
    Height = 17
    Caption = 'De&lay'
    TabOrder = 12
    OnClick = CheckBoxDelayClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
  end
  object ButtonRobotize: TButton
    Left = 316
    Top = 237
    Width = 107
    Height = 26
    Caption = 'Robotize (&V)'
    TabOrder = 13
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonRobotizeMouseDown
    OnMouseUp = ButtonRobotizeMouseUp
  end
  object ButtonRecRev: TButton
    Left = 316
    Top = 268
    Width = 107
    Height = 26
    Caption = 'Rec&&Rev (&B)'
    TabOrder = 14
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonRecRevMouseDown
    OnMouseUp = ButtonRecRevMouseUp
  end
  object ButtonFlange: TButton
    Left = 316
    Top = 298
    Width = 107
    Height = 26
    Caption = 'Flange (&N)'
    TabOrder = 15
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    OnMouseDown = ButtonFlangeMouseDown
    OnMouseUp = ButtonFlangeMouseUp
  end
  object TrackBarVolume: TTrackBar
    Left = 316
    Top = 52
    Width = 103
    Height = 15
    Max = 100
    Position = 80
    TabOrder = 16
    ThumbLength = 14
    OnChange = TrackBarVolumeChange
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
  end
  object ButtonClear: TButton
    Left = 316
    Top = 174
    Width = 107
    Height = 35
    Caption = 'C&lear'
    TabOrder = 17
    OnClick = ButtonClearClick
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
  end
  object CheckBoxQuantize: TComboBox
    Left = 369
    Top = 144
    Width = 54
    Height = 24
    Style = csDropDownList
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -13
    Font.Name = 'Verdana'
    Font.Style = []
    ItemIndex = 3
    ParentFont = False
    TabOrder = 18
    Text = '16th'
    OnChange = ComboBoxKitChange
    OnKeyDown = FormKeyDown
    OnKeyUp = FormKeyUp
    Items.Strings = (
      'free'
      '4th'
      '8th'
      '16th')
  end
  object VstHost: TVstHost
    CanDos = [hcdSendVstEvents, hcdSendVstMidiEvent, hcdSendVstTimeInfo, hcdReceiveVstEvents, hcdReceiveVstMidiEvent, hcdReceiveVstTimeInfo, hcdReportConnectionChanges, hcdAcceptIOChanges, hcdSizeWindow, hcdAsyncProcessing, hcdOffline, hcdSupplyIdle, hcdStartStopProcess]
    ManageIdleAutomaticly = False
    ParameterQuantization = 0
    PlugInDir = 'C:\Programme\Audio\Plugins\VST'
    Tempo = 120.000000000000000000
    VendorVersion = 0
    VstPlugIns = <
      item
        DisplayName = 'Realtime'
        VstOfflineTasks = <>
      end
      item
        DisplayName = 'Output'
        VstOfflineTasks = <>
      end>
    VstTimeInfo.SampleRate = 44100.000000000000000000
    VstTimeInfo.Tempo = 120.000000000000000000
    VstTimeInfo.Flags = [vtiNanosValid, vtiPpqPosValid, vtiTempoValid, vtiBarsValid, vtiCyclePosValid, vtiTimeSigValid, vtiSmpteValid, vtiClockValid]
    VstVersion = 2300
    Left = 56
    Top = 89
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcAnalog
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnLatencyChanged = ASIOHostReset
    OnReset = ASIOHostReset
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 104
    Top = 89
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 89
    object MenuItemFile: TMenuItem
      Caption = '&File'
      object MenuItemNewBeat: TMenuItem
        Caption = '&New Beat'
        OnClick = MenuItemNewBeatClick
      end
      object MenuItemLoadBeat: TMenuItem
        Caption = '&Load Beat'
        OnClick = MenuItemLoadBeatClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MenuItemSaveBeat: TMenuItem
        Caption = '&Save Beat'
        OnClick = MenuItemSaveBeatClick
      end
      object MenuItemSaveBeatAs: TMenuItem
        Caption = 'Save Beat &As...'
        OnClick = MenuItemSaveBeatAsClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MenuItemExportWAV: TMenuItem
        Caption = 'E&xport WAV...'
        OnClick = MenuItemExportWAVClick
      end
      object MenuItemExportMID: TMenuItem
        Caption = 'Ex&port MID...'
        OnClick = MenuItemExportMIDClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MenuItemQuit: TMenuItem
        Caption = '&Quit'
        OnClick = MenuItemQuitClick
      end
    end
    object MenuItemOptions: TMenuItem
      Caption = '&Options'
      object MenuItemSettings: TMenuItem
        Caption = '&ASIO Settings'
        OnClick = MenuItemSettingsClick
      end
      object MenuItemVST: TMenuItem
        Caption = '&VST Settings'
        OnClick = MenuItemVSTClick
      end
      object MenuItemMoreSettings: TMenuItem
        Caption = '&More Settings'
        Visible = False
      end
    end
    object MenuItemHelp: TMenuItem
      Caption = '&Help'
      object MenuItemShowKeys: TMenuItem
        Caption = '&Show Keys'
        OnClick = MenuItemShowKeysClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MenuItemAbout: TMenuItem
        Caption = '&About'
        OnClick = MenuItemAboutClick
      end
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = 'lunch'
    Filter = 'Beats (*.lunch)|*.lunch|All files (*.*)|*.*'
    Title = 'Load Beat'
    Left = 152
    Top = 89
  end
  object SaveWAVDialog: TSaveDialog
    DefaultExt = 'wav'
    Filter = 'Wave files (*.wav)|*.wav|All files (*.*)|*.*'
    Title = 'Export WAV'
    Left = 200
    Top = 89
  end
  object SaveMIDIDialog: TSaveDialog
    DefaultExt = 'mid'
    Filter = 'MIDI files (*.mid)|*.mid|All files (*.*)|*.*'
    Title = 'Export MID'
    Left = 264
    Top = 89
  end
end
