object Player: TPlayer
  Left = 248
  Top = 103
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'MIDI/WAV Player/Recorder'
  ClientHeight = 472
  ClientWidth = 392
  Color = clGray
  Ctl3D = False
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object GroupBoxWavFilePlayer: TGroupBox
    Left = 8
    Top = 0
    Width = 377
    Height = 353
    Caption = ' WAV File Player '
    TabOrder = 0
    object LabelWavCurrentFile: TLabel
      Left = 8
      Top = 247
      Width = 55
      Height = 14
      Caption = 'current file:'
    end
    object LabelWaveFile: TLabel
      Left = 77
      Top = 247
      Width = 36
      Height = 14
      Caption = '<none>'
      Transparent = True
    end
    object LabelWavPitch: TLabel
      Left = 8
      Top = 272
      Width = 60
      Height = 14
      Caption = 'pitch: 100 %'
    end
    object LabelWavPosition: TLabel
      Left = 8
      Top = 312
      Width = 62
      Height = 14
      Caption = 'position: 0 %'
    end
    object LabelWavPlayMode: TLabel
      Left = 226
      Top = 193
      Width = 141
      Height = 14
      Caption = 'action when finished playing:'
    end
    object ListBoxWavFiles: TListBox
      Left = 8
      Top = 24
      Width = 359
      Height = 161
      AutoComplete = False
      BevelInner = bvNone
      BevelOuter = bvNone
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemHeight = 14
      MultiSelect = True
      ParentFont = False
      TabOrder = 0
      OnDblClick = ListBoxWavFilesDblClick
    end
    object ButtonWavAdd: TButton
      Left = 8
      Top = 192
      Width = 49
      Height = 17
      Caption = 'add'
      TabOrder = 1
      OnClick = ButtonWavAddClick
    end
    object ButtonWavRemove: TButton
      Left = 64
      Top = 192
      Width = 49
      Height = 17
      Caption = 'remove'
      TabOrder = 2
      OnClick = ButtonWavRemoveClick
    end
    object ButtonWavStop: TButton
      Left = 64
      Top = 216
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 3
      OnClick = ButtonWavStopClick
    end
    object ButtonWavPlay: TButton
      Left = 8
      Top = 216
      Width = 49
      Height = 17
      Caption = 'play'
      TabOrder = 4
      OnClick = ButtonWavPlayClick
    end
    object ComboBoxWavPlayMode: TComboBox
      Left = 226
      Top = 209
      Width = 140
      Height = 22
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBlack
      Ctl3D = False
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemIndex = 1
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 5
      Text = 'play same file again'
      OnChange = ComboBoxWavPlayModeChange
      Items.Strings = (
        'stop playback'
        'play same file again'
        'play next file in list'
        'play random file in list')
    end
    object ScrollBarPitch: TScrollBar
      Left = 8
      Top = 288
      Width = 359
      Height = 16
      Max = 341
      Min = 1
      PageSize = 0
      Position = 170
      TabOrder = 6
      OnChange = ScrollBarPitchChange
    end
    object ScrollBarWavPosition: TScrollBar
      Left = 8
      Top = 328
      Width = 359
      Height = 16
      PageSize = 0
      TabOrder = 7
      OnChange = ScrollBarWavPositionChange
    end
  end
  object GroupBoxWavRecorder: TGroupBox
    Left = 8
    Top = 359
    Width = 378
    Height = 105
    Caption = ' WAV Recorder '
    TabOrder = 1
    object LabelCurrentRecordFile: TLabel
      Left = 8
      Top = 48
      Width = 138
      Height = 14
      Caption = 'current file (click to change):'
    end
    object LabelRecordFile: TLabel
      Left = 8
      Top = 62
      Width = 36
      Height = 14
      Cursor = crHandPoint
      Caption = '<none>'
      Transparent = True
      OnClick = LabelRecordFileClick
    end
    object LabelStatus: TLabel
      Left = 8
      Top = 80
      Width = 75
      Height = 14
      Caption = 'status: stopped'
    end
    object ButtonWavPause: TButton
      Left = 64
      Top = 24
      Width = 49
      Height = 17
      Caption = 'pause'
      TabOrder = 0
      OnClick = ButtonWavPauseClick
    end
    object ButtonWavStopRec: TButton
      Left = 120
      Top = 24
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 1
      OnClick = ButtonWavStopRecClick
    end
    object ButtonWavRecord: TButton
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Caption = 'record'
      TabOrder = 2
      OnClick = ButtonWavRecordClick
    end
    object CheckBoxRecInMono: TCheckBox
      Left = 185
      Top = 24
      Width = 96
      Height = 17
      Caption = 'record in mono'
      TabOrder = 3
    end
    object ComboBoxRecordFormat: TComboBox
      Left = 280
      Top = 22
      Width = 89
      Height = 22
      BevelInner = bvNone
      BevelOuter = bvNone
      Style = csDropDownList
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ItemIndex = 0
      ParentFont = False
      TabOrder = 4
      Text = '16 bit integer'
      Items.Strings = (
        '16 bit integer'
        '32 bit float')
    end
  end
end
