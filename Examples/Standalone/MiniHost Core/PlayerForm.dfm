object Player: TPlayer
  Left = 248
  Top = 103
  BorderIcons = [biSystemMenu]
  BorderStyle = bsToolWindow
  Caption = 'MIDI/WAV Player/Recorder'
  ClientHeight = 529
  ClientWidth = 424
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
  object GroupBox4: TGroupBox
    Left = 7
    Top = 0
    Width = 201
    Height = 417
    Caption = ' MIDI File Player '
    TabOrder = 0
    object Label1: TLabel
      Left = 8
      Top = 294
      Width = 55
      Height = 14
      Caption = 'current file:'
    end
    object Label2: TLabel
      Left = 8
      Top = 308
      Width = 36
      Height = 14
      Caption = '<none>'
      Transparent = True
    end
    object tmp: TLabel
      Left = 8
      Top = 328
      Width = 76
      Height = 14
      Caption = 'tempo: 120 bpm'
    end
    object Label5: TLabel
      Left = 8
      Top = 368
      Width = 62
      Height = 14
      Caption = 'position: 0 %'
    end
    object Label9: TLabel
      Left = 8
      Top = 245
      Width = 141
      Height = 14
      Caption = 'action when finished playing:'
    end
    object MidiBox: TListBox
      Left = 8
      Top = 24
      Width = 185
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
      OnDblClick = MidiBoxDblClick
    end
    object Button1: TButton
      Left = 8
      Top = 192
      Width = 49
      Height = 17
      Caption = 'add'
      TabOrder = 1
      OnClick = Button1Click
    end
    object Button2: TButton
      Left = 64
      Top = 192
      Width = 49
      Height = 17
      Caption = 'remove'
      TabOrder = 2
      OnClick = Button2Click
    end
    object Button3: TButton
      Left = 64
      Top = 216
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 3
      OnClick = Button3Click
    end
    object Button4: TButton
      Left = 8
      Top = 216
      Width = 49
      Height = 17
      Caption = 'play'
      TabOrder = 4
      OnClick = Button4Click
    end
    object onlych1: TCheckBox
      Left = 124
      Top = 192
      Width = 69
      Height = 17
      Caption = 'only CH1'
      TabOrder = 5
    end
    object mode1: TComboBox
      Left = 8
      Top = 261
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
      ItemHeight = 14
      ItemIndex = 1
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 6
      Text = 'play same file again'
      Items.Strings = (
        'stop playback'
        'play same file again'
        'play next file in list'
        'play random file in list')
    end
    object s_tempo: TScrollBar
      Left = 8
      Top = 344
      Width = 177
      Height = 16
      Max = 240
      Min = 20
      PageSize = 0
      Position = 120
      TabOrder = 7
      OnChange = s_tempoChange
    end
    object s_pos: TScrollBar
      Left = 8
      Top = 384
      Width = 177
      Height = 16
      PageSize = 0
      TabOrder = 8
      OnChange = s_posChange
    end
  end
  object GroupBox1: TGroupBox
    Left = 215
    Top = 0
    Width = 201
    Height = 417
    Caption = ' WAV File Player '
    TabOrder = 1
    object Label3: TLabel
      Left = 8
      Top = 294
      Width = 55
      Height = 14
      Caption = 'current file:'
    end
    object Label4: TLabel
      Left = 8
      Top = 308
      Width = 36
      Height = 14
      Caption = '<none>'
      Transparent = True
    end
    object Label6: TLabel
      Left = 8
      Top = 328
      Width = 60
      Height = 14
      Caption = 'pitch: 100 %'
    end
    object Label7: TLabel
      Left = 8
      Top = 368
      Width = 62
      Height = 14
      Caption = 'position: 0 %'
    end
    object Label10: TLabel
      Left = 8
      Top = 245
      Width = 141
      Height = 14
      Caption = 'action when finished playing:'
    end
    object WavBox: TListBox
      Left = 8
      Top = 24
      Width = 185
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
      OnDblClick = WavBoxDblClick
    end
    object Button5: TButton
      Left = 8
      Top = 192
      Width = 49
      Height = 17
      Caption = 'add'
      TabOrder = 1
      OnClick = Button5Click
    end
    object Button6: TButton
      Left = 64
      Top = 192
      Width = 49
      Height = 17
      Caption = 'remove'
      TabOrder = 2
      OnClick = Button6Click
    end
    object Button7: TButton
      Left = 64
      Top = 216
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 3
      OnClick = Button7Click
    end
    object Button8: TButton
      Left = 8
      Top = 216
      Width = 49
      Height = 17
      Caption = 'play'
      TabOrder = 4
      OnClick = Button8Click
    end
    object mode2: TComboBox
      Left = 8
      Top = 261
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
      ItemHeight = 14
      ItemIndex = 1
      ParentCtl3D = False
      ParentFont = False
      TabOrder = 5
      Text = 'play same file again'
      OnChange = mode2Change
      Items.Strings = (
        'stop playback'
        'play same file again'
        'play next file in list'
        'play random file in list')
    end
    object s_pitch: TScrollBar
      Left = 8
      Top = 344
      Width = 177
      Height = 16
      Max = 341
      Min = 1
      PageSize = 0
      Position = 170
      TabOrder = 6
      OnChange = s_pitchChange
    end
    object s_pos2: TScrollBar
      Left = 8
      Top = 384
      Width = 177
      Height = 16
      PageSize = 0
      TabOrder = 7
      OnChange = s_pos2Change
    end
  end
  object GroupBox2: TGroupBox
    Left = 7
    Top = 418
    Width = 410
    Height = 105
    Caption = ' WAV Recorder '
    TabOrder = 2
    object Label8: TLabel
      Left = 8
      Top = 48
      Width = 138
      Height = 14
      Caption = 'current file (click to change):'
    end
    object wavfile: TLabel
      Left = 8
      Top = 62
      Width = 36
      Height = 14
      Cursor = crHandPoint
      Caption = '<none>'
      Transparent = True
      OnClick = wavfileClick
    end
    object rstatus: TLabel
      Left = 8
      Top = 80
      Width = 75
      Height = 14
      Caption = 'status: stopped'
    end
    object Button10: TButton
      Left = 64
      Top = 24
      Width = 49
      Height = 17
      Caption = 'pause'
      TabOrder = 0
      OnClick = Button10Click
    end
    object Button11: TButton
      Left = 120
      Top = 24
      Width = 49
      Height = 17
      Caption = 'stop'
      TabOrder = 1
      OnClick = Button11Click
    end
    object Button12: TButton
      Left = 8
      Top = 24
      Width = 49
      Height = 17
      Caption = 'record'
      TabOrder = 2
      OnClick = Button12Click
    end
    object recinmono: TCheckBox
      Left = 185
      Top = 24
      Width = 96
      Height = 17
      Caption = 'record in mono'
      TabOrder = 3
    end
    object rformat: TComboBox
      Left = 304
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
      ItemHeight = 14
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
