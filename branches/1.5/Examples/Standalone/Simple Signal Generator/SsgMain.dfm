object FmASIO: TFmASIO
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple Signal Generator'
  ClientHeight = 173
  ClientWidth = 319
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    319
    173)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDrivername: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object LabelCopyright: TLabel
    Left = 34
    Top = 155
    Width = 259
    Height = 13
    Anchors = [akBottom]
    Caption = '(C)opyright in 2004-2019 by Delphi ASIO && VST Project'
  end
  object LabelFreq: TLabel
    Left = 8
    Top = 112
    Width = 96
    Height = 13
    Caption = 'Frequency: 1000 Hz'
  end
  object LabelVolume: TLabel
    Left = 8
    Top = 69
    Width = 121
    Height = 13
    Caption = 'Volume: 1,00 equals 0 dB'
  end
  object LabelSignal: TLabel
    Left = 8
    Top = 37
    Width = 32
    Height = 13
    Caption = 'Signal:'
  end
  object LabelDistribution: TLabel
    Left = 8
    Top = 112
    Width = 55
    Height = 13
    Caption = 'Distribution:'
    Visible = False
  end
  object ComboBoxDriver: TComboBox
    Left = 64
    Top = 7
    Width = 159
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = ComboBoxDriverChange
  end
  object ButtonControlPanel: TButton
    Left = 229
    Top = 7
    Width = 84
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = ButtonControlPanelClick
  end
  object ButtonStartStop: TButton
    Left = 229
    Top = 34
    Width = 84
    Height = 21
    Anchors = [akTop, akRight]
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 2
    OnClick = ButtonStartStopClick
  end
  object ScrollBarFreq: TScrollBar
    Left = 8
    Top = 128
    Width = 302
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    LargeChange = 1000
    Max = 100000
    PageSize = 0
    Position = 56633
    SmallChange = 10
    TabOrder = 3
    OnChange = ScrollBarFreqChange
  end
  object ScrollBarVolume: TScrollBar
    Left = 8
    Top = 85
    Width = 302
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 100000
    PageSize = 0
    Position = 100000
    TabOrder = 4
    OnChange = ScrollBarVolumeChange
  end
  object ComboBoxSignal: TComboBox
    Left = 64
    Top = 34
    Width = 159
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 0
    TabOrder = 5
    Text = 'Sine'
    OnChange = ComboBoxSignalChange
    Items.Strings = (
      'Sine'
      'White Noise'
      'Pink Noise')
  end
  object ComboBoxDistribution: TComboBox
    Left = 64
    Top = 109
    Width = 154
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    ItemIndex = 1
    TabOrder = 6
    Text = 'Triangular'
    Visible = False
    OnChange = ComboBoxDistributionChange
    Items.Strings = (
      'Rectangle'
      'Triangular'
      'FastGauss'
      'Gauss')
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 152
    Top = 16
  end
end
