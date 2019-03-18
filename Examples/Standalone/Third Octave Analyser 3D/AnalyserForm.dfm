object FmAnalyser: TFmAnalyser
  Left = 328
  Top = 278
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple ASIO Third Octave Analyser'
  ClientHeight = 326
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbDrivername: TLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
    OnClick = LbDrivernameClick
  end
  object LbChannels: TLabel
    Left = 8
    Top = 36
    Width = 69
    Height = 13
    Caption = 'Input Channel:'
  end
  object LbSpeed: TLabel
    Left = 8
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LbFullscale: TLabel
    Left = 206
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Fullscale = '
  end
  object LbFullScaleUnit: TLabel
    Left = 323
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object CbDriver: TComboBox
    Left = 50
    Top = 7
    Width = 288
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = CbDriverChange
  end
  object BtControlPanel: TButton
    Left = 347
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = BtControlPanelClick
  end
  object CbChannel: TComboBox
    Left = 92
    Top = 32
    Width = 246
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object BtAnalyse: TButton
    Left = 347
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = BtAnalyseClick
  end
  object RbFast: TRadioButton
    Left = 50
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 4
    OnClick = RbFastClick
  end
  object RbMedium: TRadioButton
    Left = 92
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RbMediumClick
  end
  object RbSlow: TRadioButton
    Left = 151
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 6
    OnClick = RbSlowClick
  end
  object SeFullscaleGain: TSpinEdit
    Left = 263
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SeFullscaleGainChange
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreFillOutBuffer = bpfZero
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = BSDownSampled
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 253
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerTimer
    Left = 281
    Top = 24
  end
end
