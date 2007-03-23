object FmAnalyser: TFmAnalyser
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple ASIO Third Octave Analyser'
  ClientHeight = 328
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Lb_Drivername: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object Lb_Channels: TLabel
    Left = 7
    Top = 36
    Width = 77
    Height = 13
    Caption = 'Output Channel:'
  end
  object LbSpeed: TLabel
    Left = 7
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LbFullscale: TLabel
    Left = 205
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Fullscale = '
  end
  object Lb_dB: TLabel
    Left = 322
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object BarChart: TFrequencyBarChart
    Left = 0
    Top = 88
    Width = 446
    Height = 240
    Align = alBottom
    MagnitudeUpper = 90.000000000000000000
  end
  object DriverCombo: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object Bt_CP: TButton
    Left = 350
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = Bt_CPClick
  end
  object ChannelBox: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object Bt_Analyse: TButton
    Left = 350
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = Bt_AnalyseClick
  end
  object RB_Fast: TRadioButton
    Left = 49
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 4
    OnClick = RB_FastClick
  end
  object RB_Medium: TRadioButton
    Left = 91
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RB_MediumClick
  end
  object RB_Slow: TRadioButton
    Left = 150
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 6
    OnClick = RB_SlowClick
  end
  object SEFullscaleGain: TSpinEdit
    Left = 262
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 7
    Value = 120
    OnChange = SEFullscaleGainChange
  end
  object ASIOHost: TASIOHost
    CanDos = []
    PreventClipping = pcDigital
    PreFillOutBuffer = bpfZero
    ConvertOptimizations = [coSSE]
    SampleRate = 44100.000000000000000000
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    OnBufferSwitch32 = BSDownSampled
    Left = 252
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 40
    OnTimer = TimerTimer
    Left = 280
    Top = 24
  end
end
