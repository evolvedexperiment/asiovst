object FormAnalyser: TFormAnalyser
  Left = 287
  Top = 277
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
  DesignSize = (
    446
    326)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDriverName: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
    OnClick = LabelDriverNameClick
  end
  object LabelChannels: TLabel
    Left = 7
    Top = 36
    Width = 77
    Height = 13
    Caption = 'Output Channel:'
  end
  object LabelSpeed: TLabel
    Left = 7
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LabelFullScale: TLabel
    Left = 205
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Fullscale = '
  end
  object LabelFullScaleUnit: TLabel
    Left = 322
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object ComboBoxDriver: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = ComboBoxDriverChange
  end
  object ButtonControlPanel: TButton
    Left = 350
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = ButtonControlPanelClick
  end
  object ComboBoxChannel: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    TabOrder = 2
  end
  object ButtonAnalyse: TButton
    Left = 350
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = ButtonAnalyseClick
  end
  object RadioButtonFast: TRadioButton
    Left = 49
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 4
    OnClick = RadioButtonFastClick
  end
  object RadioButtonMedium: TRadioButton
    Left = 91
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RadioButtonMediumClick
  end
  object RadioButtonSlow: TRadioButton
    Left = 150
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 6
    OnClick = RadioButtonSlowClick
  end
  object SpinEditFullscaleGain: TSpinEdit
    Left = 262
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SpinEditFullscaleGainChange
  end
  object ChartAnalyser: TChart
    Left = 7
    Top = 88
    Width = 434
    Height = 231
    BackWall.Brush.Style = bsClear
    Title.Text.Strings = (
      'TChart')
    Title.Visible = False
    LeftAxis.Automatic = False
    LeftAxis.AutomaticMaximum = False
    LeftAxis.AutomaticMinimum = False
    LeftAxis.Maximum = 140.000000000000000000
    LeftAxis.Title.Caption = 'Magnitude [dB]'
    View3D = False
    View3DWalls = False
    TabOrder = 8
    Anchors = [akLeft, akTop, akRight, akBottom]
    DefaultCanvas = 'TGDIPlusCanvas'
    ColorPaletteIndex = 13
    object BarSeries: TBarSeries
      Legend.Visible = False
      Marks.Visible = False
      ShowInLegend = False
      MultiBar = mbNone
      XValues.Name = 'X'
      XValues.Order = loAscending
      YValues.Name = 'Balken'
      YValues.Order = loNone
    end
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    ConvertOptimizations = [coSSE]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = BSDownSampled
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 252
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerTimer
    Left = 280
    Top = 24
  end
end
