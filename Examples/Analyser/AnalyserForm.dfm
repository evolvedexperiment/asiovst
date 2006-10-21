object FmAnalyser: TFmAnalyser
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 305
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
  DesignSize = (
    446
    305)
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
  object Bt_Play: TButton
    Left = 350
    Top = 32
    Width = 91
    Height = 21
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = Bt_PlayClick
  end
  object AnalyserChart: TChart
    Left = 8
    Top = 59
    Width = 432
    Height = 239
    BackWall.Brush.Color = clWhite
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
    TabOrder = 4
    Anchors = [akLeft, akTop, akRight, akBottom]
    ExplicitWidth = 465
    ExplicitHeight = 254
    object BarSeries: TBarSeries
      Marks.ArrowLength = 20
      Marks.Visible = False
      SeriesColor = clRed
      ShowInLegend = False
      MultiBar = mbNone
      XValues.DateTime = False
      XValues.Name = 'X'
      XValues.Multiplier = 1.000000000000000000
      XValues.Order = loAscending
      YValues.DateTime = False
      YValues.Name = 'Balken'
      YValues.Multiplier = 1.000000000000000000
      YValues.Order = loNone
    end
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
    OnBufferSwitch = ASIOHostBufferSwitch
    Left = 308
    Top = 32
  end
end
