object FmThirdOctaveAnalyser: TFmThirdOctaveAnalyser
  Left = 286
  Top = 81
  BorderStyle = bsNone
  Caption = 'Third-Octave Analyser'
  ClientHeight = 234
  ClientWidth = 342
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    342
    234)
  PixelsPerInch = 96
  TextHeight = 13
  object LbSpeed: TLabel
    Left = 8
    Top = 8
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LbFullscale: TLabel
    Left = 205
    Top = 8
    Width = 54
    Height = 13
    Caption = 'Fullscale = '
  end
  object Lb_dB: TLabel
    Left = 322
    Top = 8
    Width = 12
    Height = 13
    Caption = 'dB'
  end
  object RB_Fast: TRadioButton
    Left = 50
    Top = 7
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 0
    OnClick = RB_FastClick
  end
  object RB_Medium: TRadioButton
    Left = 92
    Top = 7
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 1
    TabStop = True
    OnClick = RB_MediumClick
  end
  object RB_Slow: TRadioButton
    Left = 151
    Top = 7
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 2
    OnClick = RB_SlowClick
  end
  object SEFullscaleGain: TSpinEdit
    Left = 262
    Top = 4
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 3
    Value = 0
    OnChange = SEFullscaleGainChange
  end
  object AnalyserChart: TChart
    Left = 8
    Top = 29
    Width = 326
    Height = 197
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
  object Timer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerTimer
    Left = 24
    Top = 24
  end
end
