object FmLA1701: TFmLA1701
  Left = 220
  Top = 77
  BorderStyle = bsNone
  Caption = 'LA-1701'
  ClientHeight = 310
  ClientWidth = 338
  Color = 4210752
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PnB: TGuiPanel
    Left = 8
    Top = 131
    Width = 321
    Height = 170
    LineColor = clGray
    Linewidth = 1
    PanelColor = clBlack
    ParentColor = True
    Radius = 4
    TabOrder = 0
    Transparent = True
    UseDockManager = True
    object DialAttack: TGuiDial
      Left = 8
      Top = 34
      Width = 48
      Height = 48
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Inertia = 0.100000001490116100
      Min = -0.699000000953674300
      Max = 1.301030039787292000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialAttackChange
    end
    object DialRelease: TGuiDial
      Left = 8
      Top = 88
      Width = 48
      Height = 48
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Position = 1.398000001907349000
      DefaultPosition = 1.398000001907349000
      Inertia = 0.100000001490116100
      Min = 1.000000000000000000
      Max = 2.698999881744385000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialReleaseChange
    end
    object LbRelease: TGuiLabel
      Left = 8
      Top = 142
      Width = 48
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Release'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbAttack: TGuiLabel
      Left = 8
      Top = 10
      Width = 48
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Attack'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbTitle: TGuiLabel
      Left = 149
      Top = 10
      Width = 64
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'LA-1701'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = [fsBold]
      OnClick = LbTitleClick
    end
    object LEDOnOff: TGuiLED
      Left = 69
      Top = 11
      Width = 16
      Height = 16
      OnClick = LEDOnOffClick
      Brightness_Percent = 100.000000000000000000
      Color = clBlack
      LineWidth = 2
      LEDColor = clRed
      AntiAlias = gaaLinear4x
      LineColor = clRed
    end
    object LbOnOff: TGuiLabel
      Left = 86
      Top = 12
      Width = 44
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'On/Off'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = []
      OnClick = LEDOnOffClick
    end
    object LbSlow: TGuiLabel
      Left = 33
      Top = 78
      Width = 22
      Height = 12
      Alignment = taCenter
      AntiAlias = gaaLinear2x
      Caption = 'slow'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
    end
    object LbFast: TGuiLabel
      Left = 8
      Top = 78
      Width = 19
      Height = 12
      Alignment = taCenter
      AntiAlias = gaaLinear2x
      Caption = 'fast'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
    end
    object VUMeter: TGuiVUMeter
      Left = 72
      Top = 47
      Width = 168
      Height = 72
      Color = clBlack
      Position = 0
      NumGlyphs = 65
      StitchKind = skHorizontal
      PopupMenu = PopupVUMeterSpeed
    end
    object BtIn: TGuiButton
      Left = 72
      Top = 134
      Width = 52
      Height = 25
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'In'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      LineColor = clSilver
      LineWidth = 2
      ButtonColor = 2105376
      Radius = 5
      OnClick = BtInClick
    end
    object BtGR: TGuiButton
      Left = 130
      Top = 134
      Width = 52
      Height = 25
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'GR'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      LineColor = 3355443
      LineWidth = 2
      ButtonColor = clBlack
      Radius = 5
      OnClick = BtGRClick
    end
    object BtOut: TGuiButton
      Left = 188
      Top = 134
      Width = 52
      Height = 25
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'Out'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      LineColor = 3355443
      LineWidth = 2
      ButtonColor = clBlack
      Radius = 5
      OnClick = BtOutClick
    end
    object LbVUMeterDisplay: TLabel
      Left = 87
      Top = 93
      Width = 105
      Height = 12
      Alignment = taCenter
      AutoSize = False
      Caption = 'Input'
      Color = 15461355
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -9
      Font.Name = 'Verdana'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = True
    end
    object LbMix: TGuiLabel
      Left = 248
      Top = 46
      Width = 64
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Mix'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbLevelingAmplifier: TLabel
      Left = 219
      Top = 14
      Width = 90
      Height = 12
      Caption = 'Leveling Amplifier'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clGray
      Font.Height = -9
      Font.Name = 'Verdana'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      Transparent = True
    end
    object SpDivide1: TShape
      Left = 72
      Top = 33
      Width = 241
      Height = 3
      Brush.Color = 3355443
    end
    object SpDivide2: TShape
      Left = 72
      Top = 35
      Width = 241
      Height = 3
      Brush.Color = 3355443
    end
    object DialMix: TGuiDial
      Left = 248
      Top = 72
      Width = 64
      Height = 64
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Inertia = 0.100000001490116100
      Max = 100.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialMixChange
    end
    object PnMix: TGuiPanel
      Left = 248
      Top = 142
      Width = 64
      Height = 17
      AntiAlias = gaaLinear4x
      LineColor = 3355443
      PanelColor = 1118481
      ParentColor = True
      Radius = 5
      TabOrder = 0
      UseDockManager = True
      object LbMixValue: TLabel
        Left = 3
        Top = 2
        Width = 58
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Color = 1118481
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
  end
  object PnA: TGuiPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 117
    LineColor = clGray
    Linewidth = 1
    PanelColor = clBlack
    ParentColor = True
    Radius = 4
    TabOrder = 1
    Transparent = True
    UseDockManager = True
    object DialInput: TGuiDial
      Left = 8
      Top = 28
      Width = 64
      Height = 64
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Inertia = 0.100000001490116100
      Min = -24.000000000000000000
      Max = 24.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialInputChange
    end
    object DialOutput: TGuiDial
      Left = 88
      Top = 28
      Width = 64
      Height = 64
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Inertia = 0.100000001490116100
      Min = -24.000000000000000000
      Max = 24.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialOutputChange
    end
    object LbInput: TGuiLabel
      Left = 8
      Top = 8
      Width = 64
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Input'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbOutput: TGuiLabel
      Left = 88
      Top = 8
      Width = 64
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Output'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object DialRatio: TGuiDial
      Left = 168
      Top = 28
      Width = 64
      Height = 64
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Inertia = 0.100000001490116100
      Max = 2.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialRatioChange
    end
    object LbRatioX: TGuiLabel
      Left = 168
      Top = 8
      Width = 64
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Ratio'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object DialKnee: TGuiDial
      Left = 248
      Top = 28
      Width = 64
      Height = 64
      Color = clBlack
      LineWidth = 2
      LineColor = clSilver
      CircleColor = 2105376
      Inertia = 0.100000001490116100
      Max = 10.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      OnChange = DialKneeChange
    end
    object LbKnee: TGuiLabel
      Left = 248
      Top = 6
      Width = 64
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Knee'
      Color = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14869218
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object PnKnee: TGuiPanel
      Left = 248
      Top = 90
      Width = 64
      Height = 17
      AntiAlias = gaaLinear4x
      Color = clBlack
      LineColor = 3355443
      PanelColor = 1118481
      Radius = 5
      TabOrder = 0
      UseDockManager = True
      object LbKneeValue: TLabel
        Left = 3
        Top = 2
        Width = 58
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Color = 1118481
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object PnInputValue: TGuiPanel
      Left = 8
      Top = 90
      Width = 64
      Height = 17
      AntiAlias = gaaLinear4x
      Color = clBlack
      LineColor = 3355443
      PanelColor = 1118481
      Radius = 5
      TabOrder = 1
      UseDockManager = True
      object LbInputValue: TLabel
        Left = 3
        Top = 2
        Width = 58
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'dB'
        Color = 1118481
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object PnRatio: TGuiPanel
      Left = 168
      Top = 90
      Width = 64
      Height = 17
      AntiAlias = gaaLinear4x
      Color = clBlack
      LineColor = 3355443
      PanelColor = 1118481
      Radius = 5
      TabOrder = 2
      UseDockManager = True
      object LbRatioValue: TLabel
        Left = 3
        Top = 2
        Width = 58
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Color = 1118481
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
    object PnOutputValue: TGuiPanel
      Left = 88
      Top = 90
      Width = 64
      Height = 17
      AntiAlias = gaaLinear4x
      Color = clBlack
      LineColor = 3355443
      PanelColor = 1118481
      Radius = 5
      TabOrder = 3
      UseDockManager = True
      object LbOutputValue: TLabel
        Left = 3
        Top = 2
        Width = 58
        Height = 13
        Alignment = taCenter
        AutoSize = False
        Caption = 'dB'
        Color = 1118481
        Font.Charset = DEFAULT_CHARSET
        Font.Color = clSilver
        Font.Height = -11
        Font.Name = 'Tahoma'
        Font.Style = []
        ParentColor = False
        ParentFont = False
      end
    end
  end
  object Timer1: TTimer
    Interval = 30
    OnTimer = VUMeterTimerTimer
    Left = 264
    Top = 56
  end
  object PopupVUMeterSpeed: TPopupMenu
    OnPopup = PopupVUMeterSpeedPopup
    Left = 95
    Top = 206
    object MIFast: TMenuItem
      Caption = '&Fast'
      RadioItem = True
      OnClick = MIFastClick
    end
    object MIMedium: TMenuItem
      Caption = '&Medium'
      Checked = True
      RadioItem = True
      OnClick = MIMediumClick
    end
    object MISlow: TMenuItem
      Caption = '&Slow'
      RadioItem = True
      OnClick = MISlowClick
    end
  end
end