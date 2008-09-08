object EditorForm: TEditorForm
  Left = 220
  Top = 77
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 319
  ClientWidth = 337
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
  object GuiPanel2: TGuiPanel
    Left = 8
    Top = 143
    Width = 321
    Height = 168
    LineColor = clGray
    PanelColor = clBlack
  end
  object GuiPanel1: TGuiPanel
    Left = 8
    Top = 8
    Width = 321
    Height = 129
    LineColor = clGray
    PanelColor = clBlack
  end
  object PnKnee: TGuiPanel
    Left = 256
    Top = 110
    Width = 64
    Height = 17
    AntiAlias = gaaLinear4x
    Color = clBlack
    LineColor = 3355443
    PanelColor = 1118481
    Radius = 5
  end
  object PnInputValue: TGuiPanel
    Left = 16
    Top = 110
    Width = 64
    Height = 17
    AntiAlias = gaaLinear4x
    Color = clBlack
    LineColor = 3355443
    PanelColor = 1118481
    Radius = 5
  end
  object PnRatio: TGuiPanel
    Left = 176
    Top = 110
    Width = 64
    Height = 17
    AntiAlias = gaaLinear4x
    Color = clBlack
    LineColor = 3355443
    PanelColor = 1118481
    Radius = 5
  end
  object LbInputValue: TLabel
    Left = 19
    Top = 112
    Width = 58
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'dB'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LbRatioValue: TLabel
    Left = 179
    Top = 112
    Width = 58
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object LbKneeValue: TLabel
    Left = 259
    Top = 112
    Width = 58
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object DialInput: TGuiDial
    Left = 16
    Top = 40
    Width = 64
    Height = 64
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Inertia = 50.000000000000000000
    Min = -24.000000000000000000
    Max = 24.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialInputChange
  end
  object DialAttack: TGuiDial
    Left = 16
    Top = 178
    Width = 48
    Height = 48
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Inertia = 50.000000000000000000
    Min = -0.699000000953674300
    Max = 1.301030039787292000
    NumGlyphs = 31
    StitchKind = skVertical
    OnChange = DialAttackChange
  end
  object DialRelease: TGuiDial
    Left = 16
    Top = 232
    Width = 48
    Height = 48
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Position = 1.398000001907349000
    DefaultPosition = 1.398000001907349000
    Inertia = 50.000000000000000000
    Min = 1.000000000000000000
    Max = 2.698999881744385000
    NumGlyphs = 31
    StitchKind = skVertical
    OnChange = DialReleaseChange
  end
  object DialOutput: TGuiDial
    Left = 96
    Top = 40
    Width = 64
    Height = 64
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Inertia = 50.000000000000000000
    Min = -24.000000000000000000
    Max = 24.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialOutputChange
  end
  object LbInput: TGuiLabel
    Left = 16
    Top = 16
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
    Left = 96
    Top = 16
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
  object PnOutputValue: TGuiPanel
    Left = 96
    Top = 110
    Width = 64
    Height = 17
    AntiAlias = gaaLinear4x
    Color = clBlack
    LineColor = 3355443
    PanelColor = 1118481
    Radius = 5
  end
  object LbRelease: TGuiLabel
    Left = 16
    Top = 286
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
    Left = 16
    Top = 154
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
  object DialRatio: TGuiDial
    Left = 176
    Top = 40
    Width = 64
    Height = 64
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Inertia = 50.000000000000000000
    Max = 2.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialRatioChange
  end
  object LbRatioX: TGuiLabel
    Left = 176
    Top = 16
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
  object LbTitle: TGuiLabel
    Left = 157
    Top = 154
    Width = 64
    Height = 18
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'LA-4029'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14869218
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
  end
  object LbOutputValue: TLabel
    Left = 99
    Top = 112
    Width = 58
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'dB'
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clSilver
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
    ParentFont = False
  end
  object DialKnee: TGuiDial
    Left = 256
    Top = 40
    Width = 64
    Height = 64
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Inertia = 50.000000000000000000
    Max = 10.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialKneeChange
  end
  object LbKnee: TGuiLabel
    Left = 256
    Top = 14
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
  object LEDOnOff: TGuiLED
    Left = 77
    Top = 155
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
    Left = 94
    Top = 156
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
    Left = 41
    Top = 222
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
    Left = 16
    Top = 222
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
    Left = 80
    Top = 191
    Width = 168
    Height = 72
    Color = clBlack
    Position = 0
    NumGlyphs = 65
    StitchKind = skVertical
    PopupMenu = PopupVUMeterSpeed
  end
  object BtIn: TGuiButton
    Left = 80
    Top = 278
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
    Left = 138
    Top = 278
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
    Left = 196
    Top = 278
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
    Left = 95
    Top = 237
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
  object PnMix: TGuiPanel
    Left = 256
    Top = 286
    Width = 64
    Height = 17
    AntiAlias = gaaLinear4x
    LineColor = 3355443
    PanelColor = 1118481
    Radius = 5
  end
  object LbMixValue: TLabel
    Left = 259
    Top = 288
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
  object DialMix: TGuiDial
    Left = 256
    Top = 216
    Width = 64
    Height = 64
    Color = clBlack
    LineWidth = 2
    LineColor = clSilver
    CircleColor = 2105376
    Inertia = 50.000000000000000000
    Max = 100.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialMixChange
  end
  object LbMix: TGuiLabel
    Left = 256
    Top = 190
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
    Left = 227
    Top = 158
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
    Left = 80
    Top = 177
    Width = 241
    Height = 3
    Brush.Color = 3355443
  end
  object SpDivide2: TShape
    Left = 80
    Top = 179
    Width = 241
    Height = 3
    Brush.Color = 3355443
  end
  object VUMeterTimer: TTimer
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
