object FmUniQuE: TFmUniQuE
  Left = 428
  Top = 268
  BorderStyle = bsNone
  Caption = 'UniQuE GUI'
  ClientHeight = 154
  ClientWidth = 360
  Color = 5329233
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 15263976
  Font.Height = -16
  Font.Name = 'Trebuchet MS'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 22
  object GpUnique: TGuiGroup
    Left = 8
    Top = 8
    Width = 345
    Height = 137
    AntiAlias = gaaLinear4x
    Caption = 'UNIQUE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5329233
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    HeaderMinWidth = 80
    LineColor = 15790320
    LineWidth = 8
    ParentFont = False
    Radius = 9
    TabOrder = 0
    object DialLow: TGuiDial
      Left = 11
      Top = 34
      Width = 75
      Height = 75
      DialImageIndex = -1
      LineColor = 15790320
      LineWidth = 2
      Max = 15.000000000000000000
      Min = -15.000000000000000000
      NumGlyphs = 65
      OnChange = DialLowChange
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
    end
    object DialMid: TGuiDial
      Left = 92
      Top = 34
      Width = 75
      Height = 75
      DialImageIndex = -1
      LineColor = 15790320
      LineWidth = 2
      Max = 15.000000000000000000
      Min = -15.000000000000000000
      NumGlyphs = 65
      OnChange = DialMidChange
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
    end
    object DialPresence: TGuiDial
      Left = 173
      Top = 34
      Width = 75
      Height = 75
      DialImageIndex = -1
      LineColor = 15790320
      LineWidth = 2
      Max = 15.000000000000000000
      Min = -15.000000000000000000
      NumGlyphs = 65
      OnChange = DialPresenceChange
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
    end
    object DialHigh: TGuiDial
      Left = 254
      Top = 34
      Width = 75
      Height = 75
      DialImageIndex = -1
      LineColor = 15790320
      LineWidth = 2
      Max = 15.000000000000000000
      Min = -15.000000000000000000
      NumGlyphs = 65
      OnChange = DialHighChange
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
    end
    object LEDOnOff: TGuiLED
      Left = 93
      Top = 10
      Width = 17
      Height = 18
      Brightness_Percent = 100.000000000000000000
      LineWidth = 2
      LEDColor = clLime
      AntiAlias = gaaLinear4x
      LineColor = 5329233
      OnClick = OnOffClick
    end
    object LbOnOff: TGuiLabel
      Left = 116
      Top = 8
      Width = 48
      Height = 20
      AntiAlias = gaaLinear8x
      Caption = 'on/off'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      OnClick = OnOffClick
    end
    object LbPad: TGuiLabel
      Left = 204
      Top = 8
      Width = 27
      Height = 20
      AntiAlias = gaaLinear8x
      Caption = 'pad'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      OnClick = PadClick
    end
    object LEDPad: TGuiLED
      Left = 181
      Top = 10
      Width = 17
      Height = 18
      Brightness_Percent = 100.000000000000000000
      LineWidth = 2
      LEDColor = clYellow
      AntiAlias = gaaLinear4x
      LineColor = 5329233
      OnClick = PadClick
    end
    object LbInvert: TGuiLabel
      Left = 277
      Top = 8
      Width = 52
      Height = 20
      AntiAlias = gaaLinear8x
      Caption = 'invert'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      OnClick = InvertClick
    end
    object LEDInvert: TGuiLED
      Left = 254
      Top = 10
      Width = 17
      Height = 18
      Brightness_Percent = 100.000000000000000000
      LineWidth = 2
      LEDColor = 16735092
      AntiAlias = gaaLinear4x
      LineColor = 5329233
      OnClick = InvertClick
    end
    object LbLow: TGuiLabel
      Left = 11
      Top = 103
      Width = 75
      Height = 22
      Alignment = taCenter
      AntiAlias = gaaLinear8x
      Caption = 'Low'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbMid: TGuiLabel
      Left = 92
      Top = 103
      Width = 75
      Height = 22
      Alignment = taCenter
      AntiAlias = gaaLinear8x
      Caption = 'Mid'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbPRes: TGuiLabel
      Left = 173
      Top = 103
      Width = 75
      Height = 22
      Alignment = taCenter
      AntiAlias = gaaLinear8x
      Caption = 'Pres'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbHigh: TGuiLabel
      Left = 254
      Top = 103
      Width = 75
      Height = 22
      Alignment = taCenter
      AntiAlias = gaaLinear8x
      Caption = 'High'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -16
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
  end
end
