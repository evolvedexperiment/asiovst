object FmUniQuE: TFmUniQuE
  Left = 428
  Top = 268
  BorderStyle = bsNone
  Caption = 'UniQuE GUI'
  ClientHeight = 152
  ClientWidth = 370
  Color = 5329233
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 15263976
  Font.Height = -16
  Font.Name = 'Trebuchet MS'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 22
  object GpUnique: TGuiGroup
    Left = 8
    Top = 8
    Width = 353
    Height = 137
    AntiAlias = gaaLinear4x
    Caption = 'UniQuE'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5329233
    Font.Height = -16
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    HeaderMinWidth = 64
    LineColor = 15790320
    LineWidth = 8
    Radius = 9
  end
  object DialLow: TGuiDial
    Left = 24
    Top = 43
    Width = 75
    Height = 75
    LineWidth = 2
    LineColor = 15790320
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialLowChange
  end
  object DialMid: TGuiDial
    Left = 105
    Top = 43
    Width = 75
    Height = 75
    LineWidth = 2
    LineColor = 15790320
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialMidChange
  end
  object DialPresence: TGuiDial
    Left = 186
    Top = 43
    Width = 75
    Height = 75
    LineWidth = 2
    LineColor = 15790320
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialPresenceChange
  end
  object DialHigh: TGuiDial
    Left = 267
    Top = 43
    Width = 75
    Height = 75
    LineWidth = 2
    LineColor = 15790320
    Min = -15.000000000000000000
    Max = 15.000000000000000000
    NumGlyphs = 65
    StitchKind = skVertical
    OnChange = DialHighChange
  end
  object LEDOnOff: TGuiLED
    Left = 106
    Top = 19
    Width = 17
    Height = 18
    OnClick = OnOffClick
    Brightness_Percent = 100.000000000000000000
    LineWidth = 2
    LEDColor = clLime
    AntiAlias = gaaLinear4x
    LineColor = 5329233
  end
  object LbOnOff: TGuiLabel
    Left = 129
    Top = 17
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
    Left = 217
    Top = 17
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
    Left = 194
    Top = 19
    Width = 17
    Height = 18
    OnClick = PadClick
    Brightness_Percent = 100.000000000000000000
    LineWidth = 2
    LEDColor = clYellow
    AntiAlias = gaaLinear4x
    LineColor = 5329233
  end
  object LbInvert: TGuiLabel
    Left = 290
    Top = 17
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
    Left = 267
    Top = 19
    Width = 17
    Height = 18
    OnClick = InvertClick
    Brightness_Percent = 100.000000000000000000
    LineWidth = 2
    LEDColor = 16735092
    AntiAlias = gaaLinear4x
    LineColor = 5329233
  end
  object LbLow: TGuiLabel
    Left = 24
    Top = 112
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
    Left = 105
    Top = 112
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
    Left = 186
    Top = 112
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
    Left = 267
    Top = 112
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
