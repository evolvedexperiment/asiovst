object FmChunkDemo: TFmChunkDemo
  Left = 218
  Top = 77
  BorderStyle = bsNone
  Caption = 'Chunk Demo'
  ClientHeight = 123
  ClientWidth = 285
  Color = 13359821
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 3747121
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DialAlpha: TGuiDial
    Left = 8
    Top = 62
    Width = 56
    Height = 56
    CircleColor = 13359821
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 3747121
    LineWidth = 2
    Max = 100.000000000000000000
    NumGlyphs = 65
    OnChange = DialAlphaChange
    Position = 50.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbAlpha: TGuiLabel
    Left = 8
    Top = 9
    Width = 56
    Height = 40
    Alignment = taCenter
    AntiAlias = gaaLinear3x
    Caption = 'a'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3747121
    Font.Height = -37
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
  end
  object LbBeta: TGuiLabel
    Left = 79
    Top = 62
    Width = 56
    Height = 47
    Alignment = taCenter
    AntiAlias = gaaLinear3x
    Caption = 'b'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3747121
    Font.Height = -37
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
  end
  object LbGamma: TGuiLabel
    Left = 150
    Top = 9
    Width = 56
    Height = 47
    Alignment = taCenter
    AntiAlias = gaaLinear3x
    Caption = 'g'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3747121
    Font.Height = -37
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
  end
  object LbDelta: TGuiLabel
    Left = 221
    Top = 62
    Width = 56
    Height = 47
    Alignment = taCenter
    AntiAlias = gaaLinear3x
    Caption = 'd'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 3747121
    Font.Height = -37
    Font.Name = 'Symbol'
    Font.Style = [fsBold]
  end
  object DialBeta: TGuiDial
    Left = 79
    Top = 8
    Width = 56
    Height = 56
    CircleColor = 13359821
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 3747121
    LineWidth = 2
    Max = 100.000000000000000000
    NumGlyphs = 65
    OnChange = DialBetaChange
    Position = 50.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialGamma: TGuiDial
    Left = 150
    Top = 62
    Width = 56
    Height = 56
    CircleColor = 13359821
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 3747121
    LineWidth = 2
    Max = 100.000000000000000000
    NumGlyphs = 65
    OnChange = DialGammaChange
    Position = 50.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialDelta: TGuiDial
    Left = 221
    Top = 8
    Width = 56
    Height = 56
    CircleColor = 13359821
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 3747121
    LineWidth = 2
    Max = 100.000000000000000000
    NumGlyphs = 65
    OnChange = DialDeltaChange
    Position = 50.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 16
    Top = 8
  end
end
