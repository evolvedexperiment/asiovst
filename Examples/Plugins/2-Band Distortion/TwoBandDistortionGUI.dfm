object FmTwoBandDistortion: TFmTwoBandDistortion
  Left = 392
  Top = 195
  BorderStyle = bsNone
  Caption = '2-band distortion'
  ClientHeight = 129
  ClientWidth = 319
  Color = 4144959
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
  DesignSize = (
    319
    129)
  PixelsPerInch = 96
  TextHeight = 13
  object PnControl: TGuiPanel
    Left = 8
    Top = 8
    Width = 303
    Height = 113
    Anchors = [akLeft, akTop, akRight]
    Caption = 'PnControl'
    LineColor = clWhite
    BorderWidth = 2.000000000000000000
    PanelColor = 6908265
    ParentColor = True
    Radius = 13.000000000000000000
    TabOrder = 0
    UseDockManager = True
    Transparent = True
    object DialFreq: TGuiDial
      Left = 16
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      CurveMapping = -2.099999904632568000
      DefaultPosition = 20.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 116222
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      GlyphCount = 65
      OnChange = DialFreqChange
      ParentColor = False
      Position = 100.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbFreq: TGuiLabel
      Left = 16
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbFreqValue: TGuiLabel
      Left = 16
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1kHz'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialLowDist: TGuiDial
      Left = 156
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 116222
      LineWidth = 2
      Max = 100.000000000000000000
      GlyphCount = 65
      OnChange = DialLowDistChange
      ParentColor = False
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLowDist: TGuiLabel
      Left = 156
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Low Dist.'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbLowDistValue: TGuiLabel
      Left = 156
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbHighDist: TGuiLabel
      Left = 226
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'High Dist.'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialHighDist: TGuiDial
      Left = 226
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 116222
      LineWidth = 2
      Max = 100.000000000000000000
      GlyphCount = 65
      OnChange = DialHighDistChange
      ParentColor = False
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbHighDistValue: TGuiLabel
      Left = 226
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialOrder: TGuiDial
      Left = 86
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      DefaultPosition = 16.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 116222
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 2.000000000000000000
      GlyphCount = 65
      OnChange = DialOrderChange
      ParentColor = False
      Position = 2.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbOrderValue: TGuiLabel
      Left = 86
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '2'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbOrder: TGuiLabel
      Left = 86
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 16
    Top = 24
  end
end
