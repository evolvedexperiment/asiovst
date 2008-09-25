object FmCrossoverDistortion: TFmCrossoverDistortion
  Left = 392
  Top = 195
  BorderStyle = bsNone
  Caption = 'FmCrossoverDistortion'
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
    AntiAlias = gaaLinear4x
    Caption = 'PnControl'
    LineColor = clWhite
    PanelColor = 6908265
    ParentColor = True
    Radius = 13
    TabOrder = 0
    Transparent = True
    UseDockManager = True
    object DialFreq: TGuiDial
      Left = 16
      Top = 24
      Width = 64
      Height = 64
      Color = 7039851
      LineColor = 116222
      CurveMapping = -2.099999904632568000
      DefaultPosition = 20.000000000000000000
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      NumGlyphs = 65
      Position = 100.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialFreqChange
    end
    object LbFreq: TGuiLabel
      Left = 16
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbFreqValue: TGuiLabel
      Left = 16
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1kHz'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object DialLowDist: TGuiDial
      Left = 156
      Top = 24
      Width = 64
      Height = 64
      Color = 7039851
      LineColor = 116222
      Max = 100.000000000000000000
      NumGlyphs = 65
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialLowDistChange
    end
    object LbLowDist: TGuiLabel
      Left = 156
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Low Dist.'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbLowDistValue: TGuiLabel
      Left = 156
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbHighDist: TGuiLabel
      Left = 226
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'High Dist.'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object DialHighDist: TGuiDial
      Left = 226
      Top = 24
      Width = 64
      Height = 64
      Color = 7039851
      LineColor = 116222
      Max = 100.000000000000000000
      NumGlyphs = 65
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialHighDistChange
    end
    object LbHighDistValue: TGuiLabel
      Left = 226
      Top = 88
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object DialOrder: TGuiDial
      Left = 86
      Top = 24
      Width = 64
      Height = 64
      Color = 7039851
      LineColor = 116222
      DefaultPosition = 16.000000000000000000
      Max = 16.000000000000000000
      Min = 2.000000000000000000
      NumGlyphs = 65
      Position = 2.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialOrderChange
    end
    object LbOrderValue: TGuiLabel
      Left = 86
      Top = 86
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '2'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbOrder: TGuiLabel
      Left = 86
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Color = 7039851
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
  end
end
