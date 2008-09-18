object FmExciter: TFmExciter
  Left = 392
  Top = 203
  BorderStyle = bsNone
  Caption = 'FmExciter'
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
    object DialTune: TGuiDial
      Left = 16
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      LineWidth = 2
      LineColor = 116222
      CurveMapping = -1.200000047683716000
      Position = 4000.000000000000000000
      DefaultPosition = 1000.000000000000000000
      Min = 1000.000000000000000000
      Max = 16000.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialTuneChange
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
    end
    object DialShape: TGuiDial
      Left = 156
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      LineWidth = 2
      LineColor = 116222
      Position = 50.000000000000000000
      Max = 100.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialShapeChange
    end
    object LbShape: TGuiLabel
      Left = 156
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Shape'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object LbShapeValue: TGuiLabel
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
    end
    object LbMix: TGuiLabel
      Left = 226
      Top = 6
      Width = 64
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Mix'
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
    object DialMix: TGuiDial
      Left = 226
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      LineWidth = 2
      LineColor = 116222
      Position = 50.000000000000000000
      Max = 100.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialMixChange
    end
    object LbMixValue: TGuiLabel
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
    end
    object DialOrder: TGuiDial
      Left = 86
      Top = 24
      Width = 64
      Height = 64
      Color = 6908265
      LineWidth = 2
      LineColor = 116222
      CurveMapping = -0.660000026226043700
      Position = 4.000000000000000000
      DefaultPosition = 8.000000000000000000
      Min = 2.000000000000000000
      Max = 8.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 400.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
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
      Color = 6908265
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
      Color = 6908265
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Trebuchet MS'
      Font.Style = []
    end
  end
end
