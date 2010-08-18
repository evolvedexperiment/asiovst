object FmBassExtender: TFmBassExtender
  Left = 232
  Top = 148
  BorderStyle = bsNone
  Caption = 'Bass Extender'
  ClientHeight = 287
  ClientWidth = 543
  Color = 2372409
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
  PixelsPerInch = 96
  TextHeight = 13
  object PnMain: TGuiPanel
    Left = 6
    Top = 8
    Width = 529
    Height = 271
    AntiAlias = gaaLinear4x
    LineColor = 1450277
    Linewidth = 3
    PanelColor = 2372409
    ParentColor = True
    Radius = 13
    TabOrder = 0
    UseDockManager = True
    Transparent = True
    object GuiLabel2: TGuiLabel
      Left = 13
      Top = 7
      Width = 85
      Height = 23
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 1450277
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialFrequency: TGuiDial
      Left = 22
      Top = 35
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      CurveMapping = -0.250000000000000000
      DefaultPosition = 80.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 160.000000000000000000
      Min = 16.000000000000000000
      GlyphCount = 65
      OnChange = DialFrequencyChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 80.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbFrequency: TGuiLabel
      Left = 12
      Top = 6
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbFrequencyValue: TGuiLabel
      Left = 12
      Top = 105
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '100,34 Hz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbOrderValue: TGuiLabel
      Left = 116
      Top = 105
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '4'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialOrder: TGuiDial
      Left = 126
      Top = 35
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      CurveMapping = -0.400000005960464500
      DefaultPosition = 2.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 6.000000000000000000
      Min = 1.000000000000000000
      GlyphCount = 65
      OnChange = DialOrderChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 2.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbOrder: TGuiLabel
      Left = 116
      Top = 6
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbDivideValue: TGuiLabel
      Left = 220
      Top = 105
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialDivide: TGuiDial
      Left = 230
      Top = 35
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      DefaultPosition = 50.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100.000000000000000000
      GlyphCount = 65
      OnChange = DialDivideChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbDivide: TGuiLabel
      Left = 220
      Top = 6
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Divide'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbShapeValue: TGuiLabel
      Left = 324
      Top = 105
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialShape: TGuiDial
      Left = 334
      Top = 35
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      DefaultPosition = 50.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100.000000000000000000
      GlyphCount = 65
      OnChange = DialShapeChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbShape: TGuiLabel
      Left = 324
      Top = 6
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Shape'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbBalanceValue: TGuiLabel
      Left = 428
      Top = 105
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '0%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialBalance: TGuiDial
      Left = 438
      Top = 35
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      GlyphCount = 65
      OnChange = DialBalanceChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbBalance: TGuiLabel
      Left = 428
      Top = 6
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Balance'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialThreshold: TGuiDial
      Left = 22
      Top = 173
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100.000000000000000000
      Min = -80.000000000000000000
      GlyphCount = 65
      OnChange = DialThresholdChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbThreshold: TGuiLabel
      Left = 12
      Top = 144
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbThresholdValue: TGuiLabel
      Left = 12
      Top = 243
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '-40dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbRatioValue: TGuiLabel
      Left = 116
      Top = 243
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 : 6'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialRatio: TGuiDial
      Left = 126
      Top = 173
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      CurveMapping = -1.799999952316284000
      DefaultPosition = 10.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      GlyphCount = 65
      OnChange = DialRatioChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbRatio: TGuiLabel
      Left = 116
      Top = 144
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Ratio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbAttackValue: TGuiLabel
      Left = 220
      Top = 243
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialAttack: TGuiDial
      Left = 230
      Top = 173
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      CurveMapping = -2.200000047683716000
      DefaultPosition = 100.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100000.000000000000000000
      Min = 10.000000000000000000
      GlyphCount = 65
      OnChange = DialAttackChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbAttack: TGuiLabel
      Left = 220
      Top = 144
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbReleaseValue: TGuiLabel
      Left = 324
      Top = 243
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '200ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialRelease: TGuiDial
      Left = 334
      Top = 173
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      CurveMapping = -1.100000023841858000
      DefaultPosition = 100.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 800.000000000000000000
      Min = 20.000000000000000000
      GlyphCount = 65
      OnChange = DialReleaseChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 200.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbRelease: TGuiLabel
      Left = 324
      Top = 144
      Width = 85
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbCompressionValue: TGuiLabel
      Left = 428
      Top = 243
      Width = 85
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '50%'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialCompression: TGuiDial
      Left = 438
      Top = 173
      Width = 64
      Height = 64
      CircleColor = 3163213
      Color = 2372409
      DefaultPosition = 50.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 1450277
      LineWidth = 2
      Max = 100.000000000000000000
      GlyphCount = 65
      OnChange = DialCompressionChange
      ParentColor = False
      PointerAngles.Start = 220
      PointerAngles.Range = 280
      PointerAngles.Resolution = 280.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompression: TGuiLabel
      Left = 418
      Top = 144
      Width = 105
      Height = 23
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Compression'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 6857659
      Font.Height = -19
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 24
    Top = 40
  end
end
