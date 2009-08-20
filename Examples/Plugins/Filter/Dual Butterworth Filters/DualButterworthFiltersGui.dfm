object FmLinkwitzRiley: TFmLinkwitzRiley
  Left = 396
  Top = 138
  BorderStyle = bsNone
  Caption = 'Linkwitz-Riley'
  ClientHeight = 170
  ClientWidth = 232
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GpLiknwitzRiley: TGuiGroup
    Left = 8
    Top = 9
    Width = 217
    Height = 154
    AntiAlias = gaaLinear4x
    Caption = 'Dual Linkwitz Riley Filters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    HeaderMinWidth = 64
    LineColor = 15659506
    LineWidth = 3
    ParentFont = False
    Radius = 7
    TabOrder = 0
    object DialLowpassFrequency: TGuiDial
      Left = 11
      Top = 25
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -2.099999904632568000
      DefaultPosition = 20.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      NumGlyphs = 65
      OnChange = DialLowpassFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object DialLowpassSlope: TGuiDial
      Left = 75
      Top = 25
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -1.000000000000000000
      DefaultPosition = 1.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      NumGlyphs = 65
      OnChange = DialLowpassSlopeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbFrequency: TGuiLabel
      Left = 11
      Top = 79
      Width = 48
      Height = 13
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbSlope: TGuiLabel
      Left = 75
      Top = 79
      Width = 48
      Height = 13
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Slope'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object DialHighpassFrequency: TGuiDial
      Left = 11
      Top = 98
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -2.099999904632568000
      DefaultPosition = 20.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      NumGlyphs = 65
      OnChange = DialHighpassFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object DialHighpassSlope: TGuiDial
      Left = 75
      Top = 98
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -1.000000000000000000
      DefaultPosition = 1.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      NumGlyphs = 65
      OnChange = DialHighpassSlopeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLowpass: TGuiLabel
      Left = 129
      Top = 41
      Width = 59
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Highcut'
      Color = 7373965
      Font.Charset = ANSI_CHARSET
      Font.Color = 15659506
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbHighpass: TGuiLabel
      Left = 129
      Top = 112
      Width = 59
      Height = 21
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Lowcut'
      Font.Charset = ANSI_CHARSET
      Font.Color = 15659506
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LedHighCut: TGuiLED
      Left = 189
      Top = 44
      Width = 15
      Height = 15
      Brightness_Percent = 90.000000000000000000
      Color = 7373965
      LineWidth = 2
      LEDColor = 14870505
      AntiAlias = gaaLinear4x
      LineColor = 3226174
      ParentColor = False
      OnClick = LedHighCutClick
    end
    object LedLowCut: TGuiLED
      Left = 189
      Top = 115
      Width = 15
      Height = 15
      Brightness_Percent = 90.000000000000000000
      Color = 7373965
      LineWidth = 2
      LEDColor = 14870505
      AntiAlias = gaaLinear4x
      LineColor = 3226174
      ParentColor = False
      OnClick = LedLowCutClick
    end
    object PnDisplay: TGuiPanel
      Left = 125
      Top = 77
      Width = 83
      Height = 17
      AntiAlias = gaaLinear4x
      Caption = 'PnDisplay'
      LineColor = 5398887
      Linewidth = 1
      PanelColor = 3226174
      ParentColor = True
      Radius = 5
      TabOrder = 0
      UseDockManager = True
      Transparent = True
      DesignSize = (
        83
        17)
      object LbDisplay: TGuiLabel
        Left = 3
        Top = 2
        Width = 76
        Height = 13
        Alignment = taCenter
        Anchors = [akLeft, akTop, akRight]
        AntiAlias = gaaLinear4x
        Caption = 'Value'
        Color = 3226174
        Font.Charset = ANSI_CHARSET
        Font.Color = 15659506
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
      end
    end
  end
  object GbFrequencyResponse: TGuiGroup
    Left = 8
    Top = 169
    Width = 217
    Height = 105
    AntiAlias = gaaLinear4x
    Caption = 'Frequency Response'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    HeaderMinWidth = 64
    LineColor = 15659506
    LineWidth = 3
    ParentFont = False
    Radius = 7
    TabOrder = 1
    Visible = False
    object GuiEQGraph: TGuiEQGraph
      Left = 7
      Top = 26
      Width = 187
      Height = 72
      AutoColor = False
      GraphColorLight = 5991539
      ColorChart = 3158064
      BorderRadius = 5
      MaxGain = 15.000000000000000000
      dBLabelStyle = dlsNone
      OnGetFilterGain = GuiEQGraphGetFilterGain
      Color = 7373965
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 3158064
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ParentColor = False
    end
  end
end
