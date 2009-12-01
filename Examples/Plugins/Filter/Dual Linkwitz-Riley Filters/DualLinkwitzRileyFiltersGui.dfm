object FmLinkwitzRiley: TFmLinkwitzRiley
  Left = 396
  Top = 138
  BorderStyle = bsNone
  Caption = 'Dual Linkwitz-Riley Filters'
  ClientHeight = 235
  ClientWidth = 236
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
  object LbShowFrequencyPlot: TGuiLabel
    Left = 8
    Top = 183
    Width = 220
    Height = 29
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Show Frequency Plot'
    Font.Charset = ANSI_CHARSET
    Font.Color = 15659506
    Font.Height = -24
    Font.Name = 'Times New Roman'
    Font.Style = []
    Visible = False
    Transparent = True
    OnClick = LbShowFrequencyPlotClick
  end
  object GpDualLiknwitzRiley: TGuiGroup
    Left = 8
    Top = 8
    Width = 220
    Height = 161
    AntiAlias = gaaLinear4x
    Caption = 'Dual Linkwitz-Riley Filters'
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
      Top = 29
      Width = 48
      Height = 48
      PopupMenu = PuFrequency
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
      OnMouseEnter = DialLowpassFrequencyMouseEnter
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
      Top = 29
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
      OnMouseEnter = DialLowpassSlopeMouseEnter
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
      Top = 83
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
      Top = 83
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
      Top = 102
      Width = 48
      Height = 48
      PopupMenu = PuFrequency
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
      OnMouseEnter = DialHighpassFrequencyMouseEnter
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
      Top = 102
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
      OnMouseEnter = DialHighpassSlopeMouseEnter
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLowpass: TGuiLabel
      Left = 131
      Top = 45
      Width = 59
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Highcut'
      Font.Charset = ANSI_CHARSET
      Font.Color = 15659506
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = []
      PopupMenu = PuPreset
      OnClick = LedHighCutClick
      OnMouseDown = LbMouseDown
    end
    object LbHighpass: TGuiLabel
      Left = 131
      Top = 116
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
      PopupMenu = PuPreset
      OnClick = LedLowCutClick
      OnMouseDown = LbMouseDown
    end
    object LedHighCut: TGuiLED
      Left = 191
      Top = 47
      Width = 15
      Height = 15
      Color = 7373965
      LineWidth = 2
      LEDColor = 14870505
      Brightness_Percent = 90.000000000000000000
      Uniformity_Percent = 36.754447937011720000
      AntiAlias = gaaLinear4x
      LineColor = 3226174
      ParentColor = False
      OnClick = LedHighCutClick
    end
    object LedLowCut: TGuiLED
      Left = 191
      Top = 118
      Width = 15
      Height = 15
      Color = 7373965
      LineWidth = 2
      LEDColor = 14870505
      Brightness_Percent = 90.000000000000000000
      Uniformity_Percent = 36.754447937011720000
      AntiAlias = gaaLinear4x
      LineColor = 3226174
      ParentColor = False
      OnClick = LedLowCutClick
    end
    object PnDisplay: TGuiPanel
      Left = 127
      Top = 81
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
  object GuiEQGraph: TGuiEQGraph
    Left = 8
    Top = 175
    Width = 220
    Height = 53
    AntiAlias = gaaLinear4x
    GraphColorDark = 5991539
    GraphColorLight = 6978950
    ColorChart = 7373965
    BorderRadius = 7
    BorderWidth = 2
    BorderColor = 15659506
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = 15659506
        OnGetFilterGain = GuiEQGraphGetFilterGain
      end>
    YAxis.LowerLevel = -15.000000000000000000
    YAxis.UpperLevel = 3.000000000000000000
    YAxis.Granularity = 2.000000000000000000
    XAxis.LabelStyle = xlsBottom
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    Color = 7373965
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 15659506
    Font.Height = -7
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    OnClick = GuiEQGraphClick
    ParentColor = False
  end
  object PuFrequency: TPopupMenu
    OnPopup = PuFrequencyPopup
    Left = 64
    Top = 80
    object Mi16Hz: TMenuItem
      Tag = 16
      Caption = '16 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi20Hz: TMenuItem
      Tag = 20
      Caption = '20 Hz'
      OnClick = MiFrequencyClick
    end
    object N251: TMenuItem
      Tag = 25
      Caption = '25 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi31Hz5: TMenuItem
      Tag = 32
      Caption = '31.5 Hz'
      OnClick = Mi31Hz5Click
    end
    object Mi40Hz: TMenuItem
      Tag = 40
      Caption = '40 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi50Hz: TMenuItem
      Tag = 50
      Caption = '50 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi63Hz: TMenuItem
      Tag = 63
      Caption = '63 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi80Hz: TMenuItem
      Tag = 80
      Caption = '80 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi100Hz: TMenuItem
      Tag = 100
      Caption = '100 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi125Hz: TMenuItem
      Tag = 125
      Caption = '125 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi160Hz: TMenuItem
      Tag = 160
      Caption = '160 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi200Hz: TMenuItem
      Tag = 200
      Caption = '200 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi250Hz: TMenuItem
      Tag = 250
      Caption = '250 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi315Hz: TMenuItem
      Tag = 315
      Caption = '315 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi400Hz: TMenuItem
      Tag = 400
      Caption = '400 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi500Hz: TMenuItem
      Tag = 500
      Caption = '500 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi630Hz: TMenuItem
      Tag = 630
      Caption = '630 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi800Hz: TMenuItem
      Tag = 800
      Caption = '800 Hz'
      OnClick = MiFrequencyClick
    end
    object Mi1kHz: TMenuItem
      Tag = 1000
      Caption = '1 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi1k25Hz: TMenuItem
      Tag = 1250
      Caption = '1.25 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi1k6Hz: TMenuItem
      Tag = 1600
      Caption = '1.6 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi2kHz: TMenuItem
      Tag = 2000
      Caption = '2 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi2k5Hz: TMenuItem
      Tag = 2500
      Caption = '2.5 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi31k5Hz: TMenuItem
      Tag = 3150
      Caption = '3.15 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi4kHz: TMenuItem
      Tag = 4000
      Caption = '4 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi5kHz: TMenuItem
      Tag = 5000
      Caption = '5 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi6k3Hz: TMenuItem
      Tag = 6300
      Caption = '6.3 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi8kHz: TMenuItem
      Tag = 8000
      Caption = '8 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi10kHz: TMenuItem
      Tag = 10000
      Caption = '10 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi12k5Hz: TMenuItem
      Tag = 12500
      Caption = '12.5 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi16kHz: TMenuItem
      Tag = 16000
      Caption = '16 kHz'
      OnClick = MiFrequencyClick
    end
    object Mi20kHz: TMenuItem
      Tag = 20000
      Caption = '20 kHz'
      OnClick = MiFrequencyClick
    end
  end
  object PuPreset: TPopupMenu
    OnPopup = PuPresetPopup
    Left = 160
    Top = 80
    object MiLoadHigh: TMenuItem
      Caption = '&Load'
      object MiLoadA: TMenuItem
        Caption = '&A'
        OnClick = MiLoadClick
      end
      object MiLoadB: TMenuItem
        Tag = 1
        Caption = '&B'
        OnClick = MiLoadClick
      end
      object MiLoadC: TMenuItem
        Tag = 2
        Caption = '&C'
        OnClick = MiLoadClick
      end
      object MiLoadD: TMenuItem
        Tag = 3
        Caption = '&D'
        OnClick = MiLoadClick
      end
      object MiLoadE: TMenuItem
        Tag = 4
        Caption = '&E'
        OnClick = MiLoadClick
      end
      object MiLoadF: TMenuItem
        Tag = 5
        Caption = '&F'
        OnClick = MiLoadClick
      end
    end
    object MiStoreHigh: TMenuItem
      Caption = 'Store'
      object MiStoreA: TMenuItem
        Caption = '&A'
        OnClick = MiStoreClick
      end
      object MiStoreB: TMenuItem
        Tag = 1
        Caption = '&B'
        OnClick = MiStoreClick
      end
      object MiStoreC: TMenuItem
        Tag = 2
        Caption = '&C'
        OnClick = MiStoreClick
      end
      object MiStoreD: TMenuItem
        Tag = 3
        Caption = '&D'
        OnClick = MiStoreClick
      end
      object MiStoreE: TMenuItem
        Tag = 4
        Caption = '&E'
        OnClick = MiStoreClick
      end
      object MiStoreF: TMenuItem
        Tag = 5
        Caption = '&F'
        OnClick = MiStoreClick
      end
    end
  end
  object EQGraphUpdate: TTimer
    Interval = 10
    OnTimer = EQGraphUpdateTimer
    Left = 88
    Top = 184
  end
end
