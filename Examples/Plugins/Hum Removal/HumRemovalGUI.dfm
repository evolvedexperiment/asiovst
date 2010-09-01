object FmHumRemoval: TFmHumRemoval
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'Hum Removal'
  ClientHeight = 350
  ClientWidth = 396
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 11580859
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GuiEQGraph: TGuiEQGraph
    Left = 8
    Top = 8
    Width = 379
    Height = 169
    AntiAlias = gaaLinear4x
    BorderColor = 14277598
    BorderRadius = 7
    BorderWidth = 2
    ColorChart = clBlack
    FilterSeries = <
      item
        DisplayName = 'Mono'
        Color = 14277598
        OnGetFilterGain = GuiEQGraphGetFilterGain
      end>
    GraphColorDark = 7699334
    GraphColorLight = 2829873
    XAxis.LabelPosition = xlpBottom
    XAxis.UpperFrequency = 1600.000000000000000000
    XAxis.LowerFrequency = 4.000000000000000000
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -36.000000000000000000
    YAxis.UpperLevel = 6.000000000000000000
    YAxis.Granularity = 20.000000000000000000
    YAxis.MaximumGridLines = 4
    Color = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7699334
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
  end
  object GbHighpass: TGuiGroup
    Left = 8
    Top = 183
    Width = 172
    Height = 160
    AntiAlias = gaaLinear4x
    Caption = 'Highpass'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    OutlineWidth = 2
    PanelColor = clBlack
    ParentFont = False
    Radius = 8
    TabOrder = 1
    object LbHighpassFrequency: TGuiLabel
      Left = 10
      Top = 63
      Width = 85
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialHighpassFrequency: TGuiDial
      Left = 35
      Top = 87
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      DefaultPosition = 20.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 12632777
      LineWidth = 2
      Max = 200.000000000000000000
      Min = 2.000000000000000000
      GlyphCount = 65
      OnChange = DialHighpassFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 20.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LedHighpassActive: TGuiLED
      Left = 10
      Top = 38
      Width = 16
      Height = 16
      BorderStrength_Percent = 80.000000000000000000
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      LineWidth = 1.299999952316284000
      Uniformity_Percent = 40.000000000000000000
      Transparent = False
      OnClick = LedHighpassActiveClick
    end
    object SbHighpassType: TGuiSelectBox
      Left = 32
      Top = 35
      Width = 113
      Height = 22
      AntiAlias = gaaLinear4x
      ArrowColor = 14606306
      ButtonColor = clBlack
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = -1
      Items.Strings = (
        'Butterworth'
        'Chebyshev I'
        'Chebyshev II')
      LineColor = 12632777
      ParentFont = False
      SelectBoxColor = clBlack
      OnChange = SbHighpassTypeChange
    end
    object LbHighpassOrder: TGuiLabel
      Left = 101
      Top = 63
      Width = 51
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialHighpassOrder: TGuiDial
      Left = 109
      Top = 87
      Width = 36
      Height = 36
      CurveMapping = -1.200000047683716000
      DefaultPosition = 4.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 12632777
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      GlyphCount = 65
      OnChange = DialHighpassOrderChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 4.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbHighpassFrequencyValue: TGuiLabel
      Left = 18
      Top = 129
      Width = 69
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbHighpassOrderValue: TGuiLabel
      Left = 97
      Top = 129
      Width = 59
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
  end
  object GbNotchFilters: TGuiGroup
    Left = 187
    Top = 182
    Width = 201
    Height = 160
    AntiAlias = gaaLinear4x
    Caption = 'Notch Filters'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    OutlineWidth = 2
    PanelColor = clBlack
    ParentFont = False
    Radius = 8
    TabOrder = 2
    object LbFundamentalFrequency: TGuiLabel
      Left = 10
      Top = 63
      Width = 85
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialFundamentalFrequency: TGuiDial
      Left = 35
      Top = 87
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 50.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 12632777
      LineWidth = 2
      Max = 120.000000000000000000
      Min = 40.000000000000000000
      GlyphCount = 65
      OnChange = DialFundamentalFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbNotchBandwidth: TGuiLabel
      Left = 106
      Top = 63
      Width = 84
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Bandwidth'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialNotchBandwidth: TGuiDial
      Left = 130
      Top = 87
      Width = 36
      Height = 36
      CurveMapping = -1.200000047683716000
      DefaultPosition = 0.079999998211860660
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 12632777
      LineWidth = 2
      Max = 0.300000011920929000
      Min = 0.029999999329447750
      GlyphCount = 65
      OnChange = DialNotchBandwidthChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 0.079999998211860660
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbFundamentalFrequencyValue: TGuiLabel
      Left = 10
      Top = 129
      Width = 85
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbNotchBandwidthValue: TGuiLabel
      Left = 106
      Top = 129
      Width = 84
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LedHumProfile: TGuiLED
      Left = 10
      Top = 38
      Width = 16
      Height = 16
      BorderStrength_Percent = 80.000000000000000000
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      LineWidth = 1.299999952316284000
      Uniformity_Percent = 40.000000000000000000
      Transparent = False
      OnClick = LedHumProfileClick
    end
    object LbCaptureHumProfile: TGuiLabel
      Left = 32
      Top = 34
      Width = 158
      Height = 22
      AntiAlias = gaaLinear4x
      Caption = 'Capture Hum Profile'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -17
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = clBlack
      OnClick = LedHumProfileClick
    end
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 111
    Top = 240
  end
end
