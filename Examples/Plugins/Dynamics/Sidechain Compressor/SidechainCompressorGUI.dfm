object FmSidechainCompressor: TFmSidechainCompressor
  Left = 385
  Top = 243
  BorderStyle = bsNone
  Caption = 'Sidechain Compressor'
  ClientHeight = 318
  ClientWidth = 449
  Color = 5196083
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 9681911
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    449
    318)
  PixelsPerInch = 96
  TextHeight = 13
  object LbAttack: TGuiLabel
    Left = 8
    Top = 164
    Width = 55
    Height = 13
    Caption = 'Attack:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbRelease: TGuiLabel
    Left = 8
    Top = 183
    Width = 55
    Height = 13
    Caption = 'Release:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbThreshold: TGuiLabel
    Left = 8
    Top = 202
    Width = 55
    Height = 13
    Caption = 'Threshold:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbKnee: TGuiLabel
    Left = 8
    Top = 221
    Width = 55
    Height = 13
    Caption = 'Knee:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbRatio: TGuiLabel
    Left = 8
    Top = 240
    Width = 55
    Height = 13
    Caption = 'Ratio:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbAttackValue: TGuiLabel
    Left = 215
    Top = 164
    Width = 55
    Height = 13
    Caption = 'Attack'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbReleaseValue: TGuiLabel
    Left = 215
    Top = 183
    Width = 55
    Height = 13
    Caption = 'Release'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbThresholdValue: TGuiLabel
    Left = 215
    Top = 202
    Width = 55
    Height = 13
    Caption = 'Threshold'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbKneeValue: TGuiLabel
    Left = 215
    Top = 221
    Width = 55
    Height = 13
    Caption = 'Knee'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbRatioValue: TGuiLabel
    Left = 215
    Top = 240
    Width = 55
    Height = 13
    Caption = 'Ratio'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbSidechainVstPlugin: TGuiLabel
    Left = 27
    Top = 41
    Width = 102
    Height = 14
    Caption = 'Sidechain VST Plugin:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbMakupGain: TGuiLabel
    Left = 8
    Top = 259
    Width = 55
    Height = 13
    Caption = 'Make-Up:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbMakeUpGainValue: TGuiLabel
    Left = 215
    Top = 259
    Width = 55
    Height = 13
    Caption = 'Make-Up'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbAutoMakeUpGain: TGuiLabel
    Left = 27
    Top = 297
    Width = 94
    Height = 13
    Caption = 'Auto Make-Up Gain'
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnClick = LEDAutoGainClick
  end
  object LEDAutoGain: TGuiLED
    Left = 8
    Top = 297
    Width = 13
    Height = 13
    LEDColor = 9681911
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 50.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    AntiAlias = gaaLinear4x
    LineColor = 9681911
    OnClick = LEDAutoGainClick
  end
  object LbSoftClip: TGuiLabel
    Left = 156
    Top = 297
    Width = 45
    Height = 13
    Caption = 'Soft Clip'
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnClick = LEDSoftClipClick
  end
  object LEDSoftClip: TGuiLED
    Left = 137
    Top = 297
    Width = 13
    Height = 13
    LEDColor = 9681911
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 50.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    AntiAlias = gaaLinear4x
    LineColor = 9681911
    OnClick = LEDSoftClipClick
  end
  object LbStereo: TGuiLabel
    Left = 234
    Top = 297
    Width = 35
    Height = 13
    Caption = 'Stereo'
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnClick = LEDStereoClick
  end
  object LEDStereo: TGuiLED
    Left = 215
    Top = 297
    Width = 13
    Height = 13
    LEDColor = 9681911
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 50.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    AntiAlias = gaaLinear4x
    LineColor = 9681911
    OnClick = LEDStereoClick
  end
  object GuiGraphXY: TGuiGraphXY
    Left = 276
    Top = 145
    Width = 165
    Height = 165
    BorderColor = 9681911
    FrameColor = 6322591
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = 9681911
      end>
    XAxis.Granularity = 20.000000000000000000
    XAxis.Minimum = -66.000000000000000000
    XAxis.Maximum = 6.000000000000000000
    XAxis.Lower = -66.000000000000000000
    XAxis.Upper = 6.000000000000000000
    YAxis.Granularity = 20.000000000000000000
    YAxis.Minimum = -66.000000000000000000
    YAxis.Maximum = 6.000000000000000000
    YAxis.Lower = -66.000000000000000000
    YAxis.Upper = 6.000000000000000000
    Color = 4341035
    LineColor = 6322591
    LineWidth = 2
  end
  object LbHold: TGuiLabel
    Left = 8
    Top = 145
    Width = 63
    Height = 13
    Caption = 'Attack Delay:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbHoldValue: TGuiLabel
    Left = 215
    Top = 146
    Width = 55
    Height = 13
    Caption = 'Hold'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbLowcutFrequency: TGuiLabel
    Left = 8
    Top = 69
    Width = 74
    Height = 13
    Caption = 'Lowcut Freq.:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbHighcutFrequency: TGuiLabel
    Left = 8
    Top = 107
    Width = 70
    Height = 13
    Caption = 'Highcut Freq.:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbHighcutOrder: TGuiLabel
    Left = 8
    Top = 126
    Width = 70
    Height = 13
    Caption = 'Highcut Slope:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbLowcutFrequencyValue: TGuiLabel
    Left = 215
    Top = 69
    Width = 55
    Height = 13
    Caption = 'Frequency'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbHighcutFrequencyValue: TGuiLabel
    Left = 215
    Top = 107
    Width = 55
    Height = 13
    Caption = 'Frequency'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbHighcutOrderValue: TGuiLabel
    Left = 215
    Top = 126
    Width = 55
    Height = 13
    Caption = 'Order'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbLowcutOrder: TGuiLabel
    Left = 8
    Top = 88
    Width = 74
    Height = 13
    Caption = 'Lowcut Slope:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbLowcutOrderValue: TGuiLabel
    Left = 215
    Top = 88
    Width = 55
    Height = 13
    Caption = 'Order'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbMix: TGuiLabel
    Left = 8
    Top = 278
    Width = 55
    Height = 13
    Caption = 'Mix:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbMixValue: TGuiLabel
    Left = 215
    Top = 278
    Width = 55
    Height = 13
    Caption = 'Mix'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object GuiLabel1: TGuiLabel
    Left = 406
    Top = 41
    Width = 35
    Height = 14
    Alignment = taCenter
    Anchors = [akLeft, akTop, akRight]
    Caption = '[clear]'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9681911
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnDblClick = LbClearDblClick
  end
  object LEDSideChain: TGuiLED
    Left = 8
    Top = 41
    Width = 13
    Height = 13
    LEDColor = 9681911
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 50.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    AntiAlias = gaaLinear4x
    LineColor = 9681911
    OnClick = LEDAutoGainClick
  end
  object PnTitle: TPanel
    Left = 0
    Top = 0
    Width = 449
    Height = 27
    Align = alTop
    BevelOuter = bvNone
    Color = 3222816
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9681911
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentFont = False
    TabOrder = 0
    DesignSize = (
      449
      27)
    object LbTitle: TGuiLabel
      Left = 104
      Top = 1
      Width = 237
      Height = 25
      Anchors = [akTop]
      AntiAlias = gaaLinear4x
      Caption = 'Sidechain Compressor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 9681911
      Font.Height = -19
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Transparent = True
      Shadow.Color = clBlack
      Shadow.Visible = True
    end
  end
  object SliderAttack: TGuiSlider
    Left = 69
    Top = 165
    Width = 140
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    CurveMapping = 2.000000000000000000
    DefaultPosition = 10.000000000000000000
    Max = 1000.000000000000000000
    Min = 0.009999999776482582
    Position = 10.000000000000000000
    SlideColor = 6322591
    OnChange = SliderAttackChange
    Color = 5196083
    ParentColor = False
  end
  object SliderRelease: TGuiSlider
    Left = 69
    Top = 184
    Width = 140
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    CurveMapping = 2.000000000000000000
    DefaultPosition = 50.000000000000000000
    Max = 5000.000000000000000000
    Min = 0.050000000745058060
    Position = 50.000000000000000000
    SlideColor = 6322591
    OnChange = SliderReleaseChange
    Color = 5196083
    ParentColor = False
  end
  object SliderThreshold: TGuiSlider
    Left = 69
    Top = 203
    Width = 140
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    DefaultPosition = -20.000000000000000000
    Max = 10.000000000000000000
    Min = -90.000000000000000000
    Position = -20.000000000000000000
    SlideColor = 6322591
    OnChange = SliderThresholdChange
    Color = 5196083
    ParentColor = False
  end
  object SliderKnee: TGuiSlider
    Left = 69
    Top = 222
    Width = 140
    Height = 11
    Hint = '100'
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    DefaultPosition = 1.000000000000000000
    Max = 10.000000000000000000
    Position = 1.000000000000000000
    SlideColor = 6322591
    OnChange = SliderKneeChange
    Color = 5196083
    ParentColor = False
  end
  object SliderRatio: TGuiSlider
    Left = 69
    Top = 241
    Width = 140
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    CurveMapping = 1.000000000000000000
    DefaultPosition = 50.000000000000000000
    Max = 100.000000000000000000
    Min = 1.000000000000000000
    Position = 10.000000000000000000
    SlideColor = 6322591
    OnChange = SliderRatioChange
    Color = 5196083
    ParentColor = False
  end
  object SliderMakeUpGain: TGuiSlider
    Left = 69
    Top = 260
    Width = 140
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    DefaultPosition = 50.000000000000000000
    Max = 90.000000000000000000
    Min = -10.000000000000000000
    Position = 10.000000000000000000
    SlideColor = 6322591
    OnChange = SliderMakeUpGainChange
    Color = 5196083
    ParentColor = False
  end
  object SliderHold: TGuiSlider
    Left = 88
    Top = 146
    Width = 121
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    CurveMapping = 2.000000000000000000
    DefaultPosition = 10.000000000000000000
    Max = 1000.000000000000000000
    Min = 0.009999999776482582
    Position = 10.000000000000000000
    SlideColor = 6322591
    OnChange = SliderHoldChange
    Color = 5196083
    ParentColor = False
  end
  object SliderLowcutFrequency: TGuiSlider
    Left = 88
    Top = 70
    Width = 121
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    CurveMapping = 2.000000000000000000
    DefaultPosition = 10.000000000000000000
    Max = 20000.000000000000000000
    Min = 20.000000000000000000
    Position = 20.000000000000000000
    SlideColor = 6322591
    OnChange = SliderLowcutFrequencyChange
    Color = 5196083
    ParentColor = False
  end
  object SliderHighcutFrequency: TGuiSlider
    Left = 88
    Top = 108
    Width = 121
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    CurveMapping = 2.000000000000000000
    DefaultPosition = 50.000000000000000000
    Max = 20000.000000000000000000
    Min = 20.000000000000000000
    Position = 20000.000000000000000000
    SlideColor = 6322591
    OnChange = SliderHighcutFrequencyChange
    Color = 5196083
    ParentColor = False
  end
  object SliderHighcutOrder: TGuiSlider
    Left = 88
    Top = 127
    Width = 121
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    DefaultPosition = 16.000000000000000000
    Max = 16.000000000000000000
    Position = 2.000000000000000000
    SlideColor = 6322591
    OnChange = SliderHighcutOrderChange
    Color = 5196083
    ParentColor = False
  end
  object SliderLowcutOrder: TGuiSlider
    Left = 88
    Top = 89
    Width = 121
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    DefaultPosition = 16.000000000000000000
    Max = 16.000000000000000000
    Position = 2.000000000000000000
    SlideColor = 6322591
    OnChange = SliderLowcutOrderChange
    Color = 5196083
    ParentColor = False
  end
  object GuiEQGraph: TGuiEQGraph
    Left = 276
    Top = 70
    Width = 165
    Height = 69
    BorderColor = 9681911
    BorderRadius = 2
    ColorChart = 4341035
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = 9681911
        OnGetFilterGain = GetFilterGain
      end>
    GraphColorDark = 6322591
    GraphColorLight = 5989478
    XAxis.LabelPosition = xlpBottom
    XAxis.UnitPosition = upSide
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -33.000000000000000000
    YAxis.UpperLevel = 3.000000000000000000
    YAxis.Granularity = 10.000000000000000000
    YAxis.MaximumGridLines = 4
    YAxis.UnitPosition = upSide
    Color = 5196083
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 9681911
    Font.Height = -9
    Font.Name = 'Tahoma'
    Font.Style = []
    ParentColor = False
  end
  object SliderMix: TGuiSlider
    Left = 69
    Top = 279
    Width = 140
    Height = 11
    AntiAlias = gaaLinear3x
    BorderColor = 9681911
    BorderRadius = 2
    DefaultPosition = 50.000000000000000000
    Max = 100.000000000000000000
    Position = 10.000000000000000000
    SlideColor = 6322591
    OnChange = SliderMixChange
    Color = 5196083
    ParentColor = False
  end
  object GuiPanel1: TGuiPanel
    Left = 135
    Top = 38
    Width = 265
    Height = 20
    AntiAlias = gaaLinear3x
    Caption = 'GuiPanel1'
    Color = 5196083
    LineColor = 9681911
    Linewidth = 1
    PanelColor = 4341035
    TabOrder = 14
    UseDockManager = True
    DesignSize = (
      265
      20)
    object LbVstPluginValue: TGuiLabel
      Left = 1
      Top = 3
      Width = 263
      Height = 14
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      Caption = '(double click to load)'
      Color = 4341035
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 9681911
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsItalic]
      ParentFont = False
      Shadow.Color = clBlack
      Shadow.Visible = True
      OnClick = LbVstPluginValueClick
      OnDblClick = LbVstPluginValueDblClick
    end
  end
end
