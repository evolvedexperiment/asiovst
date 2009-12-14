object FmSidechainCompressor: TFmSidechainCompressor
  Left = 580
  Top = 329
  BorderStyle = bsNone
  Caption = 'Sidechain Compressor'
  ClientHeight = 205
  ClientWidth = 411
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
  PixelsPerInch = 96
  TextHeight = 13
  object LbAttack: TGuiLabel
    Left = 8
    Top = 73
    Width = 55
    Height = 13
    Caption = 'Attack:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbRelease: TGuiLabel
    Left = 8
    Top = 92
    Width = 55
    Height = 13
    Caption = 'Release:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbThreshold: TGuiLabel
    Left = 8
    Top = 111
    Width = 55
    Height = 13
    Caption = 'Threshold:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbKnee: TGuiLabel
    Left = 8
    Top = 130
    Width = 55
    Height = 13
    Caption = 'Knee:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbRatio: TGuiLabel
    Left = 8
    Top = 149
    Width = 55
    Height = 13
    Caption = 'Ratio:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbAttackValue: TGuiLabel
    Left = 215
    Top = 73
    Width = 55
    Height = 13
    Caption = 'Attack'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbReleaseValue: TGuiLabel
    Left = 215
    Top = 92
    Width = 55
    Height = 13
    Caption = 'Release'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbThresholdValue: TGuiLabel
    Left = 215
    Top = 111
    Width = 55
    Height = 13
    Caption = 'Threshold'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbKneeValue: TGuiLabel
    Left = 215
    Top = 130
    Width = 55
    Height = 13
    Caption = 'Knee'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbRatioValue: TGuiLabel
    Left = 215
    Top = 149
    Width = 55
    Height = 13
    Caption = 'Ratio'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbSidechainVstPlugin: TGuiLabel
    Left = 8
    Top = 41
    Width = 105
    Height = 14
    Caption = 'Sidechain VST Plugin:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbVstPluginValue: TGuiLabel
    Left = 119
    Top = 41
    Width = 284
    Height = 14
    Alignment = taCenter
    Caption = '(double click to load)'
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
  object LbMakupGain: TGuiLabel
    Left = 8
    Top = 168
    Width = 55
    Height = 13
    Caption = 'Make-Up:'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbMakeUpGainValue: TGuiLabel
    Left = 215
    Top = 168
    Width = 55
    Height = 13
    Caption = 'Make-Up'
    Shadow.Color = clBlack
    Shadow.Visible = True
  end
  object LbAutoMakeUpGain: TGuiLabel
    Left = 27
    Top = 187
    Width = 94
    Height = 13
    Caption = 'Auto Make-Up Gain'
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnClick = LEDAutoGainClick
  end
  object LEDAutoGain: TGuiLED
    Left = 8
    Top = 187
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
    Top = 187
    Width = 45
    Height = 13
    Caption = 'Soft Clip'
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnClick = LEDSoftClipClick
  end
  object LEDSoftClip: TGuiLED
    Left = 137
    Top = 186
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
    Top = 187
    Width = 35
    Height = 13
    Caption = 'Stereo'
    Shadow.Color = clBlack
    Shadow.Visible = True
    OnClick = LEDStereoClick
  end
  object LEDStereo: TGuiLED
    Left = 215
    Top = 187
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
    Top = 73
    Width = 127
    Height = 127
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
    LineColor = 6322591
    LineWidth = 2
  end
  object PnTitle: TPanel
    Left = 0
    Top = 0
    Width = 411
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
      411
      27)
    object LbTitle: TGuiLabel
      Left = 100
      Top = 1
      Width = 209
      Height = 25
      Anchors = [akTop]
      AntiAlias = gaaLinear4x
      Caption = 'Sidechain Dynamics'
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
    Top = 74
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
    Top = 93
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
    Top = 112
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
    Top = 131
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
    Top = 150
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
    Top = 169
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
end
