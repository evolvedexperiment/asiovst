object FmLightweightDynamics: TFmLightweightDynamics
  Left = 452
  Top = 86
  BorderStyle = bsNone
  Caption = 'Lightweight Dynamics'
  ClientHeight = 422
  ClientWidth = 473
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 14277598
  Font.Height = -19
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 23
  object GbGate: TGuiGroup
    Left = 8
    Top = 8
    Width = 457
    Height = 93
    AntiAlias = gaaLinear4x
    Caption = 'Gate'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    LineWidth = 2
    ParentFont = False
    Radius = 5
    TabOrder = 0
    object DialGateAttack: TGuiDial
      Left = 67
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      GlyphCount = 65
      OnChange = DialGateAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbGateAttack: TGuiLabel
      Left = 58
      Top = 8
      Width = 52
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbGateAttackValue: TGuiLabel
      Left = 48
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialGateRelease: TGuiDial
      Left = 147
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 50.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 5000.000000000000000000
      Min = 0.100000001490116100
      GlyphCount = 65
      OnChange = DialGateReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbGateRelease: TGuiLabel
      Left = 132
      Top = 8
      Width = 68
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbGateReleaseValue: TGuiLabel
      Left = 126
      Top = 66
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialGateThreshold: TGuiDial
      Left = 227
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      DefaultPosition = -15.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      Min = -90.000000000000000000
      GlyphCount = 65
      OnChange = DialGateThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbGateThreshold: TGuiLabel
      Left = 203
      Top = 8
      Width = 90
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbGateThresholdValue: TGuiLabel
      Left = 208
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialGateRatio: TGuiDial
      Left = 307
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      DefaultPosition = 6.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      GlyphCount = 65
      OnChange = DialGateRatioChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbGateRatio: TGuiLabel
      Left = 299
      Top = 8
      Width = 50
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Ratio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbGateRatioValue: TGuiLabel
      Left = 286
      Top = 66
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialGateKnee: TGuiDial
      Left = 387
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      GlyphCount = 65
      OnChange = DialGateKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbGateKnee: TGuiLabel
      Left = 363
      Top = 8
      Width = 82
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Soft Knee'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbGateKneeValue: TGuiLabel
      Left = 368
      Top = 66
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LmLeft: TGuiColorLevelMeter
      Left = 9
      Top = 32
      Width = 13
      Height = 36
      Visible = False
      BorderColor = 14277598
      ContrastLuminance = 0.219999998807907100
      Upper = 1.100000023841858000
    end
    object LmRight: TGuiColorLevelMeter
      Left = 28
      Top = 32
      Width = 13
      Height = 36
      Visible = False
      BorderColor = 14277598
      ContrastLuminance = 0.219999998807907100
      Upper = 1.100000023841858000
    end
    object LbInputGainLeft: TGuiLabel
      Left = 9
      Top = 70
      Width = 13
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'L'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
    object LbInputGainRight: TGuiLabel
      Left = 28
      Top = 70
      Width = 13
      Height = 17
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'R'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      Visible = False
    end
  end
  object GbCompressor: TGuiGroup
    Left = 8
    Top = 107
    Width = 273
    Height = 208
    AntiAlias = gaaLinear4x
    Caption = 'Compressor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    LineWidth = 2
    ParentFont = False
    Radius = 5
    TabOrder = 1
    object DialCompressorThreshold: TGuiDial
      Left = 190
      Top = 60
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      DefaultPosition = -15.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      Min = -90.000000000000000000
      GlyphCount = 65
      OnChange = DialCompressorThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompressorThreshold: TGuiLabel
      Left = 166
      Top = 36
      Width = 90
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbCompressorThresholdValue: TGuiLabel
      Left = 171
      Top = 94
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialCompressorAttack: TGuiDial
      Left = 27
      Top = 60
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      GlyphCount = 65
      OnChange = DialCompressorAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompressorAttack: TGuiLabel
      Left = 18
      Top = 36
      Width = 52
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbCompressorAttackValue: TGuiLabel
      Left = 8
      Top = 94
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialCompressorRelease: TGuiDial
      Left = 107
      Top = 60
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 50.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 5000.000000000000000000
      Min = 0.100000001490116100
      GlyphCount = 65
      OnChange = DialCompressorReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompressorRelease: TGuiLabel
      Left = 92
      Top = 35
      Width = 68
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbCompressorReleaseValue: TGuiLabel
      Left = 86
      Top = 94
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialCompressorRatio: TGuiDial
      Left = 27
      Top = 146
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      DefaultPosition = 6.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 100.000000000000000000
      Min = 1.000000000000000000
      GlyphCount = 65
      OnChange = DialCompressorRatioChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompressorRatio: TGuiLabel
      Left = 19
      Top = 122
      Width = 50
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Ratio'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbCompressorRatioValue: TGuiLabel
      Left = 6
      Top = 180
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialCompressorKnee: TGuiDial
      Left = 107
      Top = 146
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      GlyphCount = 65
      OnChange = DialCompressorKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompressorKnee: TGuiLabel
      Left = 83
      Top = 122
      Width = 82
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Soft Knee'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbCompressorKneeValue: TGuiLabel
      Left = 88
      Top = 180
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialCompressorMakeUpGain: TGuiDial
      Left = 190
      Top = 146
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 10.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 60.000000000000000000
      GlyphCount = 65
      OnChange = DialCompressorMakeUpGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbCompressorMakeUpGain: TGuiLabel
      Left = 173
      Top = 122
      Width = 72
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Make Up'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbCompressorMakeUpGainValue: TGuiLabel
      Left = 171
      Top = 180
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LEDAutoGain: TGuiLED
      Left = 110
      Top = 7
      Width = 15
      Height = 15
      LEDColor = 14277598
      Brightness_Percent = 10.000000000000000000
      Uniformity_Percent = 40.000000000000000000
      AntiAlias = gaaLinear3x
      LineColor = 14277598
      OnClick = LEDAutoGainClick
    end
    object LbAutomaticMakeupGain: TGuiLabel
      Left = 127
      Top = 8
      Width = 136
      Height = 13
      AntiAlias = gaaLinear4x
      Caption = 'Automatic Make-Up Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = LEDAutoGainClick
    end
  end
  object GbCharacteristics: TGuiGroup
    Left = 287
    Top = 107
    Width = 178
    Height = 208
    AntiAlias = gaaLinear4x
    Caption = 'Characteristic Plot'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    LineWidth = 2
    ParentFont = False
    Radius = 5
    TabOrder = 2
    object GuiGraphXY: TGuiGraphXY
      Left = 16
      Top = 35
      Width = 145
      Height = 160
      FrameColor = 11383224
      SeriesCollection = <
        item
          DisplayName = 'TGuiGraphXYSeriesCollectionItem'
          SeriesClassName = 'TGuiGraphXYFunctionSeries'
          Series.Color = clWhite
        end>
      XAxis.Flags = []
      XAxis.Granularity = 20.000000000000000000
      XAxis.Minimum = -90.000000000000000000
      XAxis.Maximum = 10.000000000000000000
      XAxis.Lower = -90.000000000000000000
      XAxis.Upper = 10.000000000000000000
      YAxis.Flags = []
      YAxis.Granularity = 20.000000000000000000
      YAxis.Minimum = -90.000000000000000000
      YAxis.Maximum = 10.000000000000000000
      YAxis.Lower = -90.000000000000000000
      YAxis.Upper = 10.000000000000000000
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = []
      AntiAlias = gaaLinear2x
      LineColor = 5922921
      LineWidth = 2
    end
  end
  object GbLimiter: TGuiGroup
    Left = 8
    Top = 321
    Width = 457
    Height = 93
    AntiAlias = gaaLinear4x
    Caption = 'Limiter'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 14277598
    LineWidth = 2
    ParentFont = False
    Radius = 5
    TabOrder = 3
    object DialLimiterAttack: TGuiDial
      Left = 112
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.009999999776482582
      GlyphCount = 65
      OnChange = DialLimiterAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLimiterAttack: TGuiLabel
      Left = 103
      Top = 8
      Width = 52
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Attack'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbLimiterAttackValue: TGuiLabel
      Left = 93
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialLimiterRelease: TGuiDial
      Left = 200
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 50.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 5000.000000000000000000
      Min = 0.100000001490116100
      GlyphCount = 65
      OnChange = DialLimiterReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLimiterRelease: TGuiLabel
      Left = 185
      Top = 8
      Width = 68
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Release'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbLimiterReleaseValue: TGuiLabel
      Left = 179
      Top = 66
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialLimiterThreshold: TGuiDial
      Left = 288
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -1.799999952316284000
      DefaultPosition = -15.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      Min = -90.000000000000000000
      GlyphCount = 65
      OnChange = DialLimiterThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLimiterThreshold: TGuiLabel
      Left = 264
      Top = 8
      Width = 90
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Threshold'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbLimiterThresholdValue: TGuiLabel
      Left = 269
      Top = 66
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object DialLimiterKnee: TGuiDial
      Left = 387
      Top = 32
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      GlyphCount = 65
      OnChange = DialLimiterKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLimiterKnee: TGuiLabel
      Left = 363
      Top = 8
      Width = 82
      Height = 25
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Soft Knee'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -19
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbLimiterKneeValue: TGuiLabel
      Left = 368
      Top = 66
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
    end
    object LbSoftClip: TGuiLabel
      Left = 26
      Top = 35
      Width = 49
      Height = 14
      AntiAlias = gaaLinear4x
      Caption = 'Soft Clip'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      ParentFont = False
      OnClick = LbSoftClipClick
    end
    object LEDSoftClip: TGuiLED
      Left = 9
      Top = 34
      Width = 15
      Height = 15
      LEDColor = 14277598
      Brightness_Percent = 10.000000000000000000
      Uniformity_Percent = 40.000000000000000000
      AntiAlias = gaaLinear3x
      LineColor = 14277598
      OnClick = LbSoftClipClick
    end
  end
  object GuiDialImageList: TGuiDialImageList
    DialImages = <
      item
        DisplayName = 'Dial'
        DialBitmap.Data = {
          76140000424D7614000000000000360000002800000024000000240000000100
          2000000000004014000000000000000000000000000000000000FFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF
          FFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFFF}
        GlyphCount = 65
        StitchKind = skHorizontal
        Height = 36
        Width = 36
      end>
    Left = 63
    Top = 20
  end
end
