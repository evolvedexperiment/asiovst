object FmLightweightMultibandCompressor: TFmLightweightMultibandCompressor
  Left = 286
  Top = 77
  BorderStyle = bsNone
  Caption = 'Fast Multiband Compressor'
  ClientHeight = 461
  ClientWidth = 601
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    601
    461)
  PixelsPerInch = 96
  TextHeight = 13
  object LEDSoftClip: TGuiLED
    Left = 502
    Top = 29
    Width = 15
    Height = 15
    Brightness_Percent = 10.000000000000000000
    LEDColor = 14277598
    AntiAlias = gaaLinear3x
    LineColor = clRed
    OnClick = LEDSoftClipClick
  end
  object LbSoftClip: TGuiLabel
    Left = 521
    Top = 28
    Width = 59
    Height = 17
    AntiAlias = gaaLinear4x
    Caption = 'Soft Clip'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    OnClick = LEDSoftClipClick
  end
  object LbLow: TGuiLabel
    Left = 36
    Top = 12
    Width = 33
    Height = 19
    AntiAlias = gaaLinear4x
    Caption = 'Low'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbLowMid: TGuiLabel
    Left = 139
    Top = 12
    Width = 70
    Height = 19
    AntiAlias = gaaLinear4x
    Caption = 'Low Mid'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbHighMid: TGuiLabel
    Left = 275
    Top = 12
    Width = 73
    Height = 23
    AntiAlias = gaaLinear4x
    Caption = 'High Mid'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbHigh: TGuiLabel
    Left = 413
    Top = 12
    Width = 37
    Height = 23
    AntiAlias = gaaLinear4x
    Caption = 'High'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialLowFreq: TGuiDial
    Left = 84
    Top = 5
    Width = 36
    Height = 36
    CurveMapping = -0.550000011920929000
    DefaultPosition = 300.000000000000000000
    DialImageList = GuiDialImageList
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 800.000000000000000000
    Min = 20.000000000000000000
    NumGlyphs = 65
    OnChange = DialLowFreqChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 300.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
  end
  object LbLowFreqValue: TGuiLabel
    Left = 65
    Top = 40
    Width = 72
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = '300 Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialMidFreq: TGuiDial
    Left = 224
    Top = 5
    Width = 36
    Height = 36
    CurveMapping = -1.500000000000000000
    DefaultPosition = 800.000000000000000000
    DialImageList = GuiDialImageList
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 4000.000000000000000000
    Min = 300.000000000000000000
    NumGlyphs = 65
    OnChange = DialMidFreqChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 800.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
  end
  object LbMidFreqValue: TGuiLabel
    Left = 205
    Top = 40
    Width = 72
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = '800 Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialHighFreq: TGuiDial
    Left = 354
    Top = 5
    Width = 36
    Height = 36
    CurveMapping = -1.350000023841858000
    DefaultPosition = 4000.000000000000000000
    DialImageList = GuiDialImageList
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 20000.000000000000000000
    Min = 800.000000000000000000
    NumGlyphs = 65
    OnChange = DialHighFreqChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 4000.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
  end
  object LbHighFreqValue: TGuiLabel
    Left = 343
    Top = 40
    Width = 72
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = '4 kHz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbLowS: TGuiLabel
    Left = 36
    Top = 42
    Width = 9
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbLowMute: TGuiLabel
    Left = 45
    Top = 42
    Width = 11
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'M'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbLowBypass: TGuiLabel
    Left = 57
    Top = 42
    Width = 8
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbLowMidSolo: TGuiLabel
    Left = 159
    Top = 42
    Width = 9
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbLowMidMute: TGuiLabel
    Left = 168
    Top = 42
    Width = 11
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'M'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbLowMidBypass: TGuiLabel
    Left = 180
    Top = 42
    Width = 8
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbHighMidSolo: TGuiLabel
    Left = 293
    Top = 42
    Width = 9
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbHighMidMute: TGuiLabel
    Left = 302
    Top = 42
    Width = 11
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'M'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbHighMidBypass: TGuiLabel
    Left = 314
    Top = 42
    Width = 8
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbHighSolo: TGuiLabel
    Left = 421
    Top = 42
    Width = 9
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'S'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbHighMute: TGuiLabel
    Left = 430
    Top = 42
    Width = 11
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'M'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object LbHighBypass: TGuiLabel
    Left = 442
    Top = 42
    Width = 8
    Height = 16
    AntiAlias = gaaLinear4x
    Caption = 'B'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 4804436
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Visible = False
    OnClick = LEDSoftClipClick
  end
  object PnLowBand: TGuiPanel
    Left = 8
    Top = 68
    Width = 585
    Height = 92
    Anchors = [akLeft, akRight, akBottom]
    AntiAlias = gaaLinear3x
    Caption = 'LowBand'
    LineColor = 14277598
    PanelColor = clBlack
    ParentColor = True
    Radius = 9
    TabOrder = 1
    UseDockManager = True
    object DialLowThreshold: TGuiDial
      Left = 185
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowThreshold: TGuiLabel
      Left = 161
      Top = 7
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
    end
    object LbLowThresholdValue: TGuiLabel
      Left = 166
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '30 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowAttack: TGuiDial
      Left = 25
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.100000001490116100
      NumGlyphs = 65
      OnChange = DialAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowAttack: TGuiLabel
      Left = 16
      Top = 7
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
    end
    object LbLowAttackValue: TGuiLabel
      Left = 6
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowRelease: TGuiDial
      Left = 105
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowRelease: TGuiLabel
      Left = 90
      Top = 7
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
    end
    object LbLowReleaseValue: TGuiLabel
      Left = 84
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '80 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowRatio: TGuiDial
      Left = 265
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialRatioChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowRatio: TGuiLabel
      Left = 257
      Top = 7
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
    end
    object LbLowRatioValue: TGuiLabel
      Left = 244
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 : 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowKnee: TGuiDial
      Left = 345
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      NumGlyphs = 65
      OnChange = DialKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowKnee: TGuiLabel
      Left = 321
      Top = 7
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
    end
    object LbLowKneeValue: TGuiLabel
      Left = 326
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object GXYLow: TGuiGraphXY
      Left = 497
      Top = 10
      Width = 68
      Height = 56
      FrameColor = 6844281
      Flags = []
      SeriesCollection = <
        item
          DisplayName = 'TGuiGraphXYSeriesCollectionItem'
          SeriesClassName = 'TGuiGraphXYFunctionSeries'
          Series.Color = clWhite
        end>
      XAxis.Flags = []
      XAxis.Granularity = 50.000000000000000000
      XAxis.Minimum = -90.000000000000000000
      XAxis.Maximum = 10.000000000000000000
      XAxis.Lower = -90.000000000000000000
      XAxis.Upper = 10.000000000000000000
      YAxis.Flags = []
      YAxis.Granularity = 50.000000000000000000
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
      LineColor = 3948356
      LineWidth = 2
    end
    object DialLowMakeUpGain: TGuiDial
      Left = 425
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 10.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 60.000000000000000000
      NumGlyphs = 65
      OnChange = DialMakeUpGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMakeUpGain: TGuiLabel
      Left = 408
      Top = 7
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
    end
    object LbLowMakeUpGainValue: TGuiLabel
      Left = 406
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LEDLowAutoGain: TGuiLED
      Left = 494
      Top = 70
      Width = 15
      Height = 15
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      AntiAlias = gaaLinear3x
      LineColor = clRed
      OnClick = LEDLowAutoGainClick
    end
    object LbLowAutogain: TGuiLabel
      Left = 513
      Top = 71
      Width = 54
      Height = 13
      AntiAlias = gaaLinear4x
      Caption = 'Auto Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      OnClick = LEDLowAutoGainClick
    end
  end
  object PnLowMidBand: TGuiPanel
    Tag = 1
    Left = 8
    Top = 166
    Width = 585
    Height = 92
    Anchors = [akLeft, akRight, akBottom]
    AntiAlias = gaaLinear3x
    Caption = 'LowMidBand'
    LineColor = 14277598
    PanelColor = clBlack
    ParentColor = True
    Radius = 9
    TabOrder = 3
    UseDockManager = True
    object DialLowMidThreshold: TGuiDial
      Tag = 1
      Left = 185
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMidThreshold: TGuiLabel
      Tag = 1
      Left = 161
      Top = 7
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
    end
    object LbLowMidThresholdValue: TGuiLabel
      Tag = 1
      Left = 166
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '30 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowMidAttack: TGuiDial
      Tag = 1
      Left = 25
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.100000001490116100
      NumGlyphs = 65
      OnChange = DialAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMidAttack: TGuiLabel
      Tag = 1
      Left = 16
      Top = 7
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
    end
    object LbLowMidAttackValue: TGuiLabel
      Tag = 1
      Left = 6
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowMidRelease: TGuiDial
      Tag = 1
      Left = 105
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMidRelease: TGuiLabel
      Tag = 1
      Left = 90
      Top = 7
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
    end
    object LbLowMidReleaseValue: TGuiLabel
      Tag = 1
      Left = 84
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '80 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowMidRatio: TGuiDial
      Tag = 1
      Left = 265
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialRatioChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMidRatio: TGuiLabel
      Tag = 1
      Left = 257
      Top = 7
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
    end
    object LbLowMidRatioValue: TGuiLabel
      Tag = 1
      Left = 244
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 : 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialLowMidKnee: TGuiDial
      Tag = 1
      Left = 345
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      NumGlyphs = 65
      OnChange = DialKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMidKnee: TGuiLabel
      Tag = 1
      Left = 321
      Top = 7
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
    end
    object LbLowMidKneeValue: TGuiLabel
      Tag = 1
      Left = 326
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object GXYLowMid: TGuiGraphXY
      Tag = 1
      Left = 497
      Top = 10
      Width = 68
      Height = 56
      FrameColor = 6844281
      Flags = []
      SeriesCollection = <
        item
          DisplayName = 'TGuiGraphXYSeriesCollectionItem'
          SeriesClassName = 'TGuiGraphXYFunctionSeries'
          Series.Color = clWhite
        end>
      XAxis.Flags = []
      XAxis.Granularity = 50.000000000000000000
      XAxis.Minimum = -90.000000000000000000
      XAxis.Maximum = 10.000000000000000000
      XAxis.Lower = -90.000000000000000000
      XAxis.Upper = 10.000000000000000000
      YAxis.Flags = []
      YAxis.Granularity = 50.000000000000000000
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
      LineColor = 3948356
      LineWidth = 2
    end
    object DialLowMidMakeUpGain: TGuiDial
      Tag = 1
      Left = 425
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 10.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 60.000000000000000000
      NumGlyphs = 65
      OnChange = DialMakeUpGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbLowMidMakeUpGain: TGuiLabel
      Tag = 1
      Left = 408
      Top = 7
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
    end
    object LbLowMidMakeUpGainValue: TGuiLabel
      Tag = 1
      Left = 406
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LEDLowMidAutoGain: TGuiLED
      Tag = 1
      Left = 494
      Top = 70
      Width = 15
      Height = 15
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      AntiAlias = gaaLinear3x
      LineColor = clRed
      OnClick = LEDLowMidAutoGainClick
    end
    object LbLowMidAutogain: TGuiLabel
      Tag = 1
      Left = 513
      Top = 71
      Width = 54
      Height = 13
      AntiAlias = gaaLinear4x
      Caption = 'Auto Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      OnClick = LEDLowMidAutoGainClick
    end
  end
  object PnHighMidBand: TGuiPanel
    Tag = 2
    Left = 8
    Top = 264
    Width = 585
    Height = 92
    Anchors = [akLeft, akRight, akBottom]
    AntiAlias = gaaLinear3x
    Caption = 'HighMidBand'
    LineColor = 14277598
    PanelColor = clBlack
    ParentColor = True
    Radius = 9
    TabOrder = 2
    UseDockManager = True
    object DialHighMidThreshold: TGuiDial
      Tag = 2
      Left = 185
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMidThreshold: TGuiLabel
      Tag = 2
      Left = 161
      Top = 7
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
    end
    object LbHighMidThresholdValue: TGuiLabel
      Tag = 2
      Left = 166
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '30 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighMidAttack: TGuiDial
      Tag = 2
      Left = 25
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.100000001490116100
      NumGlyphs = 65
      OnChange = DialAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMidAttack: TGuiLabel
      Tag = 2
      Left = 16
      Top = 7
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
    end
    object LbHighMidAttackValue: TGuiLabel
      Tag = 2
      Left = 6
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighMidRelease: TGuiDial
      Tag = 2
      Left = 105
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMidRelease: TGuiLabel
      Tag = 2
      Left = 90
      Top = 7
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
    end
    object LbHighMidReleaseValue: TGuiLabel
      Tag = 2
      Left = 84
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '80 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighMidRatio: TGuiDial
      Tag = 2
      Left = 265
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialRatioChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMidRatio: TGuiLabel
      Tag = 2
      Left = 257
      Top = 7
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
    end
    object LbHighMidRatioValue: TGuiLabel
      Tag = 2
      Left = 244
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 : 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighMidKnee: TGuiDial
      Tag = 2
      Left = 345
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      NumGlyphs = 65
      OnChange = DialKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMidKnee: TGuiLabel
      Tag = 2
      Left = 321
      Top = 7
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
    end
    object LbHighMidKneeValue: TGuiLabel
      Tag = 2
      Left = 326
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object GXYHighMid: TGuiGraphXY
      Tag = 2
      Left = 497
      Top = 10
      Width = 68
      Height = 56
      FrameColor = 6844281
      Flags = []
      SeriesCollection = <
        item
          DisplayName = 'TGuiGraphXYSeriesCollectionItem'
          SeriesClassName = 'TGuiGraphXYFunctionSeries'
          Series.Color = clWhite
        end>
      XAxis.Flags = []
      XAxis.Granularity = 50.000000000000000000
      XAxis.Minimum = -90.000000000000000000
      XAxis.Maximum = 10.000000000000000000
      XAxis.Lower = -90.000000000000000000
      XAxis.Upper = 10.000000000000000000
      YAxis.Flags = []
      YAxis.Granularity = 50.000000000000000000
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
      LineColor = 3948356
      LineWidth = 2
    end
    object DialHighMidMakeUpGain: TGuiDial
      Tag = 2
      Left = 425
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 10.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 60.000000000000000000
      NumGlyphs = 65
      OnChange = DialMakeUpGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMidMakeUpGain: TGuiLabel
      Tag = 2
      Left = 408
      Top = 7
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
    end
    object LbHighMidMakeUpGainValue: TGuiLabel
      Tag = 2
      Left = 406
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LEDHighMidAutoGain: TGuiLED
      Tag = 2
      Left = 494
      Top = 70
      Width = 15
      Height = 15
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      AntiAlias = gaaLinear3x
      LineColor = clRed
      OnClick = LEDHighMidAutoGainClick
    end
    object LbHighMidAutogain: TGuiLabel
      Tag = 2
      Left = 513
      Top = 71
      Width = 54
      Height = 13
      AntiAlias = gaaLinear4x
      Caption = 'Auto Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      OnClick = LEDHighMidAutoGainClick
    end
  end
  object PnHighBand: TGuiPanel
    Tag = 3
    Left = 8
    Top = 362
    Width = 585
    Height = 92
    Anchors = [akLeft, akRight, akBottom]
    AntiAlias = gaaLinear3x
    Caption = 'HighBand'
    LineColor = 14277598
    PanelColor = clBlack
    ParentColor = True
    Radius = 9
    TabOrder = 0
    UseDockManager = True
    object DialHighThreshold: TGuiDial
      Tag = 3
      Left = 185
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialThresholdChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighThreshold: TGuiLabel
      Tag = 3
      Left = 161
      Top = 7
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
    end
    object LbHighThresholdValue: TGuiLabel
      Tag = 3
      Left = 166
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '30 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighAttack: TGuiDial
      Tag = 3
      Left = 25
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -1.299999952316284000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 1000.000000000000000000
      Min = 0.100000001490116100
      NumGlyphs = 65
      OnChange = DialAttackChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighAttack: TGuiLabel
      Tag = 3
      Left = 16
      Top = 7
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
    end
    object LbHighAttackValue: TGuiLabel
      Tag = 3
      Left = 6
      Top = 65
      Width = 72
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighRelease: TGuiDial
      Tag = 3
      Left = 105
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialReleaseChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 50.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighRelease: TGuiLabel
      Tag = 3
      Left = 90
      Top = 7
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
    end
    object LbHighReleaseValue: TGuiLabel
      Tag = 3
      Left = 84
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '80 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighRatio: TGuiDial
      Tag = 3
      Left = 265
      Top = 31
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
      NumGlyphs = 65
      OnChange = DialRatioChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighRatio: TGuiLabel
      Tag = 3
      Left = 257
      Top = 7
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
    end
    object LbHighRatioValue: TGuiLabel
      Tag = 3
      Left = 244
      Top = 65
      Width = 76
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 : 2'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object DialHighKnee: TGuiDial
      Tag = 3
      Left = 345
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 1.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 10.000000000000000000
      NumGlyphs = 65
      OnChange = DialKneeChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighKnee: TGuiLabel
      Tag = 3
      Left = 321
      Top = 7
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
    end
    object LbHighKneeValue: TGuiLabel
      Tag = 3
      Left = 326
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '1 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object GXYHigh: TGuiGraphXY
      Tag = 3
      Left = 497
      Top = 10
      Width = 68
      Height = 56
      FrameColor = 6844281
      Flags = []
      SeriesCollection = <
        item
          DisplayName = 'TGuiGraphXYSeriesCollectionItem'
          SeriesClassName = 'TGuiGraphXYFunctionSeries'
          Series.Color = clWhite
        end>
      XAxis.Flags = []
      XAxis.Granularity = 50.000000000000000000
      XAxis.Minimum = -90.000000000000000000
      XAxis.Maximum = 10.000000000000000000
      XAxis.Lower = -90.000000000000000000
      XAxis.Upper = 10.000000000000000000
      YAxis.Flags = []
      YAxis.Granularity = 50.000000000000000000
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
      LineColor = 3948356
      LineWidth = 2
    end
    object DialHighMakeUpGain: TGuiDial
      Tag = 3
      Left = 425
      Top = 31
      Width = 36
      Height = 36
      CurveMapping = -0.800000011920929000
      DefaultPosition = 10.000000000000000000
      DialImageList = GuiDialImageList
      DialImageIndex = -1
      LineColor = 14277598
      LineWidth = 2
      Max = 60.000000000000000000
      NumGlyphs = 65
      OnChange = DialMakeUpGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 3.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbHighMakeUpGain: TGuiLabel
      Tag = 3
      Left = 408
      Top = 7
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
    end
    object LbHighMakeUpGainValue: TGuiLabel
      Tag = 3
      Left = 406
      Top = 65
      Width = 74
      Height = 20
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '10 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -16
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LEDHighAutoGain: TGuiLED
      Tag = 3
      Left = 494
      Top = 70
      Width = 15
      Height = 15
      Brightness_Percent = 10.000000000000000000
      LEDColor = 14277598
      AntiAlias = gaaLinear3x
      LineColor = clRed
      OnClick = LEDHighAutoGainClick
    end
    object LbHighAutogain: TGuiLabel
      Tag = 3
      Left = 513
      Top = 71
      Width = 54
      Height = 13
      AntiAlias = gaaLinear4x
      Caption = 'Auto Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 14277598
      Font.Height = -11
      Font.Name = 'Tahoma'
      Font.Style = [fsBold]
      OnClick = LEDHighAutoGainClick
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
        NumGlyphs = 65
        StitchKind = skHorizontal
        Height = 36
        Width = 36
      end>
    Left = 48
    Top = 104
  end
  object Timer: TTimer
    Interval = 10
    OnTimer = TimerTimer
    Left = 80
    Top = 104
  end
end
