object FmSplitHarmonizer: TFmSplitHarmonizer
  Left = 332
  Top = 140
  BorderStyle = bsNone
  Caption = 'SoundTouch Detuner'
  ClientHeight = 267
  ClientWidth = 414
  Color = 987667
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 15133420
  Font.Height = -19
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 23
  object LbEncoding: TGuiLabel
    Left = 85
    Top = 120
    Width = 105
    Height = 24
    AntiAlias = gaaLinear4x
    Caption = 'Encoding:'
    Shadow.Color = clBlack
  end
  object SwEncoding: TGuiSwitch
    Left = 196
    Top = 120
    Width = 138
    Height = 24
    AntiAlias = gaaLinear4x
    DefaultGlyphNr = 0
    DialImageIndex = -1
    GlyphNr = 0
    LineColor = 15133420
    LineWidth = 2
    GlyphCount = 2
    OnChange = SwEncodingChange
    StitchKind = skHorizontal
    StringList.Strings = (
      'Left / Right'
      'Mid / Side')
  end
  object PnStageA: TGuiPanel
    Left = 8
    Top = 8
    Width = 398
    Height = 107
    Caption = 'PnStageA'
    Color = clBlack
    LineColor = 15133420
    BorderWidth = 2.000000000000000000
    PanelColor = clBlack
    Radius = 7.000000000000000000
    TabOrder = 0
    UseDockManager = True
    Transparent = True
    object DialDetuneA: TGuiDial
      Left = 41
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 20.000000000000000000
      Min = -20.000000000000000000
      GlyphCount = 65
      OnChange = DialDetuneAChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbDetuneA: TGuiLabel
      Left = 16
      Top = 9
      Width = 87
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Detune'
      Shadow.Color = clBlack
    end
    object LbDetuneAValue: TGuiLabel
      Left = 16
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Cent'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbDelayA: TGuiLabel
      Left = 109
      Top = 9
      Width = 87
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Delay'
      Shadow.Color = clBlack
    end
    object DialDelayA: TGuiDial
      Left = 134
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      CurveMapping = -0.699999988079071000
      DefaultPosition = 10.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 80.000000000000000000
      Min = 0.001000000047497451
      GlyphCount = 65
      OnChange = DialDelayAChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbDelayAValue: TGuiLabel
      Left = 109
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '100 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbMixA: TGuiLabel
      Left = 318
      Top = 8
      Width = 39
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Mix'
      Shadow.Color = clBlack
    end
    object DialMixA: TGuiDial
      Left = 320
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      GlyphCount = 65
      OnChange = DialMixAChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbMixAValue: TGuiLabel
      Left = 295
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '100 %'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbLowpassA: TGuiLabel
      Left = 204
      Top = 9
      Width = 91
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Lowpass'
      Shadow.Color = clBlack
    end
    object DialLowpassA: TGuiDial
      Left = 231
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      CurveMapping = -0.400000005960464500
      DefaultPosition = 10.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      GlyphCount = 65
      OnChange = DialLowpassAChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 8000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLowpassAValue: TGuiLabel
      Left = 206
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '8000 Hz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
  end
  object PnStageB: TGuiPanel
    Left = 8
    Top = 151
    Width = 398
    Height = 107
    Caption = 'PnStageA'
    Color = clBlack
    LineColor = 15133420
    BorderWidth = 2.000000000000000000
    PanelColor = clBlack
    Radius = 7.000000000000000000
    TabOrder = 1
    UseDockManager = True
    Transparent = True
    object DialDetuneB: TGuiDial
      Left = 41
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 20.000000000000000000
      Min = -20.000000000000000000
      GlyphCount = 65
      OnChange = DialDetuneBChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbDetuneB: TGuiLabel
      Left = 20
      Top = 9
      Width = 79
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Detune'
      Shadow.Color = clBlack
    end
    object LbDetuneBValue: TGuiLabel
      Left = 16
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Cent'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbDelayB: TGuiLabel
      Left = 121
      Top = 9
      Width = 63
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Delay'
      Shadow.Color = clBlack
    end
    object DialDelayB: TGuiDial
      Left = 134
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      CurveMapping = -0.699999988079071000
      DefaultPosition = 10.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 80.000000000000000000
      Min = 0.001000000047497451
      GlyphCount = 65
      OnChange = DialDelayBChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 10.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbDelayBValue: TGuiLabel
      Left = 109
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '100 ms'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbMixB: TGuiLabel
      Left = 318
      Top = 9
      Width = 39
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Mix'
      Shadow.Color = clBlack
    end
    object DialMixB: TGuiDial
      Left = 320
      Top = 39
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      GlyphCount = 65
      OnChange = DialMixBChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbMixBValue: TGuiLabel
      Left = 295
      Top = 77
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '100 %'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object LbLowpassBValue: TGuiLabel
      Left = 206
      Top = 76
      Width = 87
      Height = 18
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = '8000 Hz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15133420
      Font.Height = -16
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
      ParentFont = False
      Shadow.Color = clBlack
    end
    object DialLowpassB: TGuiDial
      Left = 231
      Top = 38
      Width = 36
      Height = 36
      AntiAlias = gaaLinear2x
      CurveMapping = -0.400000005960464500
      DefaultPosition = 10.000000000000000000
      DialImageList = DIL
      DialImageIndex = -1
      LineColor = 15133420
      LineWidth = 3
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      GlyphCount = 65
      OnChange = DialLowpassBChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 8000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      WheelStep = 1.000000000000000000
    end
    object LbLowpassB: TGuiLabel
      Left = 204
      Top = 9
      Width = 91
      Height = 24
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Lowpass'
      Shadow.Color = clBlack
    end
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 40
    Top = 24
  end
end
