object FmNoiseReduction: TFmNoiseReduction
  Left = 333
  Top = 315
  BorderStyle = bsNone
  Caption = 'Spectral Noise Gate'
  ClientHeight = 147
  ClientWidth = 348
  Color = clBlack
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 14277598
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DialThresholdOffset: TGuiDial
    Left = 20
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = 0.150000005960464500
    DefaultPosition = 8.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 15.000000000000000000
    GlyphCount = 65
    OnChange = DialThresholdOffsetChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 8.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialRatio: TGuiDial
    Left = 91
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = -1.799999952316284000
    DefaultPosition = 10.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 100.000000000000000000
    Min = 1.000000000000000000
    GlyphCount = 65
    OnChange = DialRatioChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 10.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbThresholdOffset: TGuiLabel
    Left = 7
    Top = 8
    Width = 61
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Offset'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbRatio: TGuiLabel
    Left = 83
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Ratio'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbThresholdOffsetValue: TGuiLabel
    Left = 7
    Top = 66
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbRatioValue: TGuiLabel
    Left = 78
    Top = 66
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialKnee: TGuiDial
    Left = 158
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = -1.000000000000000000
    DefaultPosition = 1.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 10.000000000000000000
    GlyphCount = 65
    OnChange = DialKneeChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbKnee: TGuiLabel
    Left = 150
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Knee'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbKneeValue: TGuiLabel
    Left = 145
    Top = 66
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialAttack: TGuiDial
    Left = 224
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = -1.299999952316284000
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 100.000000000000000000
    Min = 0.009999999776482582
    GlyphCount = 65
    OnChange = DialAttackChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbAttack: TGuiLabel
    Left = 216
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Attack'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbAttackValue: TGuiLabel
    Left = 211
    Top = 66
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialRelease: TGuiDial
    Left = 290
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = -1.330000042915344000
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 300.000000000000000000
    Min = 0.500000000000000000
    GlyphCount = 65
    OnChange = DialReleaseChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 50.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbRelease: TGuiLabel
    Left = 277
    Top = 8
    Width = 60
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Release'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbReleaseValue: TGuiLabel
    Left = 277
    Top = 66
    Width = 60
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object SbWindowFunction: TGuiSelectBox
    Left = 151
    Top = 92
    Width = 189
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
      'Rectangle'
      'Triangle'
      'Hanning'
      'Hamming'
      'Blackman'
      'Lanczos'
      'Exact Blackman'
      'Blackman-Harris 3-Term'
      'Blackman-Harris 4-Term'
      'Blackman-Nutall'
      'Lawrey 5-Term'
      'Lawrey 6-Term'
      'Burgess Opt [59 dB]'
      'Burgess Opt [71 dB]'
      'Nutall CTD'
      'Nutall CFD'
      'Flat Top'
      'Albrecht'
      'Pacman'
      'Gauss'
      'Kaiser-Bessel')
    LineColor = 12632777
    ParentFont = False
    SelectBoxColor = clBlack
    OnChange = SbWindowFunctionChange
  end
  object LbWindowFunction: TGuiLabel
    Left = 8
    Top = 92
    Width = 137
    Height = 25
    AntiAlias = gaaLinear4x
    Caption = 'Window Function:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbCaptureNoiseProfile: TGuiLabel
    Left = 178
    Top = 120
    Width = 162
    Height = 22
    AntiAlias = gaaLinear4x
    Caption = 'Capture Noise Profile'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
    OnClick = LedNoiseProfileClick
  end
  object LedNoiseProfile: TGuiLED
    Left = 156
    Top = 124
    Width = 16
    Height = 16
    LEDColor = 14277598
    Brightness_Percent = 10.000000000000000000
    Uniformity_Percent = 40.000000000000000000
    AntiAlias = gaaLinear3x
    LineColor = 14277598
    OnClick = LedNoiseProfileClick
  end
  object GuiLabel1: TGuiLabel
    Left = 8
    Top = 120
    Width = 71
    Height = 22
    AntiAlias = gaaLinear4x
    Caption = 'FFT Size:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object SbFftSize: TGuiSelectBox
    Left = 85
    Top = 120
    Width = 64
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
      '64'
      '128'
      '256'
      '512'
      '1024'
      '2048'
      '4096'
      '8192')
    LineColor = 12632777
    ParentFont = False
    SelectBoxColor = clBlack
    OnChange = SbFftSizeChange
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 159
    Top = 32
  end
end
