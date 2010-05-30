object FmSpectralNoiseGate: TFmSpectralNoiseGate
  Left = 333
  Top = 315
  BorderStyle = bsNone
  Caption = 'Spectral Noise Gate'
  ClientHeight = 123
  ClientWidth = 431
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
  object DialThreshold: TGuiDial
    Left = 26
    Top = 32
    Width = 36
    Height = 36
    DefaultPosition = 1.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 6.000000000000000000
    Min = -96.000000000000000000
    NumGlyphs = 65
    OnChange = DialThresholdChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialFftOrder: TGuiDial
    Left = 111
    Top = 32
    Width = 36
    Height = 36
    DefaultPosition = 6.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 13.000000000000000000
    Min = 6.000000000000000000
    NumGlyphs = 65
    OnChange = DialFftOrderChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 6.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialRatio: TGuiDial
    Left = 180
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
    NumGlyphs = 65
    OnChange = DialRatioChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 10.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbThreshold: TGuiLabel
    Left = 6
    Top = 8
    Width = 74
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Threshold'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbFftOrder: TGuiLabel
    Left = 88
    Top = 8
    Width = 80
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'FFT Order'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -17
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbRatio: TGuiLabel
    Left = 172
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
  object LbThresholdValue: TGuiLabel
    Left = 6
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
  end
  object LbFftOrderValue: TGuiLabel
    Left = 98
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
    Left = 167
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
    Left = 245
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
    NumGlyphs = 65
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
    Left = 237
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
    Left = 232
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
    Left = 309
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
    NumGlyphs = 65
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
    Left = 301
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
    Left = 296
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
    Left = 373
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
    NumGlyphs = 65
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
    Left = 360
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
    Left = 360
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
    Width = 272
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
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 58
    Top = 32
  end
end
