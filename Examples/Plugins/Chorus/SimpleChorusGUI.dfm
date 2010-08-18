object FmSimpleChorus: TFmSimpleChorus
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'Simple Chorus'
  ClientHeight = 89
  ClientWidth = 306
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
  object DialSpeed: TGuiDial
    Left = 16
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = -1.799999952316284000
    DefaultPosition = 1.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 10.000000000000000000
    Min = 0.009999999776482582
    GlyphCount = 65
    OnChange = DialSpeedChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 1.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialStages: TGuiDial
    Left = 76
    Top = 32
    Width = 36
    Height = 36
    DefaultPosition = 4.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 8.000000000000000000
    Min = 1.000000000000000000
    GlyphCount = 65
    OnChange = DialStagesChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 4.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialDepth: TGuiDial
    Left = 136
    Top = 32
    Width = 36
    Height = 36
    DefaultPosition = 10.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 100.000000000000000000
    GlyphCount = 65
    OnChange = DialDepthChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 10.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object DialMix: TGuiDial
    Left = 196
    Top = 32
    Width = 36
    Height = 36
    DefaultPosition = 50.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 100.000000000000000000
    GlyphCount = 65
    OnChange = DialMixChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 50.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbSpeed: TGuiLabel
    Left = 8
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Speed'
  end
  object LbStages: TGuiLabel
    Left = 65
    Top = 8
    Width = 56
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Stages'
  end
  object LbDepth: TGuiLabel
    Left = 128
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Depth'
  end
  object LbMix: TGuiLabel
    Left = 188
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Mix'
  end
  object LbSpeedValue: TGuiLabel
    Left = 3
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
    ParentFont = False
  end
  object LbStagesValue: TGuiLabel
    Left = 63
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
    ParentFont = False
  end
  object LbDepthValue: TGuiLabel
    Left = 123
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
    ParentFont = False
  end
  object LbMixValue: TGuiLabel
    Left = 183
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
    ParentFont = False
  end
  object DialDrift: TGuiDial
    Left = 254
    Top = 32
    Width = 36
    Height = 36
    CurveMapping = -1.000000000000000000
    DefaultPosition = 10.000000000000000000
    DialImageList = DIL
    DialImageIndex = -1
    LineColor = 12632777
    LineWidth = 2
    Max = 100.000000000000000000
    GlyphCount = 65
    OnChange = DialDriftChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 10.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbDrift: TGuiLabel
    Left = 246
    Top = 8
    Width = 50
    Height = 25
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Drift'
  end
  object LbDriftValue: TGuiLabel
    Left = 241
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
    ParentFont = False
  end
  object DIL: TGuiDialImageList
    DialImages = <>
    Left = 16
    Top = 16
  end
end
