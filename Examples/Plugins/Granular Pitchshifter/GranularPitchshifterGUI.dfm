object FmGranularPitchShifter: TFmGranularPitchShifter
  Left = 396
  Top = 84
  BorderStyle = bsNone
  Caption = 'GranularPitchShifter'
  ClientHeight = 93
  ClientWidth = 277
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
  PixelsPerInch = 96
  TextHeight = 13
  object DialStages: TGuiDial
    Left = 116
    Top = 31
    Width = 36
    Height = 36
    DefaultPosition = 4.000000000000000000
    DialImageIndex = -1
    LineColor = clRed
    LineWidth = 2
    Max = 16.000000000000000000
    Min = 2.000000000000000000
    NumGlyphs = 65
    OnChange = DialStagesChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 4.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbStages: TGuiLabel
    Left = 104
    Top = 8
    Width = 60
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Stages'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbStagesValue: TGuiLabel
    Left = 104
    Top = 65
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
  object DialSemitones: TGuiDial
    Left = 32
    Top = 31
    Width = 36
    Height = 36
    DefaultPosition = 4.000000000000000000
    DialImageIndex = -1
    LineColor = clRed
    LineWidth = 2
    Max = 12.000000000000000000
    Min = -12.000000000000000000
    NumGlyphs = 65
    OnChange = DialSemitonesChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 4.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbSemitones: TGuiLabel
    Left = 8
    Top = 8
    Width = 91
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Semitones'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbSemitonesValue: TGuiLabel
    Left = 8
    Top = 65
    Width = 91
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object DialGranularity: TGuiDial
    Left = 196
    Top = 31
    Width = 36
    Height = 36
    CurveMapping = -1.500000000000000000
    DefaultPosition = 20.000000000000000000
    DialImageIndex = -1
    LineColor = clRed
    LineWidth = 2
    Max = 1000.000000000000000000
    Min = 1.000000000000000000
    NumGlyphs = 65
    OnChange = DialGranularityChange
    PointerAngles.Start = 225
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 20.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object LbGranularity: TGuiLabel
    Left = 168
    Top = 8
    Width = 91
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Granularity'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbGranularityValue: TGuiLabel
    Left = 176
    Top = 65
    Width = 76
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
end
