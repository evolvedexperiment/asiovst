object FmBarberpoleFlanger: TFmBarberpoleFlanger
  Left = 396
  Top = 84
  BorderStyle = bsNone
  Caption = 'Simple Shepard'
  ClientHeight = 115
  ClientWidth = 247
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
  object DialSpeed: TGuiDial
    Left = 76
    Top = 58
    Width = 36
    Height = 36
    CurveMapping = -1.799999952316284000
    DefaultPosition = 1.000000000000000000
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 10.000000000000000000
    Min = 0.009999999776482582
    NumGlyphs = 65
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
    Left = 16
    Top = 58
    Width = 36
    Height = 36
    DefaultPosition = 4.000000000000000000
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 8.000000000000000000
    Min = 1.000000000000000000
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
  object DialDepth: TGuiDial
    Left = 136
    Top = 58
    Width = 36
    Height = 36
    DefaultPosition = 10.000000000000000000
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 100.000000000000000000
    NumGlyphs = 65
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
    Top = 58
    Width = 36
    Height = 36
    DefaultPosition = 50.000000000000000000
    DialImageIndex = -1
    LineColor = 14277598
    LineWidth = 2
    Max = 100.000000000000000000
    NumGlyphs = 65
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
    Left = 68
    Top = 34
    Width = 52
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Speed'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbStages: TGuiLabel
    Left = 6
    Top = 34
    Width = 56
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
  object LbDepth: TGuiLabel
    Left = 128
    Top = 34
    Width = 50
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Depth'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbMix: TGuiLabel
    Left = 197
    Top = 34
    Width = 34
    Height = 24
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Mix'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbSpeedValue: TGuiLabel
    Left = 64
    Top = 92
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
  object LbStagesValue: TGuiLabel
    Left = 18
    Top = 92
    Width = 32
    Height = 20
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object LbDepthValue: TGuiLabel
    Left = 123
    Top = 92
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
  object LbMixValue: TGuiLabel
    Left = 183
    Top = 92
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
  object GuiLabel1: TGuiLabel
    Left = 8
    Top = 8
    Width = 85
    Height = 23
    AntiAlias = gaaLinear4x
    Caption = 'Algorithm:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
  end
  object SBAlgorithm: TGuiSelectBox
    Left = 99
    Top = 8
    Width = 132
    Height = 23
    AntiAlias = gaaLinear4x
    ArrowColor = 14277598
    ButtonColor = clBlack
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 14277598
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    ItemIndex = -1
    Items.Strings = (
      'Up'
      'Down'
      'Up (Inv.)'
      'Down (Inv.)')
    LineColor = 14277598
    LineWidth = 2
    ParentFont = False
    Radius = 4
    SelectBoxColor = 2829619
    OnChange = SBAlgorithmChange
  end
end
