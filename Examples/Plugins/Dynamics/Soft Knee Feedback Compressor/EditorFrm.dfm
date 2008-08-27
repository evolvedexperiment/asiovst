object EditorForm: TEditorForm
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 128
  ClientWidth = 323
  Color = clSilver
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object LbThresholdValue: TLabel
    Left = 8
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Caption = 'dB'
    Transparent = False
  end
  object LbRatioValue: TLabel
    Left = 88
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object LbAttackValue: TLabel
    Left = 166
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object LbReleaseValue: TLabel
    Left = 246
    Top = 102
    Width = 64
    Height = 13
    Alignment = taCenter
    AutoSize = False
    Transparent = False
  end
  object DialThreshold: TGuiDial
    Left = 8
    Top = 32
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clGray
    CircleColor = clSilver
    Position = -0.000000000100000001
    DefaultPosition = -0.000000000100000001
    Min = -96.000000000000000000
    Max = -0.000000000100000001
    NumGlyphs = 31
    StitchKind = skVertical
    OnChange = DialThresholdChange
  end
  object DialRatio: TGuiDial
    Left = 88
    Top = 32
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clGray
    CircleColor = clSilver
    Max = 200.000000000000000000
    NumGlyphs = 31
    StitchKind = skVertical
    OnChange = DialRatioChange
  end
  object DialAttack: TGuiDial
    Left = 166
    Top = 32
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clGray
    CircleColor = clSilver
    Min = -200.000000000000000000
    Max = 300.000000000000000000
    NumGlyphs = 31
    StitchKind = skVertical
    OnChange = DialAttackChange
  end
  object DialRelease: TGuiDial
    Left = 246
    Top = 32
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clGray
    CircleColor = clSilver
    Position = 699.000000000000000000
    DefaultPosition = 699.000000000000000000
    Min = 699.000000000000000000
    Max = 3699.000000000000000000
    NumGlyphs = 31
    StitchKind = skVertical
    OnChange = DialReleaseChange
  end
  object LbRatio: TGuiLabel
    Left = 88
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Caption = 'Ratio'
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
  end
  object LbAttack: TGuiLabel
    Left = 166
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Caption = 'Attack'
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
  end
  object LbRelease: TGuiLabel
    Left = 246
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Caption = 'Release'
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
  end
  object LbThreshold: TGuiLabel
    Left = 8
    Top = 8
    Width = 64
    Height = 13
    Margins.Bottom = 0
    Caption = 'Threshold'
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    AutoSize = True
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
  end
end
