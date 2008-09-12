object FmCombo: TFmCombo
  Left = 338
  Top = 158
  BorderStyle = bsNone
  Caption = 'Combo'
  ClientHeight = 167
  ClientWidth = 371
  Color = 4227200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clMaroon
  Font.Height = -11
  Font.Name = 'Trebuchet MS'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object LbResonanceValue: TLabel
    Left = 288
    Top = 145
    Width = 64
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = '0.0'
  end
  object LbFrequencyValue: TLabel
    Left = 218
    Top = 145
    Width = 64
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = '0.0'
  end
  object LbOutputValue: TLabel
    Left = 148
    Top = 145
    Width = 64
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = '0.0'
  end
  object LbBiasValue: TLabel
    Left = 78
    Top = 145
    Width = 64
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = '0.0'
  end
  object LbDriveValue: TLabel
    Left = 8
    Top = 145
    Width = 64
    Height = 16
    Alignment = taCenter
    AutoSize = False
    Caption = '0.0'
  end
  object SBModel: TGuiSelectBox
    Left = 82
    Top = 8
    Width = 168
    Height = 25
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -21
    Font.Name = 'Trebuchet MS'
    Font.Style = []
    ItemIndex = -1
    Items.Strings = (
      'D.I.'
      'Speaker Sim'
      'Radio'
      'MB 1"'
      'MB 8"'
      '4x12 ^'
      '4x12 >')
    LineColor = 2039583
    LineWidth = 2
    SelectBoxColor = 4227200
    Radius = 8
    OnChange = SBModelChange
  end
  object DialDrive: TGuiDial
    Left = 8
    Top = 75
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clMaroon
    Min = -100.000000000000000000
    Max = 100.000000000000000000
    NumGlyphs = 65
    StitchKind = skHorizontal
    OnChange = DialDriveChange
  end
  object LbModel: TGuiLabel
    Left = 8
    Top = 8
    Width = 67
    Height = 25
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Model:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -21
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsBold]
  end
  object LbDrive: TGuiLabel
    Left = 8
    Top = 48
    Width = 64
    Height = 17
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Drive'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsUnderline]
  end
  object DialBias: TGuiDial
    Left = 78
    Top = 75
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clMaroon
    Min = -100.000000000000000000
    Max = 100.000000000000000000
    NumGlyphs = 65
    StitchKind = skHorizontal
    OnChange = DialBiasChange
  end
  object LbBias: TGuiLabel
    Left = 78
    Top = 48
    Width = 64
    Height = 17
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Bias'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsUnderline]
  end
  object DialOutput: TGuiDial
    Left = 148
    Top = 75
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clMaroon
    Min = -20.000000000000000000
    Max = 20.000000000000000000
    NumGlyphs = 65
    StitchKind = skHorizontal
    OnChange = DialOutputChange
  end
  object DialFrequency: TGuiDial
    Left = 218
    Top = 75
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clMaroon
    Position = 20.000000000000000000
    DefaultPosition = 20.000000000000000000
    Min = 20.000000000000000000
    Max = 20000.000000000000000000
    NumGlyphs = 65
    StitchKind = skHorizontal
    OnChange = DialFreqChange
  end
  object DialResonance: TGuiDial
    Left = 288
    Top = 75
    Width = 64
    Height = 64
    LineWidth = 2
    LineColor = clMaroon
    Max = 100.000000000000000000
    NumGlyphs = 65
    StitchKind = skHorizontal
    OnChange = DialResoChange
  end
  object LbOutput: TGuiLabel
    Left = 148
    Top = 48
    Width = 64
    Height = 17
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Output'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsUnderline]
  end
  object LbFrequency: TGuiLabel
    Left = 218
    Top = 48
    Width = 64
    Height = 17
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Frequency'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsUnderline]
  end
  object LbResonance: TGuiLabel
    Left = 288
    Top = 48
    Width = 64
    Height = 17
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Resonance'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -13
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsUnderline]
  end
  object RBMono: TRadioButton
    Left = 257
    Top = 12
    Width = 49
    Height = 17
    Caption = 'Mono'
    Checked = True
    TabOrder = 0
    TabStop = True
    OnClick = RBMonoClick
  end
  object RBStereo: TRadioButton
    Left = 310
    Top = 12
    Width = 58
    Height = 17
    Caption = 'Stereo'
    TabOrder = 1
    OnClick = RBStereoClick
  end
end
