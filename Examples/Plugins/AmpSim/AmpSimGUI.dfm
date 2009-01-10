object FmCombo: TFmCombo
  Left = 338
  Top = 158
  BorderStyle = bsNone
  Caption = 'Combo'
  ClientHeight = 171
  ClientWidth = 376
  Color = 4227200
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clMaroon
  Font.Height = -11
  Font.Name = 'Trebuchet MS'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 16
  object SBModel: TGuiSelectBox
    Left = 82
    Top = 8
    Width = 183
    Height = 25
    AntiAlias = gaaLinear4x
    ArrowColor = 4227200
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
    ParentFont = False
    Radius = 8
    SelectBoxColor = 4227200
    OnChange = SBModelChange
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
    Transparent = True
  end
  object GuiLED: TGuiLED
    Left = 271
    Top = 8
    Width = 25
    Height = 25
    Brightness_Percent = 100.000000000000000000
    LineWidth = 2
    LEDColor = 192
    AntiAlias = gaaLinear4x
    LineColor = 192
    OnClick = LbStereoClick
  end
  object LbStereo: TGuiLabel
    Left = 302
    Top = 8
    Width = 67
    Height = 25
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Stereo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clMaroon
    Font.Height = -21
    Font.Name = 'Trebuchet MS'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbStereoClick
  end
  object GuiPanel1: TGuiPanel
    Left = 8
    Top = 39
    Width = 361
    Height = 125
    AntiAlias = gaaLinear4x
    Caption = 'GuiPanel1'
    LineColor = clBlack
    PanelColor = 4227200
    ParentColor = True
    Radius = 8
    TabOrder = 0
    UseDockManager = True
    Transparent = True
    object LbResonanceValue: TLabel
      Left = 288
      Top = 105
      Width = 64
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '0.0'
    end
    object LbFrequencyValue: TLabel
      Left = 218
      Top = 105
      Width = 64
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '0.0'
    end
    object LbOutputValue: TLabel
      Left = 148
      Top = 105
      Width = 64
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '0.0'
    end
    object LbBiasValue: TLabel
      Left = 78
      Top = 105
      Width = 64
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '0.0'
    end
    object LbDriveValue: TLabel
      Left = 8
      Top = 105
      Width = 64
      Height = 16
      Alignment = taCenter
      AutoSize = False
      Caption = '0.0'
    end
    object DialDrive: TGuiDial
      Left = 8
      Top = 35
      Width = 64
      Height = 64
      DialImageIndex = -1
      LineColor = clMaroon
      LineWidth = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      NumGlyphs = 65
      OnChange = DialDriveChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbDrive: TGuiLabel
      Left = 8
      Top = 8
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
      Top = 35
      Width = 64
      Height = 64
      DialImageIndex = -1
      LineColor = clMaroon
      LineWidth = 2
      Max = 100.000000000000000000
      Min = -100.000000000000000000
      NumGlyphs = 65
      OnChange = DialBiasChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbBias: TGuiLabel
      Left = 78
      Top = 8
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
      Top = 35
      Width = 64
      Height = 64
      DialImageIndex = -1
      LineColor = clMaroon
      LineWidth = 2
      Max = 20.000000000000000000
      Min = -20.000000000000000000
      NumGlyphs = 65
      OnChange = DialOutputChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object DialFrequency: TGuiDial
      Left = 218
      Top = 35
      Width = 64
      Height = 64
      CurveMapping = -1.750000000000000000
      DefaultPosition = 100.000000000000000000
      DialImageIndex = -1
      LineColor = clMaroon
      LineWidth = 2
      Max = 10000.000000000000000000
      Min = 10.000000000000000000
      NumGlyphs = 65
      OnChange = DialFreqChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object DialResonance: TGuiDial
      Left = 288
      Top = 35
      Width = 64
      Height = 64
      DialImageIndex = -1
      LineColor = clMaroon
      LineWidth = 2
      Max = 100.000000000000000000
      NumGlyphs = 65
      OnChange = DialResoChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
    end
    object LbOutput: TGuiLabel
      Left = 148
      Top = 8
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
      Top = 8
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
      Top = 8
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
  end
end
