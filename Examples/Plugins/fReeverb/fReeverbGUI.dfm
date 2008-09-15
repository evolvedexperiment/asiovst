object FmReverb: TFmReverb
  Left = 401
  Top = 252
  BorderStyle = bsNone
  Caption = 'fReeverb'
  ClientHeight = 120
  ClientWidth = 355
  Color = 14803425
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 5855577
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object DialDry: TGuiDial
    Left = 16
    Top = 34
    Width = 48
    Height = 48
    LineWidth = 2
    LineColor = clWhite
    CircleColor = clSilver
    Max = 1.000000000000000000
    NumGlyphs = 64
    StitchKind = skVertical
    OnChange = DialDryChange
  end
  object DialWet: TGuiDial
    Left = 70
    Top = 34
    Width = 48
    Height = 48
    LineWidth = 2
    LineColor = clWhite
    CircleColor = clSilver
    Max = 1.000000000000000000
    NumGlyphs = 64
    StitchKind = skVertical
    OnChange = DialWetChange
  end
  object DialWidth: TGuiDial
    Left = 124
    Top = 34
    Width = 48
    Height = 48
    LineWidth = 2
    LineColor = clWhite
    CircleColor = clSilver
    Max = 1.000000000000000000
    NumGlyphs = 64
    StitchKind = skVertical
    OnChange = DialWidthChange
  end
  object DialDamp: TGuiDial
    Left = 178
    Top = 34
    Width = 48
    Height = 48
    LineWidth = 2
    LineColor = clWhite
    CircleColor = clSilver
    Max = 1.000000000000000000
    NumGlyphs = 64
    StitchKind = skVertical
    OnChange = DialDampChange
  end
  object DialRoomSize: TGuiDial
    Left = 232
    Top = 34
    Width = 48
    Height = 48
    LineWidth = 2
    LineColor = clWhite
    CircleColor = clSilver
    Max = 1.000000000000000000
    NumGlyphs = 64
    StitchKind = skVertical
    OnChange = DialRoomSizeChange
  end
  object DialStretch: TGuiDial
    Left = 286
    Top = 34
    Width = 48
    Height = 48
    LineWidth = 2
    LineColor = clWhite
    CircleColor = clSilver
    Max = 1.000000000000000000
    NumGlyphs = 64
    StitchKind = skVertical
    OnChange = DialStretchChange
  end
  object PnLabel: TGuiPanel
    Left = 16
    Top = 88
    Width = 325
    Height = 23
    AntiAlias = gaaLinear4x
    LineColor = clWhite
    PanelColor = clWhite
    ParentColor = True
    Radius = 9
    TabOrder = 2
    Transparent = True
    UseDockManager = True
    object LbDry: TGuiLabel
      Left = 0
      Top = 3
      Width = 48
      Height = 17
      Margins.Bottom = 0
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'dry'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LbWet: TGuiLabel
      Left = 54
      Top = 3
      Width = 48
      Height = 17
      Margins.Bottom = 0
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'wet'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LbWidth: TGuiLabel
      Left = 108
      Top = 3
      Width = 48
      Height = 17
      Margins.Bottom = 0
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'width'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LbSize: TGuiLabel
      Left = 216
      Top = 3
      Width = 48
      Height = 17
      Margins.Bottom = 0
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'size'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LbStretch: TGuiLabel
      Left = 270
      Top = 3
      Width = 48
      Height = 17
      Margins.Bottom = 0
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'stretch'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object LbDamp: TGuiLabel
      Left = 162
      Top = 3
      Width = 48
      Height = 17
      Margins.Bottom = 0
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'damp'
      Color = clWhite
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
  end
  object CBFreeze: TCheckBox
    Left = 116
    Top = 152
    Width = 57
    Height = 17
    Caption = 'FreeZe'
    TabOrder = 0
    OnClick = CBFreezeClick
  end
  object PnToolbar: TPanel
    Left = 0
    Top = 0
    Width = 355
    Height = 24
    Align = alTop
    BevelOuter = bvNone
    Color = clWhite
    TabOrder = 1
    object SBPreset: TGuiSelectBox
      Left = 51
      Top = 2
      Width = 156
      Height = 19
      Alignment = taLeftJustify
      AntiAlias = gaaLinear2x
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWhite
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      ItemIndex = -1
      Items.Strings = (
        'Random 23')
      LineColor = clSilver
      SelectBoxColor = 2960685
      Radius = 4
      OnChange = SBPresetChange
    end
    object LbPreset: TGuiLabel
      Left = 8
      Top = 2
      Width = 39
      Height = 17
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'preset'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
    end
    object BtAB: TGuiButton
      Left = 213
      Top = 2
      Width = 61
      Height = 19
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'a <> b'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      LineColor = 5855577
      ButtonColor = 14803425
      Radius = 4
    end
    object BtAbout: TGuiButton
      Left = 280
      Top = 2
      Width = 61
      Height = 19
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'about'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 5855577
      Font.Height = -13
      Font.Name = 'Tahoma'
      Font.Style = []
      LineColor = 5855577
      ButtonColor = 14803425
      Radius = 4
      OnClick = BtAboutClick
    end
  end
end
