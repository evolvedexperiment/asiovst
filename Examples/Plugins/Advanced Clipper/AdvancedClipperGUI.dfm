object FmAdvancedClipper: TFmAdvancedClipper
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'Advanced Clipper'
  ClientHeight = 234
  ClientWidth = 217
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Times New Roman'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 14
  object GpStage1: TGuiGroup
    Left = 8
    Top = 8
    Width = 201
    Height = 105
    AntiAlias = gaaLinear4x
    Caption = ' Stage 1 '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    HeaderMinWidth = 64
    LineColor = 15659506
    LineWidth = 3
    ParentFont = False
    Radius = 7
    TabOrder = 0
    object DialInputGain: TGuiDial
      Left = 11
      Top = 33
      Width = 48
      Height = 48
      CircleColor = 3226174
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 6.000000000000000000
      Min = -6.000000000000000000
      NumGlyphs = 65
      OnChange = DialInputGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
    end
    object DialOSFactor1: TGuiDial
      Left = 75
      Top = 33
      Width = 48
      Height = 48
      CircleColor = 3226174
      DefaultPosition = 1.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      NumGlyphs = 65
      OnChange = DialOSFactor1Change
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
    end
    object DialFilterOrder1: TGuiDial
      Left = 139
      Top = 33
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -1.000000000000000000
      DefaultPosition = 1.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 64.000000000000000000
      NumGlyphs = 65
      OnChange = DialFilterOrder1Change
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 2.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
    end
    object LbInputGain: TGuiLabel
      Left = 11
      Top = 85
      Width = 48
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Input Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbOSFactor: TGuiLabel
      Left = 75
      Top = 85
      Width = 48
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'OS Factor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbFilterOrder: TGuiLabel
      Left = 137
      Top = 85
      Width = 52
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Filter Order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object PnDisplay: TGuiPanel
      Left = 75
      Top = 7
      Width = 114
      Height = 17
      AntiAlias = gaaLinear4x
      Caption = 'PnDisplay'
      LineColor = 5398887
      Linewidth = 1
      PanelColor = 3226174
      ParentColor = True
      Radius = 5
      TabOrder = 0
      UseDockManager = True
      Transparent = True
      object LbDisplay: TGuiLabel
        Left = 5
        Top = 2
        Width = 105
        Height = 12
        Alignment = taCenter
        AntiAlias = gaaLinear4x
        Caption = 'Advanced Clipper'
        Color = 3226174
        Font.Charset = ANSI_CHARSET
        Font.Color = 15659506
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
      end
    end
  end
  object GpStage2: TGuiGroup
    Left = 8
    Top = 119
    Width = 201
    Height = 106
    AntiAlias = gaaLinear4x
    Caption = ' Stage 2 '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 7373965
    Font.Height = -16
    Font.Name = 'Times New Roman'
    Font.Style = [fsBold]
    HeaderMinWidth = 64
    LineColor = 15659506
    LineWidth = 3
    ParentFont = False
    Radius = 7
    TabOrder = 1
    object DialOSFactor2: TGuiDial
      Left = 11
      Top = 33
      Width = 48
      Height = 48
      CircleColor = 3226174
      DefaultPosition = 1.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      NumGlyphs = 65
      OnChange = DialOSFactor2Change
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
    end
    object LbOSFactor2: TGuiLabel
      Left = 11
      Top = 85
      Width = 48
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'OS Factor'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object LbFilterOrder2: TGuiLabel
      Left = 73
      Top = 85
      Width = 52
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Filter Order'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object DialFilterOrder2: TGuiDial
      Left = 75
      Top = 33
      Width = 48
      Height = 48
      CircleColor = 3226174
      CurveMapping = -1.000000000000000000
      DefaultPosition = 1.000000000000000000
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 64.000000000000000000
      NumGlyphs = 65
      OnChange = DialFilterOrder2Change
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 2.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
    end
    object DialOutputGain: TGuiDial
      Left = 139
      Top = 33
      Width = 48
      Height = 48
      CircleColor = 3226174
      DialImageIndex = -1
      LineColor = 15659506
      LineWidth = 2
      Max = 6.000000000000000000
      Min = -6.000000000000000000
      NumGlyphs = 65
      OnChange = DialOutputGainChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      ScrollRange_Pixel = 200.000000000000000000
      StitchKind = skHorizontal
    end
    object LbOutputGain: TGuiLabel
      Left = 136
      Top = 85
      Width = 54
      Height = 15
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Output Gain'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 15659506
      Font.Height = -11
      Font.Name = 'Times New Roman'
      Font.Style = []
    end
    object GuiPanel1: TGuiPanel
      Left = 80
      Top = 7
      Width = 100
      Height = 19
      AntiAlias = gaaLinear4x
      Caption = 'PnDisplay'
      LineColor = 5925488
      Linewidth = 1
      PanelColor = 4938079
      ParentColor = True
      Radius = 5
      TabOrder = 0
      UseDockManager = True
      Transparent = True
      object LbHardClip: TGuiLabel
        Left = 26
        Top = 3
        Width = 68
        Height = 14
        Alignment = taCenter
        AntiAlias = gaaLinear4x
        Caption = 'hard clip output'
        Color = 4938079
        Font.Charset = DEFAULT_CHARSET
        Font.Color = 15659506
        Font.Height = -11
        Font.Name = 'Times New Roman'
        Font.Style = []
        OnClick = LbHardClipClick
      end
      object LEDHardClip: TGuiLED
        Left = 5
        Top = 2
        Width = 15
        Height = 15
        OnClick = LbHardClipClick
        Brightness_Percent = 90.000000000000000000
        Color = 4938079
        LineWidth = 2
        LEDColor = 14870505
        AntiAlias = gaaLinear4x
        LineColor = 3226174
      end
    end
  end
end
