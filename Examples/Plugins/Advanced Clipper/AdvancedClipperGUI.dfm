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
      LineWidth = 2
      LineColor = 15659506
      CircleColor = 3226174
      Min = -6.000000000000000000
      Max = 6.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 200.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialInputGainChange
    end
    object DialOSFactor1: TGuiDial
      Left = 75
      Top = 33
      Width = 48
      Height = 48
      LineWidth = 2
      LineColor = 15659506
      CircleColor = 3226174
      Position = 1.000000000000000000
      DefaultPosition = 1.000000000000000000
      Min = 1.000000000000000000
      Max = 16.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 200.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialOSFactor1Change
    end
    object DialFilterOrder1: TGuiDial
      Left = 139
      Top = 33
      Width = 48
      Height = 48
      LineWidth = 2
      LineColor = 15659506
      CircleColor = 3226174
      CurveMapping = -1.000000000000000000
      Position = 2.000000000000000000
      DefaultPosition = 1.000000000000000000
      Max = 64.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 200.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialFilterOrder1Change
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
    object PnDisplay1: TGuiPanel
      Left = 75
      Top = 7
      Width = 114
      Height = 17
      AntiAlias = gaaLinear4x
      Caption = 'PnDisplay1'
      LineColor = 5398887
      Linewidth = 1
      PanelColor = 3226174
      ParentColor = True
      Radius = 5
      TabOrder = 0
      Transparent = True
      UseDockManager = True
      object LbDisplay1: TGuiLabel
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
      LineWidth = 2
      LineColor = 15659506
      CircleColor = 3226174
      Position = 1.000000000000000000
      DefaultPosition = 1.000000000000000000
      Min = 1.000000000000000000
      Max = 16.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 200.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialOSFactor2Change
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
      LineWidth = 2
      LineColor = 15659506
      CircleColor = 3226174
      CurveMapping = -1.000000000000000000
      Position = 2.000000000000000000
      DefaultPosition = 1.000000000000000000
      Max = 64.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 200.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialFilterOrder2Change
    end
    object DialOutputGain: TGuiDial
      Left = 139
      Top = 33
      Width = 48
      Height = 48
      LineWidth = 2
      LineColor = 15659506
      CircleColor = 3226174
      Min = -6.000000000000000000
      Max = 6.000000000000000000
      NumGlyphs = 65
      StitchKind = skHorizontal
      ScrollRange_Pixel = 200.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      OnChange = DialOutputGainChange
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
    object PnDisplay2: TGuiPanel
      Left = 75
      Top = 7
      Width = 114
      Height = 17
      AntiAlias = gaaLinear4x
      Caption = 'GuiPanel1'
      LineColor = 5398887
      Linewidth = 1
      PanelColor = 3226174
      ParentColor = True
      Radius = 5
      TabOrder = 0
      Transparent = True
      UseDockManager = True
      object LbDisplay2: TGuiLabel
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
end
