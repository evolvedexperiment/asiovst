object FmSplitter: TFmSplitter
  Left = 277
  Top = 185
  BorderStyle = bsNone
  Caption = 'Frequency Splitter'
  ClientHeight = 65
  ClientWidth = 588
  Color = 2830643
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object ShBorder: TShape
    Left = 261
    Top = 42
    Width = 33
    Height = 16
    Brush.Style = bsClear
  end
  object PnControl: TGuiPanel
    Left = 0
    Top = 0
    Width = 588
    Height = 37
    Align = alTop
    BorderVisible = False
    Caption = 'PnControl'
    Color = 7701642
    LineColor = clBlack
    Linewidth = 0
    PanelColor = 7701642
    Radius = 0
    TabOrder = 0
    UseDockManager = True
    object DialSplitFrequency: TGuiDial
      Left = 105
      Top = 9
      Width = 20
      Height = 20
      LineWidth = 1
      LineColor = clBlack
      CircleColor = 6450289
      AntiAlias = gaaLinear4x
      CurveMapping = -2.099999904632568000
      DefaultPosition = 100.000000000000000000
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialSplitFrequencyChange
    end
    object LbSplitFrequency: TGuiLabel
      Left = 129
      Top = 9
      Width = 59
      Height = 20
      AntiAlias = gaaLinear2x
      Caption = '1kHz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
    end
    object DialSplitOrder: TGuiDial
      Left = 194
      Top = 9
      Width = 20
      Height = 20
      LineWidth = 1
      LineColor = clBlack
      CircleColor = 6450289
      AntiAlias = gaaLinear4x
      CurveMapping = -1.250000000000000000
      DefaultPosition = 8.000000000000000000
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 8.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialSplitOrderChange
    end
    object LbSplitOrder: TGuiLabel
      Left = 219
      Top = 9
      Width = 39
      Height = 20
      AntiAlias = gaaLinear2x
      Caption = '4x'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
    end
    object GuiLEDOversampling: TGuiLED
      Left = 257
      Top = 9
      Width = 20
      Height = 20
      OnClick = GuiLEDOversamplingClick
      Brightness_Percent = 20.000000000000000000
      LineWidth = 2
      LEDColor = clLime
      AntiAlias = gaaLinear4x
      LineColor = clLime
    end
    object LbOversampling: TGuiLabel
      Left = 279
      Top = 9
      Width = 97
      Height = 20
      AntiAlias = gaaLinear4x
      Caption = 'Oversampling:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      OnClick = GuiLEDOversamplingClick
    end
    object LbOversamplingFactor: TGuiLabel
      Left = 411
      Top = 9
      Width = 33
      Height = 20
      AntiAlias = gaaLinear2x
      Caption = '4x'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      Visible = False
    end
    object DialOversampling: TGuiDial
      Left = 386
      Top = 9
      Width = 20
      Height = 20
      Visible = False
      LineWidth = 1
      LineColor = clBlack
      CircleColor = 6450289
      AntiAlias = gaaLinear4x
      CurveMapping = -1.250000000000000000
      DefaultPosition = 4.000000000000000000
      Max = 16.000000000000000000
      Min = 1.000000000000000000
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 4.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skHorizontal
      OnChange = DialOversamplingChange
    end
    object BtLow: TGuiButton
      Left = 447
      Top = 6
      Width = 65
      Height = 26
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'Low'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      LineColor = clBlack
      LineWidth = 2
      ButtonColor = 1625885
      Radius = 4
      OnClick = BtLowClick
    end
    object BtHigh: TGuiButton
      Left = 518
      Top = 6
      Width = 65
      Height = 26
      AntiAlias = gaaLinear4x
      Alignment = taCenter
      Caption = 'High'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      LineColor = clBlack
      LineWidth = 2
      ButtonColor = 6450289
      Radius = 4
      OnClick = BtHighClick
    end
    object SBMode: TGuiSelectBox
      Left = 8
      Top = 6
      Width = 89
      Height = 26
      AntiAlias = gaaLinear4x
      ArrowColor = 6450289
      ArrowWidth = 1
      ButtonColor = 6450289
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -16
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      ItemIndex = 0
      Items.Strings = (
        'Split A'
        'Split B'
        'Dyn'
        'L/R'
        'M/S'
        'Serial')
      LineColor = clBlack
      LineWidth = 2
      SelectBoxColor = 7701642
      Radius = 4
      OnChange = SBModeChange
    end
  end
  object PnGui: TPanel
    Left = 262
    Top = 43
    Width = 31
    Height = 14
    BevelOuter = bvNone
    TabOrder = 1
  end
end
