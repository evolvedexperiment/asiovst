object FmOversampler: TFmOversampler
  Left = 277
  Top = 185
  BorderStyle = bsNone
  Caption = 'Frequency Oversampler'
  ClientHeight = 54
  ClientWidth = 187
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
    Left = 77
    Top = 30
    Width = 33
    Height = 16
    Brush.Style = bsClear
  end
  object PnControl: TGuiPanel
    Left = 0
    Top = 0
    Width = 187
    Height = 24
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
    object GuiLEDOversampling: TGuiLED
      Left = 4
      Top = 4
      Width = 16
      Height = 16
      OnClick = GuiLEDOversamplingClick
      Brightness_Percent = 20.000000000000000000
      LineWidth = 2
      LEDColor = clLime
      AntiAlias = gaaLinear4x
      LineColor = clLime
    end
    object LbOversampling: TGuiLabel
      Left = 26
      Top = 4
      Width = 80
      Height = 16
      AntiAlias = gaaLinear4x
      Caption = 'Oversampling:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      OnClick = GuiLEDOversamplingClick
    end
    object LbOversamplingFactor: TGuiLabel
      Left = 134
      Top = 4
      Width = 27
      Height = 16
      AntiAlias = gaaLinear2x
      Caption = '4x'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -13
      Font.Name = 'Times New Roman'
      Font.Style = [fsBold]
      Visible = False
    end
    object DialOversampling: TGuiDial
      Left = 115
      Top = 4
      Width = 16
      Height = 16
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
  end
  object PnGui: TPanel
    Left = 78
    Top = 31
    Width = 31
    Height = 14
    BevelOuter = bvNone
    Color = 2830643
    TabOrder = 1
  end
end
