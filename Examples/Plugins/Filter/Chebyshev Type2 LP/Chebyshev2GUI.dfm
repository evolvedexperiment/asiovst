object FmChebyshev: TFmChebyshev
  Left = 518
  Top = 77
  BorderStyle = bsNone
  Caption = 'Chebyshev Lowpass Filter'
  ClientHeight = 181
  ClientWidth = 290
  Color = 657940
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    290
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object LbChebyshevFilterDemoShaddow: TGuiLabel
    Left = 11
    Top = 11
    Width = 273
    Height = 26
    AntiAlias = gaaLinear4x
    Caption = 'Chebyshev Filter Demo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2039615
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
  end
  object LbChebyshevFilterDemo: TGuiLabel
    Left = 8
    Top = 8
    Width = 274
    Height = 26
    AntiAlias = gaaLinear4x
    Caption = 'Chebyshev Filter Demo'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10526927
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Transparent = True
  end
  object PnControls: TGuiPanel
    Left = 8
    Top = 43
    Width = 274
    Height = 129
    Anchors = [akLeft, akTop, akRight, akBottom]
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10526927
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = 10526927
    Linewidth = 3
    PanelColor = 1315880
    ParentColor = True
    Radius = 8
    TabOrder = 0
    UseDockManager = True
    DesignSize = (
      274
      129)
    object DialFrequency: TGuiDial
      Left = 16
      Top = 33
      Width = 64
      Height = 64
      CircleColor = 657940
      Color = 1315880
      CurveMapping = -2.099999904632568000
      DefaultPosition = 1000.000000000000000000
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      NumGlyphs = 65
      OnChange = DialFrequencyChange
      ParentColor = False
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      WheelStep = 1.000000000000000000
    end
    object DialStopband: TGuiDial
      Left = 102
      Top = 33
      Width = 64
      Height = 64
      CircleColor = 657940
      Color = 1315880
      CurveMapping = 1.330000042915344000
      DefaultPosition = -24.000000000000000000
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = -0.009999999776482582
      Min = -100.000000000000000000
      NumGlyphs = 65
      OnChange = DialStopbandChange
      ParentColor = False
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = -24.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      WheelStep = 1.000000000000000000
    end
    object LbFrequency: TGuiLabel
      Left = 8
      Top = 8
      Width = 80
      Height = 19
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Color = 1315880
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 10526927
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
    end
    object LbStopband: TGuiLabel
      Left = 94
      Top = 8
      Width = 80
      Height = 19
      Alignment = taCenter
      Anchors = [akTop]
      AntiAlias = gaaLinear4x
      Caption = 'Stopband'
      Color = 1315880
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 10526927
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
    end
    object DialOrder: TGuiDial
      Left = 190
      Top = 33
      Width = 64
      Height = 64
      CircleColor = 657940
      Color = 1315880
      DefaultPosition = 4.000000000000000000
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = 16.000000000000000000
      NumGlyphs = 65
      OnChange = DialOrderChange
      ParentColor = False
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 4.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
      WheelStep = 1.000000000000000000
    end
    object LbOrder: TGuiLabel
      Left = 182
      Top = 8
      Width = 80
      Height = 19
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Color = 1315880
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 10526927
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
    end
    object LbOrderValue: TGuiLabel
      Left = 182
      Top = 103
      Width = 80
      Height = 19
      Alignment = taCenter
      Anchors = [akRight, akBottom]
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Color = 1315880
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 10526927
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
    end
    object LbStopbandValue: TGuiLabel
      Left = 94
      Top = 103
      Width = 80
      Height = 19
      Alignment = taCenter
      Anchors = [akBottom]
      AntiAlias = gaaLinear4x
      Caption = 'Stopband'
      Color = 1315880
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 10526927
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
    end
    object LbFrequencyValue: TGuiLabel
      Left = 8
      Top = 103
      Width = 80
      Height = 19
      Alignment = taCenter
      Anchors = [akLeft, akBottom]
      AntiAlias = gaaLinear4x
      Caption = 'Frequency'
      Color = 1315880
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 10526927
      Font.Height = -13
      Font.Name = 'Verdana'
      Font.Style = [fsBold]
    end
  end
end
