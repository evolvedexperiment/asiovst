object FmButterworth: TFmButterworth
  Left = 560
  Top = 65
  BorderStyle = bsNone
  Caption = 'Butterworth Lowpass Filter'
  ClientHeight = 243
  ClientWidth = 200
  Color = 657940
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 10526927
  Font.Height = -13
  Font.Name = 'Verdana'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    200
    243)
  PixelsPerInch = 96
  TextHeight = 16
  object LbButterworthFilterDemoShaddow: TGuiLabel
    Left = 9
    Top = 11
    Width = 186
    Height = 26
    AntiAlias = gaaLinear4x
    Caption = 'Butterworth HP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2039615
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
  object LbButterworthFilterDemo: TGuiLabel
    Left = 6
    Top = 8
    Width = 186
    Height = 26
    AntiAlias = gaaLinear4x
    Caption = 'Butterworth HP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10526927
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object PnControls: TGuiPanel
    Left = 8
    Top = 44
    Width = 184
    Height = 129
    LineColor = 10526927
    BorderWidth = 2.000000000000000000
    PanelColor = 1315880
    ParentColor = True
    Radius = 8.000000000000000000
    TabOrder = 0
    UseDockManager = True
    OnClick = PnControlsClick
    DesignSize = (
      184
      129)
    object DialFrequency: TGuiDial
      Left = 16
      Top = 33
      Width = 64
      Height = 64
      CircleColor = 1315880
      Color = 1315880
      CurveMapping = -2.099999904632568000
      DefaultPosition = 20000.000000000000000000
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      GlyphCount = 65
      OnChange = DialFrequencyChange
      OnDblClick = LbFrequencyValueDblClick
      ParentColor = False
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 20000.000000000000000000
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
      ParentFont = False
      Shadow.Color = clBlack
      OnDblClick = LbFrequencyValueDblClick
    end
    object DialOrder: TGuiDial
      Left = 104
      Top = 33
      Width = 64
      Height = 64
      CircleColor = 1315880
      Color = 1315880
      DefaultPosition = 4.000000000000000000
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = 16.000000000000000000
      GlyphCount = 65
      OnChange = DialOrderChange
      OnDblClick = LbOrderValueDblClick
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
      Left = 94
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
      ParentFont = False
      Shadow.Color = clBlack
      OnDblClick = LbOrderValueDblClick
    end
    object LbOrderValue: TGuiLabel
      Left = 94
      Top = 103
      Width = 80
      Height = 19
      Alignment = taCenter
      Anchors = [akRight, akBottom]
      AntiAlias = gaaLinear4x
      Caption = 'Order'
      Color = 1315880
      Shadow.Color = clBlack
      OnDblClick = LbOrderValueDblClick
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
      Shadow.Color = clBlack
      OnDblClick = LbFrequencyValueDblClick
    end
  end
  object GuiEQGraph: TGuiEQGraph
    Left = 8
    Top = 179
    Width = 184
    Height = 56
    AntiAlias = gaaLinear4x
    BorderColor = 10526927
    BorderRadius = 8
    BorderWidth = 2
    ColorChart = 1315880
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = 10526927
        OnGetFilterGain = GetFilterGain
      end>
    GraphColorDark = 4145018
    GraphColorLight = 2829139
    XAxis.LabelPosition = xlpBottom
    XAxis.UpperFrequency = 20000.000000000000000000
    XAxis.LowerFrequency = 20.000000000000000000
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -36.000000000000000000
    YAxis.UpperLevel = 6.000000000000000000
    YAxis.Granularity = 20.000000000000000000
    YAxis.MaximumGridLines = 3
    Anchors = [akLeft, akTop, akBottom]
    Color = 657940
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 6974133
    Font.Height = -7
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentColor = False
  end
  object Timer: TTimer
    Enabled = False
    Interval = 20
    OnTimer = EQGraphUpdateTimer
    Left = 96
    Top = 192
  end
end
