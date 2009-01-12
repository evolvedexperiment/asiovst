object FmButterworth: TFmButterworth
  Left = 560
  Top = 65
  BorderStyle = bsNone
  Caption = 'Butterworth Lowpass Filter'
  ClientHeight = 181
  ClientWidth = 200
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
    200
    181)
  PixelsPerInch = 96
  TextHeight = 13
  object LbButterworthFilterDemoShaddow: TGuiLabel
    Left = 9
    Top = 11
    Width = 184
    Height = 26
    AntiAlias = gaaLinear4x
    Caption = 'Butterworth LP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 2039615
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
  end
  object LbButterworthFilterDemo: TGuiLabel
    Left = 6
    Top = 8
    Width = 184
    Height = 26
    AntiAlias = gaaLinear4x
    Caption = 'Butterworth LP'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10526927
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    Transparent = True
  end
  object PnControls: TGuiPanel
    Left = 8
    Top = 44
    Width = 184
    Height = 129
    Anchors = [akLeft, akTop, akBottom]
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
      DefaultPosition = 20.000000000000000000
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      NumGlyphs = 65
      OnChange = DialFrequencyChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 1000.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
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
    object DialOrder: TGuiDial
      Left = 104
      Top = 33
      Width = 64
      Height = 64
      CircleColor = 1315880
      Color = 1315880
      DialImageIndex = -1
      LineColor = 10526927
      LineWidth = 2
      Max = 16.000000000000000000
      NumGlyphs = 65
      OnChange = DialOrderChange
      PointerAngles.Start = 225
      PointerAngles.Range = 270
      PointerAngles.Resolution = 270.000000000000000000
      Position = 4.000000000000000000
      ScrollRange_Pixel = 400.000000000000000000
      StitchKind = skVertical
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
