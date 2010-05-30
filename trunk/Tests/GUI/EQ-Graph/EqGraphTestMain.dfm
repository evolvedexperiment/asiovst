object FmEqGraphTest: TFmEqGraphTest
  Left = 218
  Top = 77
  Caption = 'EQ-Graph Test'
  ClientHeight = 506
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  DesignSize = (
    670
    506)
  PixelsPerInch = 96
  TextHeight = 13
  object EqGraphA: TGuiEQGraph
    Left = 8
    Top = 8
    Width = 324
    Height = 242
    GraphColorDark = 2763306
    ColorChart = 15133420
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clBlue
        OnGetFilterGain = GetFilterGain
      end
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clNavy
        LineWidth = 1
        OnGetFilterGain = GetFilterSubGain
      end>
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    YAxis.Granularity = 6.000000000000000000
    XAxis.LabelPosition = xlpBottom
    XAxis.UpperFrequency = 13000.000000000000000000
    XAxis.LowerFrequency = 80.000000000000000000
    Color = clBtnFace
    ParentColor = False
  end
  object EqGraphB: TGuiEQGraph
    Left = 8
    Top = 256
    Width = 324
    Height = 242
    AntiAlias = gaaLinear3x
    GraphColorDark = 2763306
    ColorChart = 15133420
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clLime
        OnGetFilterGain = GetFilterGain
      end
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clGreen
        LineWidth = 1
        OnGetFilterGain = GetFilterSubGain
      end>
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    YAxis.Granularity = 6.000000000000000000
    XAxis.LabelPosition = xlpBottom
    XAxis.UpperFrequency = 13000.000000000000000000
    XAxis.LowerFrequency = 80.000000000000000000
    Anchors = [akLeft, akBottom]
    Color = clBtnFace
    ParentColor = False
  end
  object EqGraphC: TGuiEQGraph
    Left = 338
    Top = 8
    Width = 324
    Height = 242
    AntiAlias = gaaLinear2x
    GraphColorDark = 2763306
    ColorChart = 15133420
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clYellow
        OnGetFilterGain = GetFilterGain
      end
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clOlive
        LineWidth = 1
        OnGetFilterGain = GetFilterSubGain
      end>
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    YAxis.Granularity = 6.000000000000000000
    XAxis.LabelPosition = xlpBottom
    XAxis.UpperFrequency = 13000.000000000000000000
    XAxis.LowerFrequency = 80.000000000000000000
    Anchors = [akTop, akRight]
    Color = clBtnFace
    ParentColor = False
  end
  object EqGraphD: TGuiEQGraph
    Left = 338
    Top = 256
    Width = 324
    Height = 242
    AntiAlias = gaaLinear4x
    GraphColorDark = 2763306
    ColorChart = 15133420
    FilterSeries = <
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        OnGetFilterGain = GetFilterGain
      end
      item
        DisplayName = 'TGuiEQGraphSeriesCollectionItem'
        Color = clMaroon
        LineWidth = 1
        OnGetFilterGain = GetFilterSubGain
      end>
    YAxis.LabelPosition = ylpLeft
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    YAxis.Granularity = 6.000000000000000000
    XAxis.LabelPosition = xlpBottom
    XAxis.UpperFrequency = 13000.000000000000000000
    XAxis.LowerFrequency = 80.000000000000000000
    Anchors = [akRight, akBottom]
    Color = clBtnFace
    ParentColor = False
  end
end
