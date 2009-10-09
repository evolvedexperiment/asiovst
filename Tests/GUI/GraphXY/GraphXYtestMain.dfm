object FmGraphXY: TFmGraphXY
  Left = 218
  Top = 77
  Caption = 'GraphXY Test'
  ClientHeight = 430
  ClientWidth = 414
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object GraphXYA: TGuiGraphXY
    Left = 8
    Top = 8
    Width = 196
    Height = 196
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clBlue
      end>
    XAxis.Granularity = 2.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 2.000000000000000000
    YAxis.Minimum = -5.000000000000000000
    YAxis.Maximum = 5.000000000000000000
    YAxis.Lower = -5.000000000000000000
    YAxis.Upper = 5.000000000000000000
    LineColor = clMaroon
    LineWidth = 2
    Transparent = True
  end
  object GraphXYB: TGuiGraphXY
    Left = 210
    Top = 8
    Width = 196
    Height = 196
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clYellow
      end>
    XAxis.Granularity = 2.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 2.000000000000000000
    YAxis.Minimum = -5.000000000000000000
    YAxis.Maximum = 5.000000000000000000
    YAxis.Lower = -5.000000000000000000
    YAxis.Upper = 5.000000000000000000
    AntiAlias = gaaLinear2x
    LineColor = clMaroon
    LineWidth = 2
    Transparent = True
  end
  object GraphXYC: TGuiGraphXY
    Left = 8
    Top = 210
    Width = 196
    Height = 196
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
        Series.Color = clLime
      end>
    XAxis.Granularity = 2.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 2.000000000000000000
    YAxis.Minimum = -5.000000000000000000
    YAxis.Maximum = 5.000000000000000000
    YAxis.Lower = -5.000000000000000000
    YAxis.Upper = 5.000000000000000000
    AntiAlias = gaaLinear3x
    LineColor = clMaroon
    LineWidth = 2
    Transparent = True
  end
  object GraphXYD: TGuiGraphXY
    Left = 210
    Top = 210
    Width = 196
    Height = 196
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
      end>
    XAxis.Granularity = 2.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 2.000000000000000000
    YAxis.Minimum = -5.000000000000000000
    YAxis.Maximum = 5.000000000000000000
    YAxis.Lower = -5.000000000000000000
    YAxis.Upper = 5.000000000000000000
    AntiAlias = gaaLinear4x
    LineColor = clMaroon
    LineWidth = 2
    Transparent = True
  end
end
