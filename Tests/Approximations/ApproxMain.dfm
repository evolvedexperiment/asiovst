object FmApproximationBenchmark: TFmApproximationBenchmark
  Left = 286
  Top = 77
  Caption = 'Approximation Benchmark'
  ClientHeight = 247
  ClientWidth = 670
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object GuiGraphXY: TGuiGraphXY
    Left = 284
    Top = 0
    Width = 386
    Height = 247
    SeriesCollection = <
      item
        DisplayName = 'TGuiGraphXYSeriesCollectionItem'
        SeriesClassName = 'TGuiGraphXYFunctionSeries'
      end>
    XAxis.Granularity = 1.000000000000000000
    XAxis.Minimum = -5.000000000000000000
    XAxis.Maximum = 5.000000000000000000
    XAxis.Lower = -5.000000000000000000
    XAxis.Upper = 5.000000000000000000
    YAxis.Granularity = 0.050000000000000000
    YAxis.Minimum = -0.100000000000000000
    YAxis.Maximum = 0.100000000000000000
    YAxis.Lower = -0.100000000000000000
    YAxis.Upper = 0.100000000000000000
    Align = alClient
    LineColor = clMaroon
    LineWidth = 2
    ExplicitLeft = 280
    ExplicitTop = 184
    ExplicitWidth = 100
    ExplicitHeight = 40
  end
  object Splitter: TSplitter
    Left = 281
    Top = 0
    Height = 247
    ExplicitLeft = 280
    ExplicitTop = 136
    ExplicitHeight = 100
  end
  object Memo: TMemo
    Left = 0
    Top = 0
    Width = 281
    Height = 247
    Align = alLeft
    TabOrder = 0
    OnClick = MemoClick
    ExplicitHeight = 343
  end
end
