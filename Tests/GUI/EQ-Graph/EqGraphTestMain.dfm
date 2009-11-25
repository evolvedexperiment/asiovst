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
    AutoColor = False
    AutoUpdate = False
    GraphColorDark = clBlue
    ColorChart = clBtnFace
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    XAxis.UpperFrequency = 9000.000000000000000000
    XAxis.LowerFrequency = 100.000000000000000000
    OnGetFilterGain = EqGraphGetFilterGain
  end
  object EqGraphB: TGuiEQGraph
    Left = 8
    Top = 256
    Width = 324
    Height = 242
    AntiAlias = gaaLinear3x
    AutoColor = False
    AutoUpdate = False
    GraphColorDark = clLime
    ColorChart = clBtnFace
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    XAxis.UpperFrequency = 9000.000000000000000000
    XAxis.LowerFrequency = 100.000000000000000000
    OnGetFilterGain = EqGraphGetFilterGain
    Anchors = [akLeft, akBottom]
  end
  object EqGraphC: TGuiEQGraph
    Left = 338
    Top = 8
    Width = 324
    Height = 242
    AntiAlias = gaaLinear2x
    AutoColor = False
    AutoUpdate = False
    GraphColorDark = clYellow
    ColorChart = clBtnFace
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    XAxis.UpperFrequency = 9000.000000000000000000
    XAxis.LowerFrequency = 100.000000000000000000
    OnGetFilterGain = EqGraphGetFilterGain
    Anchors = [akTop, akRight]
  end
  object EqGraphD: TGuiEQGraph
    Left = 338
    Top = 256
    Width = 324
    Height = 242
    AntiAlias = gaaLinear4x
    AutoColor = False
    AutoUpdate = False
    GraphColorDark = clRed
    ColorChart = clBtnFace
    YAxis.LowerLevel = -40.000000000000000000
    YAxis.UpperLevel = 15.000000000000000000
    XAxis.UpperFrequency = 9000.000000000000000000
    XAxis.LowerFrequency = 100.000000000000000000
    OnGetFilterGain = EqGraphGetFilterGain
    Anchors = [akRight, akBottom]
  end
end
