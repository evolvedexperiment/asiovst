object FmModDelay2: TFmModDelay2
  Left = 218
  Top = 77
  BorderStyle = bsNone
  Caption = 'Mod Delay II'
  ClientHeight = 232
  ClientWidth = 346
  Color = 7373964
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object LbGain: TGuiLabel
    Left = 8
    Top = 40
    Width = 41
    Height = 17
    Oversampling = fo4x
    Caption = 'Gain:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbMix: TGuiLabel
    Left = 8
    Top = 63
    Width = 41
    Height = 17
    Oversampling = fo4x
    Caption = 'Mix:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbLpf: TGuiLabel
    Left = 8
    Top = 86
    Width = 41
    Height = 17
    Oversampling = fo4x
    Caption = 'LPF:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbDelay: TGuiLabel
    Left = 8
    Top = 109
    Width = 41
    Height = 17
    Oversampling = fo4x
    Caption = 'Delay:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbDepth: TGuiLabel
    Left = 8
    Top = 132
    Width = 46
    Height = 17
    Oversampling = fo4x
    Caption = 'Depth:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbRate: TGuiLabel
    Left = 8
    Top = 155
    Width = 41
    Height = 17
    Oversampling = fo4x
    Caption = 'Rate:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbFeedback: TGuiLabel
    Left = 8
    Top = 178
    Width = 69
    Height = 17
    Oversampling = fo4x
    Caption = 'Feedback:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbLeft: TGuiLabel
    Left = 117
    Top = 8
    Width = 52
    Height = 25
    Oversampling = fo4x
    Caption = 'LEFT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbRight: TGuiLabel
    Left = 248
    Top = 8
    Width = 73
    Height = 25
    Oversampling = fo4x
    Caption = 'RIGHT'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object LbCurrentValue: TGuiLabel
    Left = 83
    Top = 207
    Width = 97
    Height = 17
    Oversampling = fo4x
    Caption = 'Current Value:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
  end
  object SBGainLeft: TScrollBar
    Left = 83
    Top = 40
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 0
  end
  object SBMixLeft: TScrollBar
    Left = 83
    Top = 63
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 1
  end
  object SBLPFLeft: TScrollBar
    Left = 83
    Top = 86
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 2
  end
  object SBDelayLeft: TScrollBar
    Left = 83
    Top = 109
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 3
  end
  object SbDepthLeft: TScrollBar
    Left = 83
    Top = 132
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 4
  end
  object SBRateLeft: TScrollBar
    Left = 83
    Top = 155
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 5
  end
  object SBFeedbackLeft: TScrollBar
    Left = 83
    Top = 178
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 6
  end
  object SBGainRight: TScrollBar
    Left = 213
    Top = 40
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 7
  end
  object SBMixRight: TScrollBar
    Left = 213
    Top = 63
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 8
  end
  object SBLpfRight: TScrollBar
    Left = 213
    Top = 86
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 9
  end
  object SBdelayRight: TScrollBar
    Left = 213
    Top = 109
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 10
  end
  object SBDepthRight: TScrollBar
    Left = 213
    Top = 132
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 11
  end
  object SBRateRight: TScrollBar
    Left = 213
    Top = 155
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 12
  end
  object SBFeedbackRight: TScrollBar
    Left = 213
    Top = 178
    Width = 124
    Height = 17
    PageSize = 0
    TabOrder = 13
  end
end
