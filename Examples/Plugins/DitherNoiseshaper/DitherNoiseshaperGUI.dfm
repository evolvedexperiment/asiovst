object FmDitherNoiseshaper: TFmDitherNoiseshaper
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'Dither & Noiseshaper Example'
  ClientHeight = 56
  ClientWidth = 249
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbNoiseshaperType: TLabel
    Left = 4
    Top = 34
    Width = 90
    Height = 13
    Caption = 'Noiseshaper Type:'
  end
  object LbFinalBitDepth: TLabel
    Left = 4
    Top = 7
    Width = 73
    Height = 13
    Caption = 'Final Bit Depth:'
  end
  object LbBit: TLabel
    Left = 156
    Top = 7
    Width = 12
    Height = 13
    Caption = 'Bit'
  end
  object CBNoiseshaperType: TComboBox
    Left = 100
    Top = 31
    Width = 145
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    ItemIndex = 0
    TabOrder = 0
    Text = '9th Order F-weighting'
    OnChange = CBNoiseshaperTypeChange
    Items.Strings = (
      '9th Order F-weighting'
      '3rd Order F-weighting'
      '2nd Order mod. E-weighting'
      '3rd Order mod. E-weighting'
      '9th Order mod. E-weighting'
      '5th Order improved E-weighting'
      '9th Order improved E-weighting'
      '2nd Order simple highpass')
  end
  object SEBitDepth: TSpinEdit
    Left = 100
    Top = 4
    Width = 49
    Height = 22
    MaxValue = 32
    MinValue = 2
    TabOrder = 1
    Value = 16
    OnChange = SEBitDepthChange
  end
end
