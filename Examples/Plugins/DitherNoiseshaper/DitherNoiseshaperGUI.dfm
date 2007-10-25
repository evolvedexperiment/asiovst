object FmDitherNoiseshaper: TFmDitherNoiseshaper
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'FmDitherNoiseshaper'
  ClientHeight = 30
  ClientWidth = 255
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
    Left = 8
    Top = 8
    Width = 90
    Height = 13
    Caption = 'Noiseshaper Type:'
  end
  object CBNoiseshaperType: TComboBox
    Left = 104
    Top = 5
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
end
