object FmASIOVST: TFmASIOVST
  Left = 335
  Top = 122
  BorderStyle = bsNone
  Caption = 'FmASIOVST'
  ClientHeight = 104
  ClientWidth = 231
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Lb_ASIOOutput: TLabel
    Left = 2
    Top = 7
    Width = 59
    Height = 13
    Margins.Bottom = 0
    Caption = 'ASIO Driver:'
    Transparent = True
    OnClick = Lb_ASIOOutputClick
  end
  object CB_ASIO: TComboBox
    Left = 67
    Top = 4
    Width = 158
    Height = 21
    ItemHeight = 13
    TabOrder = 0
    OnChange = CB_ASIOChange
  end
  object Memo: TMemo
    Left = 2
    Top = 31
    Width = 223
    Height = 66
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
end
