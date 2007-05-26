object FmASIOVST: TFmASIOVST
  Left = 335
  Top = 122
  BorderStyle = bsNone
  Caption = 'FmASIOVST'
  ClientHeight = 83
  ClientWidth = 243
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
    Width = 63
    Height = 13
    Caption = 'ASIO Output:'
    Transparent = True
  end
  object CB_ASIO: TComboBox
    Left = 67
    Top = 4
    Width = 145
    Height = 21
    ItemHeight = 13
    TabOrder = 0
  end
end
