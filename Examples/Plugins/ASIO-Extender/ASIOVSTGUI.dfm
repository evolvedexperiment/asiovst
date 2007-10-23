object FmASIOVST: TFmASIOVST
  Left = 336
  Top = 123
  BorderStyle = bsNone
  Caption = 'FmASIOVST'
  ClientHeight = 104
  ClientWidth = 251
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  DesignSize = (
    251
    104)
  PixelsPerInch = 96
  TextHeight = 13
  object Lb_ASIOOutput: TLabel
    Left = 2
    Top = 7
    Width = 59
    Height = 13
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
    Width = 243
    Height = 66
    Anchors = [akLeft, akTop, akRight]
    BorderStyle = bsNone
    Color = clBtnFace
    ReadOnly = True
    TabOrder = 1
  end
  object CBShortCircuit: TCheckBox
    Left = 230
    Top = 6
    Width = 15
    Height = 17
    Checked = True
    State = cbChecked
    TabOrder = 2
    OnClick = CBShortCircuitClick
  end
end
