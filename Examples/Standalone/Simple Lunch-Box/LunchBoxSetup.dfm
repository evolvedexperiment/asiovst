object FmSetup: TFmSetup
  Left = 805
  Top = 354
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 86
  ClientWidth = 224
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbPreset: TLabel
    Left = 4
    Top = 2
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object Label1: TLabel
    Left = 4
    Top = 29
    Width = 50
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object CBDrivers: TComboBox
    Left = 88
    Top = 2
    Width = 132
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 0
    OnChange = CBDriversChange
  end
  object CBOutput: TComboBox
    Left = 64
    Top = 29
    Width = 156
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    ItemHeight = 13
    TabOrder = 1
    OnChange = CBOutputChange
  end
  object BtControlPanel: TButton
    Left = 4
    Top = 56
    Width = 216
    Height = 25
    Caption = 'Control Panel'
    TabOrder = 2
    OnClick = BtControlPanelClick
  end
end
