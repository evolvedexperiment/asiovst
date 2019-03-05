object FormSetup: TFormSetup
  Left = 299
  Top = 149
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 84
  ClientWidth = 226
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Verdana'
  Font.Style = [fsBold]
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDriver: TLabel
    Left = 4
    Top = 2
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LabelInput: TLabel
    Left = 4
    Top = 34
    Width = 46
    Height = 21
    AutoSize = False
    Caption = 'Input:'
    Layout = tlCenter
  end
  object LabelOutput: TLabel
    Left = 4
    Top = 58
    Width = 48
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object ComboBoxDrivers: TComboBox
    Left = 88
    Top = 2
    Width = 132
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    TabOrder = 0
    OnChange = ComboBoxDriversChange
  end
  object ComboBoxInput: TComboBox
    Left = 56
    Top = 34
    Width = 164
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    TabOrder = 1
    OnChange = ComboBoxInputChange
  end
  object ComboBoxOutput: TComboBox
    Left = 56
    Top = 58
    Width = 164
    Height = 21
    Style = csDropDownList
    Color = clBtnFace
    TabOrder = 2
    OnChange = ComboBoxOutputChange
  end
end
