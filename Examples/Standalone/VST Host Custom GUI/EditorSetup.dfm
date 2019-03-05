object FormSetup: TFormSetup
  Left = 412
  Top = 277
  BorderStyle = bsDialog
  Caption = 'Setup'
  ClientHeight = 92
  ClientWidth = 235
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
  DesignSize = (
    235
    92)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDriver: TLabel
    Left = 8
    Top = 7
    Width = 81
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LabelInput: TLabel
    Left = 8
    Top = 39
    Width = 46
    Height = 21
    AutoSize = False
    Caption = 'Input:'
    Layout = tlCenter
  end
  object LabelOutput: TLabel
    Left = 8
    Top = 63
    Width = 48
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object ComboBoxDrivers: TComboBox
    Left = 95
    Top = 8
    Width = 132
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 0
    OnChange = ComboBoxDriversChange
  end
  object ComboBoxInput: TComboBox
    Left = 60
    Top = 39
    Width = 167
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 1
    OnChange = ComboBoxInputChange
  end
  object ComboBoxOutput: TComboBox
    Left = 60
    Top = 63
    Width = 167
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 2
    OnChange = ComboBoxOutputChange
  end
end
