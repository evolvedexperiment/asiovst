object FormSetup: TFormSetup
  Left = 541
  Top = 364
  BorderStyle = bsDialog
  Caption = 'ASIO Settings'
  ClientHeight = 95
  ClientWidth = 235
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  DesignSize = (
    235
    95)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelPreset: TLabel
    Left = 8
    Top = 7
    Width = 63
    Height = 21
    AutoSize = False
    Caption = 'ASIO Driver:'
    Layout = tlCenter
  end
  object LabelOutput: TLabel
    Left = 8
    Top = 35
    Width = 37
    Height = 21
    AutoSize = False
    Caption = 'Output:'
    Layout = tlCenter
  end
  object ComboBoxDrivers: TComboBox
    Left = 71
    Top = 8
    Width = 156
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 0
    OnChange = ComboBoxDriversChange
  end
  object ComboBoxOutput: TComboBox
    Left = 51
    Top = 35
    Width = 176
    Height = 21
    Style = csDropDownList
    Anchors = [akLeft, akTop, akRight]
    Color = clBtnFace
    TabOrder = 1
    OnChange = ComboBoxOutputChange
  end
  object ButtonControlPanel: TButton
    Left = 8
    Top = 62
    Width = 219
    Height = 25
    Anchors = [akLeft, akTop, akRight]
    Caption = 'Control Panel'
    Default = True
    TabOrder = 2
    OnClick = ButtonControlPanelClick
  end
end
