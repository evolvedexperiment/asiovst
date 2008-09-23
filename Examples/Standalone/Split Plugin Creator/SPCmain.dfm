object FmSplitPluginCreator: TFmSplitPluginCreator
  Left = 291
  Top = 303
  BorderStyle = bsDialog
  Caption = 'Split Plugin Creator'
  ClientHeight = 93
  ClientWidth = 336
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object LbPluginA: TLabel
    Left = 8
    Top = 11
    Width = 42
    Height = 13
    Caption = 'Plugin A:'
  end
  object LbPluginB: TLabel
    Left = 8
    Top = 38
    Width = 41
    Height = 13
    Caption = 'Plugin B:'
    Enabled = False
  end
  object EdPluginA: TEdit
    Left = 56
    Top = 8
    Width = 273
    Height = 21
    TabOrder = 0
    OnChange = EdPluginAChange
    OnClick = EdPluginAClick
  end
  object EdPluginB: TEdit
    Left = 56
    Top = 35
    Width = 273
    Height = 21
    Enabled = False
    TabOrder = 1
    Text = 'not supported yet (please donate to add this feature)'
  end
  object BtCreate: TButton
    Left = 120
    Top = 62
    Width = 75
    Height = 25
    Caption = 'Create...'
    Enabled = False
    TabOrder = 2
    OnClick = BtCreateClick
  end
end
