object VSTPluginWizarDGuiDialog: TVSTPluginWizarDGuiDialog
  Left = 293
  Top = 161
  HorzScrollBar.Visible = False
  VertScrollBar.Visible = False
  BorderStyle = bsToolWindow
  Caption = 'VST PlugIn Wizard Dialog'
  ClientHeight = 30
  ClientWidth = 268
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbEffectName: TLabel
    Left = 8
    Top = 8
    Width = 59
    Height = 13
    Caption = 'Class Name:'
  end
  object EdClassName: TEdit
    Left = 71
    Top = 4
    Width = 90
    Height = 21
    TabOrder = 0
    Text = 'VSTModule'
  end
  object BtCreate: TButton
    Left = 218
    Top = 4
    Width = 46
    Height = 21
    Caption = 'Create'
    Default = True
    ModalResult = 1
    TabOrder = 1
  end
  object GBFlags: TGroupBox
    Left = 8
    Top = 200
    Width = 169
    Height = 297
    Caption = 'GBFlags'
    TabOrder = 2
  end
  object CBEditor: TCheckBox
    Left = 168
    Top = 7
    Width = 49
    Height = 17
    Caption = 'Editor'
    Checked = True
    State = cbChecked
    TabOrder = 3
  end
end
