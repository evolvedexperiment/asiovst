object FmVOL: TFmVOL
  Left = 317
  Top = 85
  BorderStyle = bsNone
  Caption = 'VST Opcode Logger'
  ClientHeight = 277
  ClientWidth = 374
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    374
    277)
  PixelsPerInch = 96
  TextHeight = 13
  object BtClear: TButton
    Left = 299
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Clear'
    TabOrder = 1
    OnClick = BtClearClick
    ExplicitLeft = 310
  end
  object MOpcodeLog: TMemo
    Left = 0
    Top = 26
    Width = 374
    Height = 251
    Align = alBottom
    Anchors = [akLeft, akTop, akRight, akBottom]
    Lines.Strings = (
      'MOpcodeLog')
    ReadOnly = True
    TabOrder = 0
    ExplicitWidth = 354
    ExplicitHeight = 249
  end
  object BtUpdate: TButton
    Left = 224
    Top = 0
    Width = 75
    Height = 25
    Anchors = [akTop, akRight]
    Caption = '&Update'
    TabOrder = 2
    OnClick = BtUpdateClick
    ExplicitLeft = 235
  end
  object CBAutoUpdates: TCheckBox
    Left = 4
    Top = 3
    Width = 111
    Height = 17
    Caption = 'Automatic Updates'
    TabOrder = 3
  end
end
