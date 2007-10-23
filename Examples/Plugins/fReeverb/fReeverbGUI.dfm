object FmReverb: TFmReverb
  Left = 401
  Top = 252
  BorderStyle = bsNone
  Caption = 'fReeverb'
  ClientHeight = 211
  ClientWidth = 189
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbDry: TLabel
    Left = 8
    Top = 192
    Width = 16
    Height = 13
    Caption = 'Dry'
  end
  object LbWet: TLabel
    Left = 32
    Top = 192
    Width = 20
    Height = 13
    Caption = 'Wet'
  end
  object Label1: TLabel
    Left = 60
    Top = 192
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object LbSize: TLabel
    Left = 124
    Top = 192
    Width = 20
    Height = 13
    Caption = 'Size'
  end
  object LbStretch: TLabel
    Left = 150
    Top = 192
    Width = 34
    Height = 13
    Caption = 'Stretch'
  end
  object LbDamp: TLabel
    Left = 92
    Top = 192
    Width = 28
    Height = 13
    Caption = 'Damp'
  end
  object SDry: TScrollBar
    Left = 8
    Top = 8
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    OnChange = SDryChange
  end
  object SWet: TScrollBar
    Left = 33
    Top = 8
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 1
    OnChange = SWetChange
  end
  object SWidth: TScrollBar
    Left = 65
    Top = 8
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 2
    OnChange = SWidthChange
  end
  object SRoomSize: TScrollBar
    Left = 129
    Top = 8
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 3
    OnChange = SRoomSizeChange
  end
  object CBFreeze: TCheckBox
    Left = 64
    Top = 234
    Width = 57
    Height = 17
    Caption = 'FreeZe'
    TabOrder = 4
    OnClick = CBFreezeClick
  end
  object SStretch: TScrollBar
    Left = 158
    Top = 8
    Width = 16
    Height = 177
    Kind = sbVertical
    Max = 20
    PageSize = 0
    Position = 20
    TabOrder = 5
    OnChange = SStretchChange
  end
  object SDamp: TScrollBar
    Left = 97
    Top = 8
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 6
    OnChange = SDampChange
  end
end
