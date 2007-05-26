object FmReverb: TFmReverb
  Left = 401
  Top = 252
  BorderStyle = bsNone
  Caption = 'fReeverb'
  ClientHeight = 257
  ClientWidth = 194
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
    Top = 216
    Width = 16
    Height = 13
    Caption = 'Dry'
  end
  object LbWet: TLabel
    Left = 32
    Top = 216
    Width = 20
    Height = 13
    Caption = 'Wet'
  end
  object Label1: TLabel
    Left = 60
    Top = 216
    Width = 28
    Height = 13
    Caption = 'Width'
  end
  object LbSize: TLabel
    Left = 124
    Top = 216
    Width = 20
    Height = 13
    Caption = 'Size'
  end
  object LbStretch: TLabel
    Left = 150
    Top = 216
    Width = 34
    Height = 13
    Caption = 'Stretch'
  end
  object LbDamp: TLabel
    Left = 92
    Top = 216
    Width = 28
    Height = 13
    Caption = 'Damp'
  end
  object SDry: TScrollBar
    Left = 8
    Top = 32
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 0
    OnChange = SDryChange
  end
  object SWet: TScrollBar
    Left = 33
    Top = 32
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 1
    OnChange = SWetChange
  end
  object SWidth: TScrollBar
    Left = 65
    Top = 32
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 2
    OnChange = SWidthChange
  end
  object SRoomSize: TScrollBar
    Left = 129
    Top = 32
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
    Top = 32
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
    Top = 32
    Width = 16
    Height = 177
    Kind = sbVertical
    PageSize = 0
    TabOrder = 6
    OnChange = SDampChange
  end
  object AMMB1: TActionMainMenuBar
    Left = 0
    Top = 0
    Width = 194
    Height = 24
    UseSystemFont = False
    ActionManager = AM1
    Caption = 'AMMB1'
    ColorMap.HighlightColor = 14410210
    ColorMap.BtnSelectedColor = clBtnFace
    ColorMap.UnusedColor = 14410210
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = []
    PersistentHotKeys = True
    Spacing = 0
  end
  object AM1: TActionManager
    ActionBars = <
      item
        Items = <
          item
            Items = <
              item
                Caption = '&ActionClientItem0'
              end
              item
                Caption = 'A&ctionClientItem1'
              end
              item
                Caption = 'Ac&tionClientItem2'
              end>
            Caption = '&Item'
          end
          item
            Caption = '&Noch einer'
          end>
        ActionBar = AMMB1
      end>
    Left = 64
    Top = 40
    StyleName = 'XP Style'
  end
end
