object FmGroupBoxTest: TFmGroupBoxTest
  Left = 218
  Top = 77
  Caption = 'GroupBox Test'
  ClientHeight = 220
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LbLineWidth: TLabel
    Left = 8
    Top = 149
    Width = 54
    Height = 13
    Caption = 'Line Width:'
  end
  object LbRoundRadius: TLabel
    Left = 8
    Top = 173
    Width = 70
    Height = 13
    Caption = 'Round Radius:'
  end
  object GroupA: TGuiGroup
    Left = 8
    Top = 8
    Width = 128
    Height = 64
    Caption = 'Group A'
    TabOrder = 0
  end
  object GroupB: TGuiGroup
    Left = 142
    Top = 8
    Width = 128
    Height = 64
    AntiAlias = gaaLinear2x
    Caption = 'Group B'
    TabOrder = 1
  end
  object GroupC: TGuiGroup
    Left = 8
    Top = 78
    Width = 128
    Height = 64
    AntiAlias = gaaLinear3x
    Caption = 'Group C'
    TabOrder = 2
  end
  object GroupD: TGuiGroup
    Left = 142
    Top = 78
    Width = 128
    Height = 64
    AntiAlias = gaaLinear4x
    Caption = 'Group D'
    TabOrder = 3
  end
  object TbLineWidth: TTrackBar
    Left = 84
    Top = 148
    Width = 186
    Height = 18
    TabOrder = 4
    ThumbLength = 12
    OnChange = TbLineWidthChange
  end
  object TbRoundRadius: TTrackBar
    Left = 84
    Top = 172
    Width = 186
    Height = 18
    TabOrder = 5
    ThumbLength = 12
    OnChange = TbRoundRadiusChange
  end
  object CbTransparent: TCheckBox
    Left = 8
    Top = 196
    Width = 97
    Height = 17
    Caption = 'Transparent'
    TabOrder = 6
    OnClick = CbTransparentClick
  end
end
