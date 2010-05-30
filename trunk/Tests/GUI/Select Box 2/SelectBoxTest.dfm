object Form1: TForm1
  Left = 218
  Top = 77
  Caption = 'Test Select Box'
  ClientHeight = 140
  ClientWidth = 219
  Color = 8160397
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
  object SelectBoxA: TGuiSelectBox
    Left = 8
    Top = 8
    Width = 100
    Height = 25
    ItemIndex = -1
    Items.Strings = (
      'Test 1'
      'Test 2'
      'Test 3')
  end
  object SelectBoxB: TGuiSelectBox
    Left = 114
    Top = 8
    Width = 100
    Height = 25
    AntiAlias = gaaLinear2x
    ItemIndex = -1
    Items.Strings = (
      'Test 1'
      'Test 2'
      'Test 3')
  end
  object SelectBoxC: TGuiSelectBox
    Left = 8
    Top = 39
    Width = 100
    Height = 25
    AntiAlias = gaaLinear3x
    ItemIndex = -1
    Items.Strings = (
      'Test A'
      'Test B'
      'Test C'
      'Test D')
  end
  object SelectBoxD: TGuiSelectBox
    Left = 114
    Top = 39
    Width = 100
    Height = 25
    AntiAlias = gaaLinear4x
    ItemIndex = -1
    Items.Strings = (
      'Test 1'
      'Test 2'
      'Test 3'
      'Test 4')
  end
  object Label1: TLabel
    Left = 8
    Top = 72
    Width = 70
    Height = 13
    Caption = 'Round Radius:'
    Transparent = True
  end
  object Label2: TLabel
    Left = 8
    Top = 95
    Width = 64
    Height = 13
    Caption = 'Arrow Width:'
    Transparent = True
  end
  object TbRoundRadius: TTrackBar
    Left = 84
    Top = 70
    Width = 130
    Height = 19
    Max = 19
    Min = 1
    Position = 1
    TabOrder = 0
    ThumbLength = 12
    OnChange = TbRoundRadiusChange
  end
  object CbTransparent: TCheckBox
    Left = 8
    Top = 118
    Width = 97
    Height = 17
    Caption = 'Transparent'
    Color = 8160397
    ParentColor = False
    TabOrder = 1
    OnClick = CbTransparentClick
  end
  object TbArrowWidth: TTrackBar
    Left = 84
    Top = 93
    Width = 130
    Height = 19
    Max = 5
    Min = 1
    Position = 1
    TabOrder = 2
    ThumbLength = 12
    OnChange = TbArrowWidthChange
  end
end
