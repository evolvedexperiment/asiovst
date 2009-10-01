object FmPanelTest: TFmPanelTest
  Left = 218
  Top = 77
  Caption = 'Panel Test'
  ClientHeight = 132
  ClientWidth = 278
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbRoundRadius: TLabel
    Left = 8
    Top = 85
    Width = 70
    Height = 13
    Caption = 'Round Radius:'
  end
  object Label1: TLabel
    Left = 8
    Top = 109
    Width = 54
    Height = 13
    Caption = 'Line Width:'
  end
  object PanelA: TGuiPanel
    Left = 8
    Top = 8
    Width = 128
    Height = 32
    Caption = 'PanelA'
    LineColor = clBtnShadow
    PanelColor = clBlue
    ParentColor = True
    TabOrder = 0
    UseDockManager = True
  end
  object PanelB: TGuiPanel
    Left = 142
    Top = 8
    Width = 128
    Height = 32
    AntiAlias = gaaLinear2x
    Caption = 'GuiPanel1'
    LineColor = clBtnShadow
    PanelColor = clLime
    ParentColor = True
    Radius = 3
    TabOrder = 1
    UseDockManager = True
  end
  object PanelC: TGuiPanel
    Left = 8
    Top = 46
    Width = 128
    Height = 32
    AntiAlias = gaaLinear3x
    Caption = 'GuiPanel1'
    LineColor = clBtnShadow
    PanelColor = clRed
    ParentColor = True
    Radius = 4
    TabOrder = 2
    UseDockManager = True
  end
  object PanelD: TGuiPanel
    Left = 142
    Top = 46
    Width = 128
    Height = 32
    AntiAlias = gaaLinear4x
    Caption = 'GuiPanel1'
    LineColor = clBtnShadow
    PanelColor = clYellow
    ParentColor = True
    Radius = 5
    TabOrder = 3
    UseDockManager = True
  end
  object TbRoundRadius: TTrackBar
    Left = 84
    Top = 84
    Width = 186
    Height = 18
    Max = 16
    Min = 1
    Position = 1
    TabOrder = 4
    ThumbLength = 12
    OnChange = TbRoundRadiusChange
  end
  object TbLineWidth: TTrackBar
    Left = 68
    Top = 108
    Width = 118
    Height = 18
    Max = 8
    Min = 1
    Position = 1
    TabOrder = 5
    ThumbLength = 12
    OnChange = TbLineWidthChange
  end
  object CbTransparent: TCheckBox
    Left = 192
    Top = 108
    Width = 78
    Height = 17
    Caption = 'Transparent'
    TabOrder = 6
    OnClick = CbTransparentClick
  end
end
