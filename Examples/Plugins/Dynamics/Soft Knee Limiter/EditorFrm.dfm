object EditorForm: TEditorForm
  Left = 218
  Top = 81
  BorderStyle = bsNone
  Caption = 'EditorForm'
  ClientHeight = 196
  ClientWidth = 252
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  DesignSize = (
    252
    196)
  PixelsPerInch = 96
  TextHeight = 13
  object LbThreshold: TLabel
    Left = 8
    Top = 8
    Width = 51
    Height = 13
    Margins.Bottom = 0
    Caption = 'Threshold:'
  end
  object LbThresholdValue: TLabel
    Left = 160
    Top = 8
    Width = 84
    Height = 13
    Margins.Bottom = 0
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
    Caption = 'dB'
  end
  object LbRatio: TLabel
    Left = 8
    Top = 46
    Width = 29
    Height = 13
    Margins.Bottom = 0
    Caption = 'Ratio:'
  end
  object LbRatioValue: TLabel
    Left = 160
    Top = 46
    Width = 84
    Height = 13
    Margins.Bottom = 0
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
  end
  object LbAttack: TLabel
    Left = 8
    Top = 84
    Width = 35
    Height = 13
    Margins.Bottom = 0
    Caption = 'Attack:'
  end
  object LbAttackValue: TLabel
    Left = 168
    Top = 84
    Width = 76
    Height = 13
    Margins.Bottom = 0
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
  end
  object LbRelease: TLabel
    Left = 8
    Top = 122
    Width = 42
    Height = 13
    Margins.Bottom = 0
    Caption = 'Release:'
  end
  object LbReleaseValue: TLabel
    Left = 160
    Top = 122
    Width = 84
    Height = 13
    Margins.Bottom = 0
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
  end
  object LbSoftKnee: TLabel
    Left = 8
    Top = 160
    Width = 51
    Height = 13
    Margins.Bottom = 0
    Caption = 'Soft Knee:'
  end
  object LbSoftKneeValue: TLabel
    Left = 160
    Top = 160
    Width = 84
    Height = 13
    Margins.Bottom = 0
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    AutoSize = False
  end
  object SBThreshold: TScrollBar
    Left = 8
    Top = 24
    Width = 236
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 0
    Min = -96
    PageSize = 0
    Position = -80
    TabOrder = 0
    OnChange = SBThresholdChange
  end
  object SBRatio: TScrollBar
    Left = 8
    Top = 62
    Width = 236
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 200
    PageSize = 0
    TabOrder = 1
    OnChange = SBRatioChange
  end
  object SBAttack: TScrollBar
    Left = 8
    Top = 100
    Width = 236
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    Max = 300
    Min = -200
    PageSize = 0
    TabOrder = 2
    OnChange = SBAttackChange
  end
  object SBRelease: TScrollBar
    Left = 8
    Top = 138
    Width = 236
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    LargeChange = 100
    Max = 3699
    Min = 699
    PageSize = 0
    Position = 700
    SmallChange = 10
    TabOrder = 3
    OnChange = SBReleaseChange
  end
  object SBSoftKnee: TScrollBar
    Left = 8
    Top = 176
    Width = 236
    Height = 16
    Anchors = [akLeft, akTop, akRight]
    LargeChange = 100
    Max = 200
    PageSize = 0
    Position = 100
    SmallChange = 10
    TabOrder = 4
    OnChange = SBSoftKneeChange
  end
end
