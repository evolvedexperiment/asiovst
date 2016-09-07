object FmSaveAnimation: TFmSaveAnimation
  Left = 466
  Top = 437
  BorderStyle = bsDialog
  Caption = 'Save Animation'
  ClientHeight = 124
  ClientWidth = 421
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poMainFormCenter
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    421
    124)
  PixelsPerInch = 96
  TextHeight = 13
  object LbDirectory: TLabel
    Left = 8
    Top = 11
    Width = 48
    Height = 13
    Caption = 'Directory:'
  end
  object LbScaleFactor: TLabel
    Left = 8
    Top = 38
    Width = 63
    Height = 13
    Caption = 'Scale Factor:'
  end
  object LbScaleFactorUnit: TLabel
    Left = 135
    Top = 38
    Width = 11
    Height = 13
    Caption = '%'
  end
  object LbDimensions: TLabel
    Left = 160
    Top = 38
    Width = 120
    Height = 13
    Caption = 'Dimensions: 1234 x 5678'
  end
  object LbStyle: TLabel
    Left = 291
    Top = 38
    Width = 28
    Height = 13
    Caption = 'Style:'
  end
  object BtOK: TButton
    Left = 122
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&OK'
    Default = True
    Enabled = False
    ModalResult = 1
    TabOrder = 0
    OnClick = BtOKClick
  end
  object BtCancel: TButton
    Left = 203
    Top = 91
    Width = 75
    Height = 25
    Anchors = [akLeft, akBottom]
    Caption = '&Cancel'
    ModalResult = 2
    TabOrder = 1
  end
  object EdDirectory: TEdit
    Left = 62
    Top = 8
    Width = 351
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 2
    OnChange = EdDirectoryChange
  end
  object BtSelectDirectory: TButton
    Left = 393
    Top = 10
    Width = 18
    Height = 18
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 3
    OnClick = BtSelectDirectoryClick
  end
  object SEScale: TSpinEdit
    Left = 77
    Top = 35
    Width = 52
    Height = 22
    MaxValue = 1000
    MinValue = 1
    TabOrder = 4
    Value = 200
    OnChange = SEScaleChange
  end
  object CbStyle: TComboBox
    Left = 325
    Top = 34
    Width = 88
    Height = 21
    Style = csDropDownList
    ItemIndex = 0
    TabOrder = 5
    Text = 'Static'
    OnChange = CbStyleChange
    Items.Strings = (
      'Static'
      'Animated'
      'Advanced'
      'Blow All')
  end
  object PnSettingsAnimated: TPanel
    Left = 101
    Top = 63
    Width = 218
    Height = 26
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 6
    Visible = False
    object LbHalfLife: TLabel
      Left = 104
      Top = 5
      Width = 37
      Height = 13
      Caption = 'Halflife:'
    end
    object SEHalfLife: TSpinEdit
      Left = 147
      Top = 2
      Width = 71
      Height = 22
      MaxValue = 10000
      MinValue = 1
      TabOrder = 0
      Value = 500
    end
    object CbSkipFrames: TCheckBox
      Left = 1
      Top = 4
      Width = 79
      Height = 17
      Caption = 'Skip Frames'
      TabOrder = 1
    end
  end
  object PnSettingsAdvanced: TPanel
    Left = 108
    Top = 63
    Width = 205
    Height = 26
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 7
    Visible = False
    object LbMaxItems: TLabel
      Left = 0
      Top = 3
      Width = 124
      Height = 13
      Caption = 'Max. Animated Primitives:'
    end
    object SeMaxAnimatedPrimitives: TSpinEdit
      Left = 130
      Top = 0
      Width = 71
      Height = 22
      MaxValue = 1024
      MinValue = 1
      TabOrder = 0
      Value = 32
    end
  end
  object PnBlow: TPanel
    Left = 170
    Top = 63
    Width = 81
    Height = 26
    BevelOuter = bvNone
    ShowCaption = False
    TabOrder = 8
    Visible = False
    object CbInverted: TCheckBox
      Left = 11
      Top = 2
      Width = 68
      Height = 17
      Caption = 'Inverted'
      TabOrder = 0
    end
  end
end
