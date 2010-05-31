object FmUnknownFileExplorer: TFmUnknownFileExplorer
  Left = 240
  Top = 81
  Caption = 'Unknown File Explorer'
  ClientHeight = 275
  ClientWidth = 433
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnShow = FormShow
  DesignSize = (
    433
    275)
  PixelsPerInch = 96
  TextHeight = 13
  object LbFileName: TLabel
    Left = 8
    Top = 11
    Width = 46
    Height = 13
    Caption = 'Filename:'
    OnClick = LbFileNameClick
  end
  object LbStartHex: TLabel
    Left = 8
    Top = 38
    Width = 50
    Height = 13
    Caption = 'Start Hex:'
  end
  object LbValueCount: TLabel
    Left = 8
    Top = 65
    Width = 62
    Height = 13
    Caption = 'Value Count:'
  end
  object LbDataFormat: TLabel
    Left = 194
    Top = 38
    Width = 65
    Height = 13
    Caption = 'Float Format:'
  end
  object EdFileName: TEdit
    Left = 60
    Top = 8
    Width = 365
    Height = 21
    Anchors = [akLeft, akTop, akRight]
    TabOrder = 0
    OnChange = EdFileNameChange
  end
  object BtFileSelect: TButton
    Left = 405
    Top = 10
    Width = 18
    Height = 18
    Anchors = [akTop, akRight]
    Caption = '...'
    TabOrder = 1
    OnClick = BtFileSelectClick
  end
  object BtConvert: TButton
    Left = 134
    Top = 62
    Width = 67
    Height = 22
    Caption = '&Convert'
    TabOrder = 2
    OnClick = BtConvertClick
  end
  object EdHex: TEdit
    Left = 60
    Top = 35
    Width = 121
    Height = 21
    TabOrder = 3
    Text = '$00000000'
  end
  object Memo: TMemo
    Left = 8
    Top = 90
    Width = 415
    Height = 177
    Anchors = [akLeft, akTop, akRight, akBottom]
    TabOrder = 4
  end
  object SeValueCount: TSpinEdit
    Left = 76
    Top = 62
    Width = 52
    Height = 22
    MaxValue = 1000
    MinValue = 0
    TabOrder = 5
    Value = 50
  end
  object RbSingle: TRadioButton
    Left = 264
    Top = 37
    Width = 49
    Height = 17
    Caption = 'Single'
    TabOrder = 6
  end
  object RbDouble: TRadioButton
    Left = 319
    Top = 37
    Width = 52
    Height = 17
    Caption = 'Double'
    Checked = True
    TabOrder = 7
    TabStop = True
  end
  object CbSwap: TCheckBox
    Left = 377
    Top = 37
    Width = 48
    Height = 17
    Caption = 'Swap'
    TabOrder = 8
  end
  object OD: TOpenDialog
    Left = 248
    Top = 56
  end
end
