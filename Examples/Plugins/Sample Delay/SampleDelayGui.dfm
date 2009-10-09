object FmSampleDelay: TFmSampleDelay
  Left = 218
  Top = 77
  BorderStyle = bsNone
  Caption = 'Sample Delay'
  ClientHeight = 33
  ClientWidth = 272
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbSample: TLabel
    Left = 8
    Top = 9
    Width = 43
    Height = 13
    Caption = 'Samples:'
  end
  object LbSampleValue: TLabel
    Left = 239
    Top = 9
    Width = 6
    Height = 13
    Caption = '0'
  end
  object SbSample: TScrollBar
    Left = 57
    Top = 8
    Width = 176
    Height = 16
    Max = 1024
    Min = -1024
    PageSize = 0
    TabOrder = 0
    OnChange = SbSampleChange
  end
  object BrushedMetal: TGuiBackground
    Active = False
    Color = 6782354
    Left = 24
    Top = 8
  end
end
