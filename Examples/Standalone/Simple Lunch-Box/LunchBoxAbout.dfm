object FmAbout: TFmAbout
  Left = 384
  Top = 373
  BorderStyle = bsNone
  Caption = 'About'
  ClientHeight = 99
  ClientWidth = 317
  Color = clMedGray
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnClick = FormClick
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 32
    Top = 24
    Width = 263
    Height = 25
    Caption = 'Lunchbox Battles Core'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -21
    Font.Name = 'Verdana'
    Font.Style = [fsBold]
    ParentFont = False
    OnClick = FormClick
  end
  object Lb: TLabel
    Left = 88
    Top = 55
    Width = 141
    Height = 13
    Caption = 'originally created by BramBos'
    OnClick = FormClick
  end
end
