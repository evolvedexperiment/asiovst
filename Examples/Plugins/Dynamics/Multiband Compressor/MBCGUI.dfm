object FmMBC: TFmMBC
  Left = 248
  Top = 126
  BorderStyle = bsNone
  Caption = 'FmMBC'
  ClientHeight = 307
  ClientWidth = 775
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  DesignSize = (
    775
    307)
  PixelsPerInch = 96
  TextHeight = 13
  object LbAbout1: TLabel
    Left = 48
    Top = 10
    Width = 168
    Height = 15
    Caption = 'D3 Multi Band Compressor'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
  end
  object LbAbout2: TLabel
    Left = 591
    Top = 11
    Width = 168
    Height = 14
    Alignment = taRightJustify
    Anchors = [akTop, akRight]
    Caption = 'delphi asio vst presents'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    Transparent = True
    ExplicitLeft = 474
  end
end
