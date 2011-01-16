object FmESTP: TFmESTP
  Left = 299
  Top = 51
  Caption = 'Equal Spaced Thick Polyline'
  ClientHeight = 328
  ClientWidth = 411
  Color = clBtnFace
  Constraints.MinHeight = 128
  Constraints.MinWidth = 128
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnResize = FormResize
  DesignSize = (
    411
    328)
  PixelsPerInch = 96
  TextHeight = 13
  object PaintBox: TPaintBox
    Left = 8
    Top = 32
    Width = 395
    Height = 288
    Anchors = [akLeft, akTop, akRight, akBottom]
    OnClick = PaintBoxClick
    OnPaint = PaintBoxPaint
  end
  object Label1: TLabel
    Left = 128
    Top = 317
    Width = 31
    Height = 13
    Caption = 'Label1'
    Visible = False
  end
  object Label2: TLabel
    Left = 184
    Top = 317
    Width = 31
    Height = 13
    Caption = 'Label2'
    Visible = False
  end
  object SlLineWidth: TGuiSlider
    Left = 8
    Top = 8
    Width = 395
    Height = 18
    Anchors = [akLeft, akTop, akRight]
    BorderRadius = 4.000000000000000000
    BorderWidth = 1.500000000000000000
    Caption = 'Linewidth'
    Color = clBtnFace
    DefaultValue = 10.000000000000000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Tahoma'
    Font.Style = []
    FontOversampling = fo4x
    FontShadow.Blur = 2.000000000000000000
    FontShadow.Color = clBtnFace
    FontShadow.OffsetX = 0
    FontShadow.OffsetY = 0
    FontShadow.Saturation = 3.000000000000000000
    FontShadow.Visible = True
    Max = 10.000000000000000000
    Min = 1.000000000000000000
    ParentColor = False
    Value = 2.000000000000000000
    ShowText = True
    SlideColor = 6316128
    OnChange = SlLineWidthChange
  end
end
