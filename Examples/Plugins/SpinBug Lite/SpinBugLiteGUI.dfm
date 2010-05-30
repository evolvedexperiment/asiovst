object FmSpinBugLite: TFmSpinBugLite
  Left = 527
  Top = 329
  BorderStyle = bsNone
  Caption = 'SpinBug Lite'
  ClientHeight = 190
  ClientWidth = 317
  Color = 197995
  Font.Charset = DEFAULT_CHARSET
  Font.Color = 216826
  Font.Height = -13
  Font.Name = 'Comic Sans MS'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    317
    190)
  PixelsPerInch = 96
  TextHeight = 18
  object LbTitleShadow: TGuiLabel
    Left = 9
    Top = 105
    Width = 297
    Height = 76
    Anchors = [akLeft, akBottom]
    AntiAlias = gaaLinear4x
    Caption = 'SPINBUG'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 13491454
    Font.Height = -64
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Shadow.Color = clBlack
  end
  object LbTitle: TGuiLabel
    Left = 7
    Top = 103
    Width = 297
    Height = 76
    Anchors = [akLeft, akBottom]
    AntiAlias = gaaLinear4x
    Caption = 'SPINBUG'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 216826
    Font.Height = -64
    Font.Name = 'Comic Sans MS'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = clBlack
  end
  object DialLFO: TGuiDial
    Left = 213
    Top = 8
    Width = 96
    Height = 96
    AntiAlias = gaaLinear2x
    CircleColor = 216826
    CurveMapping = -0.920000016689300500
    DialImageIndex = -1
    LineColor = clWhite
    LineWidth = 2
    Max = 13.000000000000000000
    OnChange = DialLFOChange
    PointerAngles.Start = 215
    PointerAngles.Range = 270
    PointerAngles.Resolution = 270.000000000000000000
    Position = 4.000000000000000000
    ScrollRange_Pixel = 400.000000000000000000
    StitchKind = skHorizontal
    WheelStep = 1.000000000000000000
  end
  object SelectType: TGuiSelectBox
    Left = 45
    Top = 8
    Width = 89
    Height = 25
    ArrowColor = clWhite
    ButtonColor = 271837
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ItemIndex = -1
    Items.Strings = (
      'stereo a'
      'stereo b'
      'stereo c'
      'stereo d'
      'mono'
      'mono l'
      'mono r'
      'm+s'
      'special'
      'old one')
    LineColor = clWhite
    ParentFont = False
    Radius = 4
    SelectBoxColor = 271837
    OnChange = SelectTypeChange
  end
  object LbType: TLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 16
    Caption = 'type:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object SelectColour: TGuiSelectBox
    Left = 58
    Top = 39
    Width = 108
    Height = 25
    ArrowColor = clWhite
    ButtonColor = 271837
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ItemIndex = -1
    Items.Strings = (
      'rough'
      'firm'
      'medium'
      'soft'
      'smooth')
    LineColor = clWhite
    ParentFont = False
    Radius = 4
    SelectBoxColor = 271837
    OnChange = SelectColourChange
  end
  object Label1: TLabel
    Left = 8
    Top = 43
    Width = 44
    Height = 16
    Caption = 'colour:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LbLFOSpeed: TLabel
    Left = 95
    Top = 88
    Width = 66
    Height = 16
    Caption = 'lfo speed: '
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LbLFOSpeedValue: TLabel
    Left = 164
    Top = 88
    Width = 34
    Height = 16
    Caption = '10 Hz'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWhite
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
  end
end
