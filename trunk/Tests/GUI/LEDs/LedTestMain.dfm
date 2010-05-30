object FmLEDTest: TFmLEDTest
  Left = 218
  Top = 77
  Caption = 'LED-Test'
  ClientHeight = 266
  ClientWidth = 150
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    150
    266)
  PixelsPerInch = 96
  TextHeight = 13
  object LED1: TGuiLED
    Left = 8
    Top = 8
    Width = 64
    Height = 64
    LineWidth = 2
    LEDColor = clBlue
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 100.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    LineColor = clRed
  end
  object LED2: TGuiLED
    Left = 78
    Top = 8
    Width = 64
    Height = 64
    LineWidth = 2
    LEDColor = clYellow
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 100.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    Anchors = [akTop, akRight]
    AntiAlias = gaaLinear2x
    LineColor = clRed
  end
  object LED3: TGuiLED
    Left = 8
    Top = 78
    Width = 64
    Height = 64
    LineWidth = 2
    LEDColor = clRed
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 100.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    Anchors = [akLeft, akBottom]
    AntiAlias = gaaLinear3x
    LineColor = clRed
  end
  object LED4: TGuiLED
    Left = 78
    Top = 78
    Width = 64
    Height = 64
    LineWidth = 2
    LEDColor = clLime
    Brightness_Percent = 100.000000000000000000
    BorderStrength_Percent = 100.000000000000000000
    Uniformity_Percent = 50.000000000000000000
    Anchors = [akRight, akBottom]
    AntiAlias = gaaLinear4x
    LineColor = clRed
  end
  object LbUniformiy: TLabel
    Left = 8
    Top = 147
    Width = 53
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Uniformity:'
    Color = clBtnFace
    ParentColor = False
  end
  object LbBrightness: TLabel
    Left = 8
    Top = 173
    Width = 54
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Brightness:'
    Color = clBtnFace
    ParentColor = False
  end
  object LbLineWidth: TLabel
    Left = 8
    Top = 224
    Width = 54
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Line Width:'
    Color = clBtnFace
    ParentColor = False
  end
  object LbBorderStrength: TLabel
    Left = 8
    Top = 199
    Width = 36
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Border:'
    Color = clBtnFace
    ParentColor = False
  end
  object TbUniformity: TTrackBar
    Left = 63
    Top = 150
    Width = 79
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 100
    Frequency = 10
    Position = 50
    TabOrder = 0
    ThumbLength = 12
    OnChange = TbUniformityChange
  end
  object TbBrightness: TTrackBar
    Left = 63
    Top = 174
    Width = 79
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 100
    Frequency = 10
    Position = 90
    TabOrder = 1
    ThumbLength = 12
    OnChange = TbBrightnessChange
  end
  object TbLineWidth: TTrackBar
    Left = 63
    Top = 225
    Width = 79
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 6
    Min = 1
    Position = 2
    TabOrder = 2
    ThumbLength = 12
    OnChange = TbLineWidthChange
  end
  object TbBorderStrength: TTrackBar
    Left = 63
    Top = 200
    Width = 79
    Height = 17
    Anchors = [akLeft, akRight, akBottom]
    Max = 100
    Frequency = 10
    Position = 90
    TabOrder = 3
    ThumbLength = 12
    OnChange = TbBorderStrengthChange
  end
  object CbTransparent: TCheckBox
    Left = 32
    Top = 247
    Width = 79
    Height = 17
    Anchors = [akBottom]
    Caption = 'Transparent'
    TabOrder = 4
    OnClick = CbTransparentClick
  end
end
