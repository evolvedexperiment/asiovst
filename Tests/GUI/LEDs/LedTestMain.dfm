object FmLEDTest: TFmLEDTest
  Left = 218
  Top = 77
  Caption = 'LED-Test'
  ClientHeight = 224
  ClientWidth = 150
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
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
    Uniformity_Percent = 50.000000000000000000
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
    Uniformity_Percent = 50.000000000000000000
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
    Uniformity_Percent = 50.000000000000000000
    AntiAlias = gaaLinear4x
    LineColor = clRed
  end
  object LbUniformiy: TLabel
    Left = 8
    Top = 148
    Width = 53
    Height = 13
    Caption = 'Uniformity:'
  end
  object LbBrightness: TLabel
    Left = 8
    Top = 172
    Width = 54
    Height = 13
    Caption = 'Brightness:'
  end
  object LbLineWidth: TLabel
    Left = 8
    Top = 195
    Width = 54
    Height = 13
    Caption = 'Line Width:'
  end
  object TbUniformity: TTrackBar
    Left = 63
    Top = 148
    Width = 79
    Height = 17
    Max = 100
    Frequency = 10
    Position = 50
    TabOrder = 0
    ThumbLength = 11
    OnChange = TbUniformityChange
  end
  object TbBrightness: TTrackBar
    Left = 63
    Top = 172
    Width = 79
    Height = 17
    Max = 100
    Frequency = 10
    Position = 90
    TabOrder = 1
    ThumbLength = 11
    OnChange = TbBrightnessChange
  end
  object TbLineWidth: TTrackBar
    Left = 63
    Top = 195
    Width = 79
    Height = 17
    Max = 6
    Min = 1
    Position = 2
    TabOrder = 2
    ThumbLength = 11
    OnChange = TbLineWidthChange
  end
end
