object FmRenaissanceBassClone: TFmRenaissanceBassClone
  Left = 501
  Top = 225
  Caption = 'RenaissanceBass Clone'
  ClientHeight = 498
  ClientWidth = 753
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GuiColorLevelMeter1: TGuiColorLevelMeter
    Left = 620
    Top = 64
    Width = 17
    Height = 241
    BorderColor = clWindowFrame
    ContrastLuminanz = 0.300000011920929000
    Upper = 1.000000000000000000
  end
  object GuiColorLevelMeter2: TGuiColorLevelMeter
    Left = 667
    Top = 64
    Width = 17
    Height = 241
    BorderColor = clWindowFrame
    ContrastLuminanz = 0.300000011920929000
    Upper = 1.000000000000000000
  end
  object LbAudio: TGuiLabel
    Left = 628
    Top = 311
    Width = 48
    Height = 14
    Alignment = taCenter
    Caption = 'Audio'
  end
  object LbRenaissanceBass: TGuiLabel
    Left = 628
    Top = 331
    Width = 48
    Height = 14
    Alignment = taCenter
    Caption = 'RenaissanceBass'
  end
  object LbOriginalBass: TGuiLabel
    Left = 620
    Top = 351
    Width = 64
    Height = 14
    Alignment = taCenter
    Caption = 'Original Bass'
  end
  object LbClipIndicator: TGuiLabel
    Left = 628
    Top = 44
    Width = 48
    Height = 14
    Alignment = taCenter
    Caption = 'No Clip'
  end
  object LbFrequency: TLabel
    Left = 16
    Top = 16
    Width = 55
    Height = 13
    Caption = 'Frequency:'
  end
  object SBFrequency: TScrollBar
    Left = 77
    Top = 15
    Width = 121
    Height = 16
    PageSize = 0
    TabOrder = 0
  end
end
