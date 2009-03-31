object FmCTC: TFmCTC
  Left = 339
  Top = 199
  BorderStyle = bsNone
  Caption = 'Crosstalk Cancellation'
  ClientHeight = 98
  ClientWidth = 312
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object LbSpeakerDistance: TLabel
    Left = 8
    Top = 8
    Width = 87
    Height = 13
    Caption = 'Speaker Distance:'
  end
  object LbListenerDistance: TLabel
    Left = 8
    Top = 32
    Width = 86
    Height = 13
    Caption = 'Listener Distance:'
  end
  object LbSpeakerDistanceValue: TLabel
    Left = 263
    Top = 8
    Width = 14
    Height = 13
    Caption = '1m'
  end
  object LbListenerDistanceValue: TLabel
    Left = 263
    Top = 32
    Width = 14
    Height = 13
    Caption = '1m'
  end
  object LbAttenuation: TLabel
    Left = 8
    Top = 55
    Width = 61
    Height = 13
    Caption = 'Attenuation:'
  end
  object LbAttenuationValue: TLabel
    Left = 263
    Top = 55
    Width = 22
    Height = 13
    Caption = '-3dB'
  end
  object LbRecursionSteps: TLabel
    Left = 8
    Top = 78
    Width = 81
    Height = 13
    Caption = 'Recursion Steps:'
  end
  object LbRecursionStepsValue: TLabel
    Left = 263
    Top = 78
    Width = 6
    Height = 13
    Caption = '3'
  end
  object SbSpeakerDistance: TScrollBar
    Left = 101
    Top = 7
    Width = 156
    Height = 17
    Max = 5000
    Min = 10
    PageSize = 0
    Position = 100
    TabOrder = 0
    OnChange = SbSpeakerDistanceChange
  end
  object SbListenerDistance: TScrollBar
    Left = 101
    Top = 30
    Width = 156
    Height = 17
    Max = 5000
    Min = 10
    PageSize = 0
    Position = 100
    TabOrder = 1
    OnChange = SbListenerDistanceChange
  end
  object SbAttenuation: TScrollBar
    Left = 101
    Top = 53
    Width = 156
    Height = 17
    Max = 0
    Min = -150
    PageSize = 0
    Position = -60
    TabOrder = 2
    OnChange = SbAttenuationChange
  end
  object SbRecursionSteps: TScrollBar
    Left = 101
    Top = 76
    Width = 156
    Height = 17
    Max = 16
    Min = 1
    PageSize = 0
    Position = 1
    TabOrder = 3
    OnChange = SbRecursionStepsChange
  end
end
