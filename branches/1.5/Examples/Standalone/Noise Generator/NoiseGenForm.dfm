object FormASIONoiseGenerator: TFormASIONoiseGenerator
  Left = 250
  Top = 188
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'ASIO Noise Generator'
  ClientHeight = 155
  ClientWidth = 479
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  DesignSize = (
    479
    155)
  PixelsPerInch = 96
  TextHeight = 13
  object LabelDrivername: TLabel
    Left = 7
    Top = 12
    Width = 39
    Height = 13
    Caption = 'Driver:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'MS Sans Serif'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object LabelCopyright: TLabel
    Left = 91
    Top = 134
    Width = 262
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = '(C)opyright in 2003-2013 by  Delphi ASIO && VST Project'
  end
  object LabelVolume: TLabel
    Left = 8
    Top = 44
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Volume: 1,00 equals 0 dB'
  end
  object LabelPanorama: TLabel
    Left = 8
    Top = 84
    Width = 61
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Panorama: C'
  end
  object ComboBoxDriver: TComboBox
    Left = 52
    Top = 7
    Width = 285
    Height = 21
    Style = csDropDownList
    TabOrder = 0
    OnChange = ComboBoxDriverChange
  end
  object ButtonStartStop: TButton
    Left = 343
    Top = 7
    Width = 121
    Height = 21
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = ButtonStartStopClick
  end
  object ScrollBarVolume: TScrollBar
    Left = 8
    Top = 60
    Width = 462
    Height = 16
    Anchors = [akLeft, akBottom]
    Max = 100000
    PageSize = 0
    Position = 100000
    TabOrder = 2
    OnChange = ScrollBarVolumeChange
  end
  object ScrollBarPan: TScrollBar
    Left = 8
    Top = 100
    Width = 462
    Height = 16
    Anchors = [akLeft, akBottom]
    PageSize = 0
    Position = 50
    TabOrder = 3
    OnChange = ScrollBarPanChange
  end
  object ASIOHost: TAsioHost
    AsioTime.SamplePos = 0
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    Left = 300
    Top = 8
  end
end
