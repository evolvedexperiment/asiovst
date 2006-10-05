object FmASIO: TFmASIO
  Left = 291
  Top = 266
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
  ClientHeight = 220
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
  PixelsPerInch = 96
  TextHeight = 13
  object Lb_Drivername: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object Lb_Channels: TLabel
    Left = 7
    Top = 36
    Width = 82
    Height = 13
    Caption = 'Output Channels:'
  end
  object Lb_Copyright: TLabel
    Left = 87
    Top = 202
    Width = 310
    Height = 13
    Caption = 
      '(C)opyright in 2004-2006 by  Christian Budde and Tobias Fleische' +
      'r'
  end
  object LbFreq: TLabel
    Left = 8
    Top = 72
    Width = 96
    Height = 13
    Caption = 'Frequency: 1000 Hz'
  end
  object LbVolume: TLabel
    Left = 8
    Top = 112
    Width = 121
    Height = 13
    Caption = 'Volume: 1,00 equals 0 dB'
  end
  object LbPanorama: TLabel
    Left = 8
    Top = 152
    Width = 61
    Height = 13
    Caption = 'Panorama: C'
  end
  object DriverCombo: TComboBox
    Left = 64
    Top = 7
    Width = 273
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object Bt_CP: TButton
    Left = 352
    Top = 7
    Width = 121
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = Bt_CPClick
  end
  object ChannelBox: TComboBox
    Left = 104
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
    OnChange = ChannelBoxChange
  end
  object Bt_Play: TButton
    Left = 352
    Top = 32
    Width = 121
    Height = 21
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = Bt_PlayClick
  end
  object SbFreq: TScrollBar
    Left = 8
    Top = 88
    Width = 462
    Height = 16
    LargeChange = 1000
    Max = 100000
    PageSize = 0
    Position = 56633
    SmallChange = 10
    TabOrder = 4
    OnChange = SbFreqChange
  end
  object SbVolume: TScrollBar
    Left = 8
    Top = 128
    Width = 462
    Height = 16
    Max = 100000
    PageSize = 0
    Position = 100000
    TabOrder = 5
    OnChange = SbVolumeChange
  end
  object SbPan: TScrollBar
    Left = 8
    Top = 168
    Width = 462
    Height = 16
    PageSize = 0
    Position = 50
    TabOrder = 6
    OnChange = SbPanChange
  end
  object ASIOHost: TASIOHost
    CanDos = []
    PreventClipping = pcDigital
    ConvertOptimizations = [coSSE]
    SampleRate = 44100.000000000000000000
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    OnSampleRateChanged = ASIOHostSampleRateChanged
    OnBufferSwitch = ASIOHostBufferSwitch
    Left = 308
    Top = 32
  end
end
