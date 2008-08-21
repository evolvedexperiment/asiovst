object FmASIO: TFmASIO
  Left = 250
  Top = 188
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Demo application for ASIO-Host'
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
  object Lb_Drivername: TLabel
    Left = 7
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
  end
  object Lb_Copyright: TLabel
    Left = 87
    Top = 134
    Width = 310
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 
      '(C)opyright in 2004-2008 by  Christian Budde and Tobias Fleische' +
      'r'
    ExplicitTop = 154
  end
  object LbVolume: TLabel
    Left = 8
    Top = 44
    Width = 121
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Volume: 1,00 equals 0 dB'
    ExplicitTop = 64
  end
  object LbPanorama: TLabel
    Left = 8
    Top = 84
    Width = 61
    Height = 13
    Anchors = [akLeft, akBottom]
    Caption = 'Panorama: C'
    ExplicitTop = 104
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
  object Bt_Play: TButton
    Left = 343
    Top = 7
    Width = 121
    Height = 21
    Caption = 'Start Audio'
    Default = True
    Enabled = False
    TabOrder = 1
    OnClick = Bt_PlayClick
  end
  object SbVolume: TScrollBar
    Left = 8
    Top = 60
    Width = 462
    Height = 16
    Anchors = [akLeft, akBottom]
    Max = 100000
    PageSize = 0
    Position = 100000
    TabOrder = 2
    OnChange = SbVolumeChange
  end
  object SbPan: TScrollBar
    Left = 8
    Top = 100
    Width = 462
    Height = 16
    Anchors = [akLeft, akBottom]
    PageSize = 0
    Position = 50
    TabOrder = 3
    OnChange = SbPanChange
  end
  object ASIOHost: TASIOHost
    CanDos = []
    PreventClipping = pcDigital
    ConvertOptimizations = [coSSE]
    SelectorSupport = [assEngineVersion, assResetRequest, assBufferSizeChange, assResyncRequest, assLatenciesChanged]
    SampleRate = 44100.000000000000000000
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    Left = 300
    Top = 8
  end
end
