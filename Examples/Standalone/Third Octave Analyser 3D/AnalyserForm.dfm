object FmAnalyser: TFmAnalyser
  Left = 328
  Top = 278
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Simple ASIO Third Octave Analyser'
  ClientHeight = 326
  ClientWidth = 446
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Lb_Drivername: TLabel
    Left = 8
    Top = 12
    Width = 31
    Height = 13
    Caption = 'Driver:'
    OnClick = Lb_DrivernameClick
  end
  object Lb_Channels: TLabel
    Left = 8
    Top = 36
    Width = 77
    Height = 13
    Caption = 'Output Channel:'
  end
  object LbSpeed: TLabel
    Left = 8
    Top = 64
    Width = 34
    Height = 13
    Caption = 'Speed:'
  end
  object LbFullscale: TLabel
    Left = 206
    Top = 64
    Width = 53
    Height = 13
    Caption = 'Fullscale = '
  end
  object Lb_dB: TLabel
    Left = 323
    Top = 64
    Width = 13
    Height = 13
    Caption = 'dB'
  end
  object DriverCombo: TComboBox
    Left = 65
    Top = 7
    Width = 273
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 0
    OnChange = DriverComboChange
  end
  object Bt_CP: TButton
    Left = 347
    Top = 8
    Width = 91
    Height = 21
    Caption = 'Control Panel'
    Enabled = False
    TabOrder = 1
    OnClick = Bt_CPClick
  end
  object ChannelBox: TComboBox
    Left = 105
    Top = 32
    Width = 233
    Height = 21
    Style = csDropDownList
    ItemHeight = 13
    TabOrder = 2
  end
  object Bt_Analyse: TButton
    Left = 347
    Top = 32
    Width = 91
    Height = 50
    Caption = 'Analyse'
    Default = True
    Enabled = False
    TabOrder = 3
    OnClick = Bt_AnalyseClick
  end
  object RB_Fast: TRadioButton
    Left = 50
    Top = 63
    Width = 40
    Height = 17
    Caption = '&Fast'
    TabOrder = 4
    OnClick = RB_FastClick
  end
  object RB_Medium: TRadioButton
    Left = 92
    Top = 63
    Width = 55
    Height = 17
    Caption = '&Medium'
    Checked = True
    TabOrder = 5
    TabStop = True
    OnClick = RB_MediumClick
  end
  object RB_Slow: TRadioButton
    Left = 151
    Top = 63
    Width = 46
    Height = 17
    Caption = '&Slow'
    TabOrder = 6
    OnClick = RB_SlowClick
  end
  object SEFullscaleGain: TSpinEdit
    Left = 263
    Top = 60
    Width = 56
    Height = 22
    MaxValue = 200
    MinValue = 0
    TabOrder = 7
    Value = 0
    OnChange = SEFullscaleGainChange
  end
  object GLSceneViewer: TGLSceneViewer
    Left = 8
    Top = 96
    Width = 430
    Height = 222
    OnMouseWheel = GLSceneViewerMouseWheel
    Camera = GLCamera
    Buffer.BackgroundColor = 789774
    FieldOfView = 100.000000000000000000
    OnMouseDown = GLSceneViewerMouseDown
    OnMouseMove = GLSceneViewerMouseMove
  end
  object ASIOHost: TASIOHost
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    CanDos = []
    ConvertOptimizations = [coSSE]
    PreFillOutBuffer = bpfZero
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    SelectorSupport = [assEngineVersion, assResetRequest, assBufferSizeChange, assResyncRequest, assLatenciesChanged]
    OnBufferSwitch32 = BSDownSampled
    OnSampleRateChanged = ASIOHostSampleRateChanged
    Left = 253
    Top = 24
  end
  object Timer: TTimer
    Enabled = False
    Interval = 50
    OnTimer = TimerTimer
    Left = 281
    Top = 24
  end
  object BarGraphScene: TGLScene
    Left = 216
    Top = 160
    object GLDummyCube: TGLDummyCube
      Position.Coordinates = {000000000000803F000000000000803F}
      CubeSize = 1.000000000000000000
    end
    object GLLight: TGLLightSource
      Ambient.Color = {8716793F08AC3C3F1B2FDD3C0000803F}
      ConstAttenuation = 1.000000000000000000
      Diffuse.Color = {54E3453F9A99193FAAF1D23E0000803F}
      Position.Coordinates = {000000000000803F0000A0400000803F}
      LightStyle = lsOmni
      Specular.Color = {0000803F0000003F000000000000803F}
      SpotCutOff = 180.000000000000000000
    end
    object GLCamera: TGLCamera
      DepthOfView = 100.000000000000000000
      FocalLength = 93.140060424804690000
      Position.Coordinates = {000000000000803F0000A0400000803F}
    end
  end
end
