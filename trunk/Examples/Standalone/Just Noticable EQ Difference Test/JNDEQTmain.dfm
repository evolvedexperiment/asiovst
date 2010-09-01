object FmJNDEQT: TFmJNDEQT
  Left = 329
  Top = 72
  Caption = 'Just Noticable EQ Difference Test'
  ClientHeight = 311
  ClientWidth = 328
  Color = 8620693
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnKeyPress = FormKeyPress
  OnPaint = FormPaint
  OnResize = FormResize
  OnShow = FormShow
  DesignSize = (
    328
    311)
  PixelsPerInch = 96
  TextHeight = 13
  object LbAudioFile: TGuiLabel
    Left = 34
    Top = 8
    Width = 73
    Height = 15
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Audio File:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object LbAudioFileValue: TGuiLabel
    Left = 113
    Top = 8
    Width = 207
    Height = 15
    Margins.Bottom = 0
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Pink Noise (double click to change)'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Arial'
    Font.Style = [fsItalic]
    ParentFont = False
    PopupMenu = PuAudioFile
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
    OnDblClick = LbAudioFileValueDblClick
  end
  object LbVolume: TGuiLabel
    Left = 8
    Top = 295
    Width = 42
    Height = 11
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Volume:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object LbVolumeValue: TGuiLabel
    Left = 146
    Top = 295
    Width = 56
    Height = 11
    Margins.Bottom = 0
    Alignment = taCenter
    Anchors = [akTop, akRight]
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = '0 dB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object ClipLED: TGuiLED
    Left = 299
    Top = 293
    Width = 16
    Height = 16
    BorderStrength_Percent = 80.000000000000000000
    Brightness_Percent = 100.000000000000000000
    LEDColor = clRed
    LineWidth = 1.799999952316284000
    Uniformity_Percent = 30.000001907348630000
    Transparent = False
  end
  object LbClipIndicator: TGuiLabel
    Left = 229
    Top = 295
    Width = 70
    Height = 11
    Margins.Bottom = 0
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Clip Indicator:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object LbInformation: TGuiLabel
    Left = 8
    Top = 191
    Width = 312
    Height = 11
    Margins.Bottom = 0
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    AutoSize = True
    Caption = 'Information'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -11
    Font.Name = 'Arial'
    Font.Style = []
    ParentFont = False
    Transparent = True
    Shadow.Color = 10333885
    Shadow.Visible = True
  end
  object SliderVolume: TGuiSlider
    Left = 56
    Top = 297
    Width = 84
    Height = 8
    Anchors = [akLeft, akTop, akRight]
    BorderRadius = 2
    Color = 8620693
    DefaultPosition = 3.000000000000000000
    Max = 3.000000000000000000
    Min = -30.000000000000000000
    OnChange = SliderVolumeChange
    ParentColor = False
    Position = 3.000000000000000000
    SlideColor = clBlack
    Transparent = True
  end
  object BtMedia: TGuiMediaButton
    Left = 8
    Top = 4
    Width = 20
    Height = 20
    AntiAlias = gaaLinear4x
    Transparent = True
    BorderRadius = 2
    ButtonColor = 10333885
    Color = clBtnFace
    Enabled = False
    OnClick = BtMediaClick
    ParentColor = False
  end
  object GbEQFilter: TGuiGroup
    Left = 8
    Top = 209
    Width = 312
    Height = 82
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    Caption = 'EQ Peak Filter'
    Color = 8620693
    Offset = 3
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 10333885
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    GroupColor = clBlack
    LineColor = clBlack
    OutlineWidth = 2
    PanelColor = 10333885
    ParentColor = False
    ParentFont = False
    Radius = 5
    TabOrder = 2
    DesignSize = (
      312
      82)
    object LbBandwidth: TGuiLabel
      Left = 7
      Top = 60
      Width = 55
      Height = 11
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Bandwidth:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
    end
    object LbBandwidthValue: TGuiLabel
      Left = 253
      Top = 60
      Width = 56
      Height = 11
      Margins.Bottom = 0
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = '1'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
    end
    object LbFrequency: TGuiLabel
      Left = 7
      Top = 44
      Width = 55
      Height = 11
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Frequency:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
    end
    object LbFrequencyValue: TGuiLabel
      Left = 253
      Top = 44
      Width = 56
      Height = 11
      Margins.Bottom = 0
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = '1 kHz'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
    end
    object LbGain: TGuiLabel
      Left = 7
      Top = 28
      Width = 26
      Height = 11
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Gain:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
    end
    object LbGainValue: TGuiLabel
      Left = 253
      Top = 28
      Width = 56
      Height = 11
      Margins.Bottom = 0
      Alignment = taCenter
      Anchors = [akTop, akRight]
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = '0 dB'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
    end
    object LbAutoVolumeAdj: TGuiLabel
      Left = 115
      Top = 6
      Width = 149
      Height = 11
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Automatic Volume Adjustment:'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
      OnClick = LbAutoVolumeAdjustmentClick
    end
    object LbAutoVolumeAdjValue: TGuiLabel
      Left = 270
      Top = 6
      Width = 18
      Height = 11
      Margins.Bottom = 0
      AntiAlias = gaaLinear4x
      AutoSize = True
      Caption = 'Off'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -11
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      Transparent = True
      Shadow.Color = 10333885
      Shadow.Visible = True
      OnClick = LbAutoVolumeAdjustmentClick
    end
    object SliderBandwidth: TGuiSlider
      Left = 68
      Top = 62
      Width = 179
      Height = 8
      Anchors = [akLeft, akTop, akRight]
      BorderRadius = 2
      Color = 8620693
      CurveMapping = 1.000000000000000000
      DefaultPosition = 3.000000000000000000
      Max = 10.000000000000000000
      Min = 0.100000001490116100
      OnChange = SliderBandwidthChange
      ParentColor = False
      Position = 3.000000000000000000
      SlideColor = clBlack
      Transparent = True
    end
    object SliderFrequency: TGuiSlider
      Left = 68
      Top = 46
      Width = 179
      Height = 8
      Anchors = [akLeft, akTop, akRight]
      BorderRadius = 2
      Color = 8620693
      CurveMapping = 2.000000000000000000
      DefaultPosition = 1000.000000000000000000
      Max = 20000.000000000000000000
      Min = 20.000000000000000000
      OnChange = SliderFrequencyChange
      ParentColor = False
      Position = 1000.000000000000000000
      SlideColor = clBlack
      Transparent = True
    end
    object SliderGain: TGuiSlider
      Left = 68
      Top = 30
      Width = 179
      Height = 8
      Anchors = [akLeft, akTop, akRight]
      BorderRadius = 2
      Color = 8620693
      DefaultPosition = 15.000000000000000000
      Max = 15.000000000000000000
      Min = -15.000000000000000000
      OnChange = SliderGainChange
      ParentColor = False
      Position = 15.000000000000000000
      SlideColor = clBlack
      Transparent = True
    end
  end
  object PnSelectorA: TGuiPanel
    Tag = 1
    Left = 8
    Top = 31
    Width = 100
    Height = 100
    AntiAlias = gaaLinear4x
    Caption = 'PnSelectorA'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 3
    UseDockManager = True
    OnMouseDown = LbSelectionMouseDown
    OnMouseUp = LbSelectionMouseUp
    object LbSelectionA: TGuiLabel
      Tag = 1
      Left = 25
      Top = 3
      Width = 48
      Height = 80
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'A'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -75
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnMouseDown = LbSelectionMouseDown
      OnMouseUp = LbSelectionMouseUp
    end
  end
  object PnSelectorB: TGuiPanel
    Tag = 2
    Left = 220
    Top = 31
    Width = 100
    Height = 100
    AntiAlias = gaaLinear4x
    Caption = 'GuiPanel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 4
    UseDockManager = True
    OnMouseDown = LbSelectionMouseDown
    OnMouseUp = LbSelectionMouseUp
    object LbSelectionB: TGuiLabel
      Tag = 2
      Left = 27
      Top = 3
      Width = 44
      Height = 80
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'B'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -75
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnMouseDown = LbSelectionMouseDown
      OnMouseUp = LbSelectionMouseUp
    end
  end
  object PnSelectorX: TGuiPanel
    Left = 114
    Top = 31
    Width = 100
    Height = 100
    AntiAlias = gaaLinear4x
    Caption = 'GuiPanel1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 5
    UseDockManager = True
    OnMouseDown = LbSelectionMouseDown
    object LbSelectionX: TGuiLabel
      Left = 29
      Top = 3
      Width = 40
      Height = 80
      Alignment = taCenter
      AntiAlias = gaaLinear4x
      Caption = 'X'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = 27903
      Font.Height = -75
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnMouseDown = LbSelectionMouseDown
    end
  end
  object PnSelectorXisA: TGuiPanel
    Left = 8
    Top = 137
    Width = 100
    Height = 48
    AntiAlias = gaaLinear4x
    Caption = 'PnXisA'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 6
    UseDockManager = True
    OnClick = LbXisAClick
    DesignSize = (
      100
      48)
    object LbXisA: TGuiLabel
      Left = 7
      Top = 5
      Width = 85
      Height = 38
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AntiAlias = gaaLinear4x
      Caption = 'X is A'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbXisAClick
    end
  end
  object PnSelectorXisB: TGuiPanel
    Left = 220
    Top = 137
    Width = 100
    Height = 48
    AntiAlias = gaaLinear4x
    Caption = 'PnXisB'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 7
    UseDockManager = True
    OnClick = LbXisBClick
    DesignSize = (
      100
      48)
    object LbXisB: TGuiLabel
      Left = 8
      Top = 5
      Width = 85
      Height = 38
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AntiAlias = gaaLinear4x
      Caption = 'X is B'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbXisBClick
    end
  end
  object PnSkip: TGuiPanel
    Left = 114
    Top = 137
    Width = 100
    Height = 48
    AntiAlias = gaaLinear4x
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -64
    Font.Name = 'Tahoma'
    Font.Style = []
    LineColor = clBlack
    PanelColor = 10333885
    ParentColor = True
    Radius = 5
    TabOrder = 8
    UseDockManager = True
    OnClick = LbXisAClick
    DesignSize = (
      100
      48)
    object LbSkip: TGuiLabel
      Left = 7
      Top = 5
      Width = 85
      Height = 38
      Alignment = taCenter
      Anchors = [akLeft, akTop, akRight]
      AntiAlias = gaaLinear4x
      Caption = 'Skip'
      Color = 10333885
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clBlack
      Font.Height = -32
      Font.Name = 'Tahoma'
      Font.Style = []
      ParentFont = False
      Shadow.Color = 5663873
      Shadow.Visible = True
      OnClick = LbSkipClick
    end
  end
  object MainMenu: TMainMenu
    Left = 224
    Top = 4
    object MiTest: TMenuItem
      Caption = '&Test'
      object MiTestStart: TMenuItem
        Caption = 'Start'
        OnClick = MiTestFullGainReferenceClick
      end
      object N3: TMenuItem
        Caption = '-'
      end
      object MiTestTraining: TMenuItem
        Caption = '&Training'
        RadioItem = True
        object MiTestTrainingGain: TMenuItem
          Caption = '&Gain'
          OnClick = MiTestTrainingGainClick
        end
        object MiTestTrainingFrequency: TMenuItem
          Caption = '&Frequency'
          Enabled = False
        end
        object MiTestTrainingBandwidth: TMenuItem
          Caption = '&Bandwidth'
          Enabled = False
        end
      end
      object MiTestFull: TMenuItem
        Caption = '&Full'
        RadioItem = True
        object MiTestFullGain: TMenuItem
          Caption = '&Gain'
          object MiTestFullGainReference: TMenuItem
            Caption = '&Reference (1 kHz, 1 dB/Oct)'
            OnClick = MiTestFullGainReferenceClick
          end
        end
        object MiTestFullFrequency: TMenuItem
          Caption = '&Frequency'
          Enabled = False
          object MiTestFullFrequencyReference: TMenuItem
            Caption = 'Reference'
          end
        end
        object MiTestFullBandwidth: TMenuItem
          Caption = '&Bandwidth'
          Enabled = False
          object MiTestFullBandwidthReference: TMenuItem
            Caption = 'Reference'
          end
        end
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MiDecryptJNDfile: TMenuItem
        Caption = 'Decrypt JND file...'
        OnClick = MiDecryptJNDfileClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MiExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiExitClick
      end
    end
    object MiSettings: TMenuItem
      Caption = '&Settings'
      object MiAudioSettings: TMenuItem
        Caption = '&Audio'
        OnClick = MiAudioSettingsClick
      end
      object N4: TMenuItem
        Caption = '-'
      end
      object MiLatchButtons: TMenuItem
        Caption = 'Latch Buttons'
        OnClick = MiLatchButtonsClick
      end
    end
  end
  object OD: TOpenDialog
    DefaultExt = '.mp3'
    Filter = 'MP3-File (*.mp3)|*.mp3'
    Title = 'Select an MP3 file'
    Left = 256
    Top = 4
  end
  object AsioHost: TAsioHost
    AsioTime.Speed = 1.000000000000000000
    AsioTime.SampleRate = 44100.000000000000000000
    AsioTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    PreventClipping = pcDigital
    SampleRate = 44100.000000000000000000
    OnBuffersCreate = AsioHostBuffersCreate
    OnBufferSwitch32 = AsioHostBufferSwitch32
    OnSampleRateChanged = AsioHostSampleRateChanged
    Left = 288
    Top = 4
  end
  object PeakCheck: TTimer
    Interval = 50
    OnTimer = PeakCheckTimer
    Left = 272
    Top = 152
  end
  object PuAudioFile: TPopupMenu
    Left = 192
    Top = 4
    object MiPinkNoise: TMenuItem
      Caption = '&Pink Noise'
      OnClick = MiPinkNoiseClick
    end
  end
  object OpenDialog: TOpenDialog
    DefaultExt = '.JND'
    Filter = 'Just Noticable Difference Files (*.JND)|*.jnd'
    Left = 24
    Top = 160
  end
  object ResultButtonEnabler: TTimer
    Enabled = False
    Interval = 500
    OnTimer = ResultButtonEnablerTimer
    Left = 240
    Top = 152
  end
end
