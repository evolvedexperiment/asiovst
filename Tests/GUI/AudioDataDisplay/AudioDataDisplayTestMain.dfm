object FmAudioDataDisplay: TFmAudioDataDisplay
  Left = 218
  Top = 77
  Caption = 'FmAudioDataDisplay'
  ClientHeight = 254
  ClientWidth = 280
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  OnResize = FormResize
  PixelsPerInch = 96
  TextHeight = 13
  object LbLineWidth: TLabel
    Left = 8
    Top = 176
    Width = 54
    Height = 13
    Caption = 'Line Width:'
    Transparent = True
  end
  object ADD1: TGuiAudioDataDisplay
    Left = 8
    Top = 8
    Width = 128
    Height = 64
    AudioDataCollection = ADC
    DisplayChannels = <>
    LineColor = clBlue
    LineWidth = 0
    XAxis.SampleUpper = 511
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object ADD2: TGuiAudioDataDisplay
    Left = 142
    Top = 8
    Width = 128
    Height = 64
    AntiAlias = gaaLinear2x
    AudioDataCollection = ADC
    DisplayChannels = <>
    LineColor = clLime
    LineWidth = 0
    XAxis.SampleUpper = 511
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object ADD3: TGuiAudioDataDisplay
    Left = 8
    Top = 78
    Width = 128
    Height = 64
    AntiAlias = gaaLinear3x
    AudioDataCollection = ADC
    DisplayChannels = <>
    LineColor = clRed
    LineWidth = 0
    XAxis.SampleUpper = 511
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object ADD4: TGuiAudioDataDisplay
    Left = 142
    Top = 78
    Width = 128
    Height = 64
    AntiAlias = gaaLinear4x
    AudioDataCollection = ADC
    DisplayChannels = <>
    LineColor = clYellow
    LineWidth = 0
    XAxis.SampleUpper = 511
    XAxis.FractionalLower = -0.500000000000000000
    XAxis.FractionalUpper = 0.500000000000000000
  end
  object CbTransparent: TCheckBox
    Left = 8
    Top = 148
    Width = 97
    Height = 17
    Caption = 'Transparent'
    Checked = True
    State = cbChecked
    TabOrder = 4
    OnClick = CbTransparentClick
  end
  object TbLineWidth: TTrackBar
    Left = 68
    Top = 176
    Width = 202
    Height = 19
    Max = 5
    Min = 1
    Position = 1
    TabOrder = 5
    ThumbLength = 12
    OnChange = TbLineWidthChange
  end
  object ADC: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Channel 1'
      end>
    SampleFrames = 512
    SampleRate = 44100.000000000000000000
    Left = 72
    Top = 152
  end
end
