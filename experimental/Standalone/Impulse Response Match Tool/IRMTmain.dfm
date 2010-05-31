object FmImpulseResponseMatchTool: TFmImpulseResponseMatchTool
  Left = 218
  Top = 77
  Caption = 'Impulse Response Match Tool'
  ClientHeight = 604
  ClientWidth = 711
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -13
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnClose = FormClose
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnShow = FormShow
  DesignSize = (
    711
    604)
  PixelsPerInch = 96
  TextHeight = 16
  object GbOriginal: TGuiGroup
    Left = 8
    Top = 8
    Width = 695
    Height = 201
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    Caption = 'Original'
    Radius = 5
    TabOrder = 0
    DesignSize = (
      695
      201)
    object AdOriginal: TGuiAudioDataDisplay
      Left = 3
      Top = 20
      Width = 689
      Height = 178
      Anchors = [akLeft, akTop, akRight, akBottom]
      AudioDataCollection = Adc
      DisplayedChannel = 0
      DisplayChannels = <>
      LineWidth = 0
      Normalize = False
      XAxis.SampleUpper = 8191
      XAxis.FractionalLower = -0.500000000000000000
      XAxis.FractionalUpper = 0.500000000000000000
    end
  end
  object GbResidual: TGuiGroup
    Left = 8
    Top = 215
    Width = 695
    Height = 201
    Anchors = [akLeft, akTop, akRight]
    AntiAlias = gaaLinear4x
    Caption = 'Residual'
    Radius = 5
    TabOrder = 1
    DesignSize = (
      695
      201)
    object AdResidual: TGuiAudioDataDisplay
      Left = 3
      Top = 20
      Width = 689
      Height = 178
      Anchors = [akLeft, akTop, akRight, akBottom]
      AudioDataCollection = Adc
      DisplayedChannel = 1
      DisplayChannels = <>
      LineWidth = 0
      Normalize = False
      XAxis.SampleUpper = 8191
      XAxis.FractionalLower = -0.500000000000000000
      XAxis.FractionalUpper = 0.500000000000000000
    end
  end
  object GbLog: TGuiGroup
    Left = 8
    Top = 422
    Width = 695
    Height = 174
    Anchors = [akLeft, akTop, akRight, akBottom]
    AntiAlias = gaaLinear4x
    Caption = 'Residual'
    Radius = 5
    TabOrder = 2
    DesignSize = (
      695
      174)
    object MeLog: TMemo
      Left = 3
      Top = 19
      Width = 689
      Height = 152
      Anchors = [akLeft, akTop, akRight, akBottom]
      Lines.Strings = (
        'Memo1')
      TabOrder = 0
    end
  end
  object MainMenu: TMainMenu
    Left = 16
    Top = 48
    object MiFile: TMenuItem
      Caption = '&File'
      object MiNew: TMenuItem
        Caption = '&Generate New'
        OnClick = MiNewClick
      end
      object MiFileOpen: TMenuItem
        Caption = '&Open...'
        OnClick = MiFileOpenClick
      end
      object MiS1: TMenuItem
        Caption = '-'
        Visible = False
      end
      object MiS2: TMenuItem
        Caption = '-'
      end
      object MiFileExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MiFileExitClick
      end
    end
    object MiMatch: TMenuItem
      Caption = 'Match'
      object MiMatchStart: TMenuItem
        Caption = 'Start'
        OnClick = MiMatchStartClick
      end
    end
  end
  object Adc: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Original'
      end
      item
        DisplayName = 'Residual'
      end>
    SampleFrames = 8192
    SampleRate = 44100.000000000000000000
    Left = 80
    Top = 48
  end
  object OdImpulseResponse: TOpenDialog
    DefaultExt = '.wav'
    Filter = 'WAVE (*.wav)|*.wav|AIFF (*.aiff)|*.aif*|AU (*.au)|*.au'
    Options = [ofHideReadOnly, ofFileMustExist, ofEnableSizing]
    Title = 'Load Audio File'
    Left = 48
    Top = 48
  end
end
