object FmAudioEditor: TFmAudioEditor
  Left = 286
  Top = 92
  Caption = 'Simple Audio Editor'
  ClientHeight = 396
  ClientWidth = 654
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object GuiLevelMeter: TGuiLevelMeter
    Left = 592
    Top = 21
    Width = 62
    Height = 375
    Align = alRight
    BarWidthPercentage = 0.800000011920928900
    MaximumTimeFactor = 3.000000000000000000
    RedrawInterval = 30
    ExplicitHeight = 355
  end
  object GuiAudioDataDisplay: TGuiAudioDataDisplay
    Left = 0
    Top = 21
    Width = 592
    Height = 375
    Align = alClient
    AntiAlias = gaaLinear2x
    AudioData = AudioDataCollection32
    ExplicitLeft = -6
  end
  object ToolBar: TToolBar
    Left = 0
    Top = 0
    Width = 654
    Height = 21
    AutoSize = True
    ButtonHeight = 21
    ButtonWidth = 21
    Caption = 'ToolBar'
    Customizable = True
    ShowCaptions = True
    TabOrder = 0
    object BtPause: TToolButton
      Left = 0
      Top = 0
      Caption = '||'
      ImageIndex = 1
    end
    object BtPlay: TToolButton
      Left = 21
      Top = 0
      Caption = ' > '
      ImageIndex = 0
      Style = tbsTextButton
    end
  end
  object MainMenu: TMainMenu
    Left = 8
    Top = 32
    object MIFile: TMenuItem
      Caption = '&File'
      object MIOpen: TMenuItem
        Caption = 'Open...'
        OnClick = MIOpenClick
      end
      object MISave: TMenuItem
        Caption = 'Save'
      end
      object MISaveAs: TMenuItem
        Caption = 'Save as...'
        OnClick = MISaveAsClick
      end
      object N2: TMenuItem
        Caption = '-'
      end
      object MISetup: TMenuItem
        Caption = '&Setup...'
        OnClick = MISetupClick
      end
      object N1: TMenuItem
        Caption = '-'
      end
      object MIExit: TMenuItem
        Caption = 'E&xit'
        OnClick = MIExitClick
      end
    end
    object MIGenerate: TMenuItem
      Caption = '&Generate'
      object MINoise: TMenuItem
        Caption = '&Noise'
        OnClick = MINoiseClick
      end
    end
    object MIProcess: TMenuItem
      Caption = '&Process'
      object MINormalize: TMenuItem
        Caption = '&Normalize'
        OnClick = MINormalizeClick
      end
      object MIRectify: TMenuItem
        Caption = '&Rectify'
        OnClick = MIRectifyClick
      end
      object MIRemoveDC: TMenuItem
        Caption = 'Remove &DC'
        OnClick = MIRemoveDCClick
      end
      object MIInvert: TMenuItem
        Caption = '&Invert'
        OnClick = MIInvertClick
      end
    end
  end
  object ASIOHost: TASIOHost
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    CanDos = []
    ConvertOptimizations = [coSSE, co3DNow]
    SampleRate = 44100.000000000000000000
    SelectorSupport = [assEngineVersion, assResetRequest, assBufferSizeChange, assResyncRequest, assLatenciesChanged]
    Left = 40
    Top = 32
  end
  object AudioDataCollection32: TAudioDataCollection32
    Channels = <
      item
        DisplayName = 'Channel 1'
      end>
    SampleFrames = 8192
    SampleRate = 44100.000000000000000000
    Left = 72
    Top = 32
  end
end
