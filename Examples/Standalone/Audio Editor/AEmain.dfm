object FmAudioEditor: TFmAudioEditor
  Left = 286
  Top = 92
  Caption = 'Simple Audio Editor'
  ClientHeight = 376
  ClientWidth = 654
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  Menu = MainMenu
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object GuiLevelMeter: TGuiLevelMeter
    Left = 592
    Top = 21
    Width = 62
    Height = 355
    Align = alRight
    RedrawInterval = 30
    BarWidthPercentage = 0.800000011920929000
    MaximumTimeFactor = 3.000000000000000000
  end
  object GuiStaticWaveform: TGuiStaticWaveform
    Left = 0
    Top = 21
    Width = 592
    Height = 355
    Align = alClient
    ExplicitLeft = 96
    ExplicitTop = 80
    ExplicitWidth = 377
    ExplicitHeight = 193
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
      object Noise1: TMenuItem
        Caption = '&Noise'
        OnClick = Noise1Click
      end
    end
    object MIProcess: TMenuItem
      Caption = '&Process'
      object MINormalize: TMenuItem
        Caption = '&Normalize'
        OnClick = MINormalizeClick
      end
    end
  end
  object ASIOHost: TASIOHost
    CanDos = []
    ConvertOptimizations = [coSSE, co3DNow]
    SelectorSupport = [assEngineVersion, assResetRequest, assBufferSizeChange, assResyncRequest, assLatenciesChanged]
    SampleRate = 44100.000000000000000000
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    Left = 40
    Top = 32
  end
end