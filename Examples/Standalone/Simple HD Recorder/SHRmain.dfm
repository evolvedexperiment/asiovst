object FmSimpleHDRecorder: TFmSimpleHDRecorder
  Left = 258
  Top = 87
  BorderStyle = bsDialog
  Caption = 'Simple HD Recorder'
  ClientHeight = 72
  ClientWidth = 423
  Color = 4408131
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clBlack
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object GuiLED1: TGuiLED
    Left = 160
    Top = 8
    Width = 41
    Height = 40
    Brightness_Percent = 10.000000000000000000
    LineWidth = 2
    LEDColor = clRed
    AntiAlias = gaaLinear4x
    LineColor = clRed
  end
  object BtStartStop: TGuiButton
    Left = 8
    Top = 8
    Width = 146
    Height = 59
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    Caption = 'Start'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -32
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    LineColor = 2105376
    LineWidth = 3
    Radius = 7
    OnClick = BtStartStopClick
  end
  object GuiLED2: TGuiLED
    Left = 207
    Top = 8
    Width = 41
    Height = 40
    Brightness_Percent = 10.000000000000000000
    LineWidth = 2
    LEDColor = clRed
    AntiAlias = gaaLinear4x
    LineColor = clRed
  end
  object GuiLED3: TGuiLED
    Left = 254
    Top = 8
    Width = 41
    Height = 40
    Brightness_Percent = 10.000000000000000000
    LineWidth = 2
    LEDColor = clRed
    AntiAlias = gaaLinear4x
    LineColor = clRed
  end
  object GuiLED4: TGuiLED
    Left = 301
    Top = 8
    Width = 41
    Height = 40
    Brightness_Percent = 10.000000000000000000
    LineWidth = 2
    LEDColor = clRed
    AntiAlias = gaaLinear4x
    LineColor = clRed
  end
  object LbMic1: TGuiLabel
    Left = 160
    Top = 48
    Width = 41
    Height = 19
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Mic.1'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
  end
  object LbMic2: TGuiLabel
    Left = 207
    Top = 48
    Width = 41
    Height = 19
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Mic.2'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
  end
  object LbMic3: TGuiLabel
    Left = 254
    Top = 48
    Width = 41
    Height = 19
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Mic.3'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
  end
  object LbMic4: TGuiLabel
    Left = 301
    Top = 48
    Width = 41
    Height = 19
    Alignment = taCenter
    AntiAlias = gaaLinear4x
    Caption = 'Mic.4'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -13
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
  end
  object BtSetup: TGuiButton
    Left = 348
    Top = 8
    Width = 69
    Height = 27
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    Caption = 'Setup'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    LineColor = 2105376
    LineWidth = 3
    Radius = 5
    OnClick = BtSetupClick
  end
  object BtExit: TGuiButton
    Left = 348
    Top = 40
    Width = 69
    Height = 27
    AntiAlias = gaaLinear4x
    Alignment = taCenter
    Caption = 'Exit'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    LineColor = 2105376
    LineWidth = 3
    Radius = 5
    OnClick = BtExitClick
  end
  object Timer: TTimer
    Interval = 50
    OnTimer = TimerTimer
    Left = 216
    Top = 16
  end
  object ASIOHost: TASIOHost
    ASIOTime.Speed = 1.000000000000000000
    ASIOTime.SampleRate = 44100.000000000000000000
    ASIOTime.Flags = [atSystemTimeValid, atSamplePositionValid, atSampleRateValid, atSpeedValid]
    CanDos = []
    ConvertOptimizations = [coSSE, co3DNow]
    SampleRate = 44100.000000000000000000
    SelectorSupport = [assEngineVersion, assResetRequest, assBufferSizeChange, assResyncRequest, assLatenciesChanged]
    OnBufferSwitch32 = ASIOHostBufferSwitch32
    Left = 248
    Top = 16
  end
end
