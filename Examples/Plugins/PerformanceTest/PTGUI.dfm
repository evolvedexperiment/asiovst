object FmPerformanceTest: TFmPerformanceTest
  Left = 325
  Top = 157
  BorderStyle = bsNone
  Caption = 'Performance Test'
  ClientHeight = 38
  ClientWidth = 243
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 13
  object LbPerformance: TLabel
    Left = 143
    Top = 13
    Width = 38
    Height = 13
    Caption = 'Cycles: '
  end
  object LbCycles: TLabel
    Left = 181
    Top = 13
    Width = 54
    Height = 13
    AutoSize = False
    Caption = 'ns'
  end
  object BtPatchFunctionCalls: TButton
    Left = 8
    Top = 8
    Width = 129
    Height = 25
    Caption = '&Patch Function Calls'
    TabOrder = 0
    OnClick = BtPatchFunctionCallsClick
  end
  object Timer: TTimer
    Interval = 50
    OnTimer = TimerTimer
    Left = 8
    Top = 8
  end
end
