object FmTuner: TFmTuner
  Left = 320
  Top = 89
  BorderStyle = bsNone
  Caption = 'Tuner'
  ClientHeight = 95
  ClientWidth = 282
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnPaint = FormPaint
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object PBDisplay: TPaintBox
    Left = 8
    Top = 8
    Width = 266
    Height = 49
    OnPaint = PBDisplayPaint
  end
  object LbLowE: TGuiLabel
    Left = 136
    Top = 63
    Width = 18
    Height = 28
    Caption = 'E'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo4x
    ParentFont = False
    OnClick = LbNoteClick
  end
  object LbA: TGuiLabel
    Left = 160
    Top = 63
    Width = 18
    Height = 28
    Caption = 'A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo3x
    ParentFont = False
    OnClick = LbNoteClick
  end
  object LbD: TGuiLabel
    Left = 184
    Top = 63
    Width = 18
    Height = 28
    Caption = 'D'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo3x
    ParentFont = False
    OnClick = LbNoteClick
  end
  object LbG: TGuiLabel
    Left = 208
    Top = 63
    Width = 18
    Height = 28
    Caption = 'G'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo3x
    ParentFont = False
    OnClick = LbNoteClick
  end
  object LbH: TGuiLabel
    Left = 232
    Top = 63
    Width = 18
    Height = 28
    Caption = 'H'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo3x
    ParentFont = False
    OnClick = LbNoteClick
  end
  object LbE: TGuiLabel
    Left = 256
    Top = 63
    Width = 18
    Height = 28
    Caption = 'E'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -24
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    FontOversampling = fo3x
    ParentFont = False
    OnClick = LbNoteClick
  end
  object LbGuitarTuning: TGuiLabel
    Left = 8
    Top = 68
    Width = 122
    Height = 23
    Caption = 'Guitar Tuning:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -19
    Font.Name = 'Tahoma'
    Font.Style = []
    FontOversampling = fo4x
    ParentFont = False
  end
  object Timer: TTimer
    Interval = 20
    OnTimer = TimerTimer
    Left = 16
    Top = 16
  end
end