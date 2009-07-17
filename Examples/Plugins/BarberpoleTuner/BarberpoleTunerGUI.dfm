object FmBarberpoleTuner: TFmBarberpoleTuner
  Left = 218
  Top = 77
  BorderStyle = bsNone
  Caption = 'Barberpole Tuner'
  ClientHeight = 90
  ClientWidth = 274
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
  object LbDisplay: TGuiLabel
    Left = 8
    Top = 8
    Width = 137
    Height = 17
    Alignment = taCenter
    AntiAlias = gaaLinear3x
    Caption = 'Barberpole Tuner'
    Color = clBtnFace
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Arial'
    Font.Style = [fsBold, fsUnderline]
    Transparent = True
  end
  object Barberpole: TPaintBox
    Left = 8
    Top = 31
    Width = 258
    Height = 26
    OnPaint = BarberpolePaint
  end
  object LbGuitarTuning: TGuiLabel
    Left = 8
    Top = 63
    Width = 105
    Height = 23
    AntiAlias = gaaLinear4x
    Caption = 'Guitar Tuning:'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -16
    Font.Name = 'Tahoma'
    Font.Style = []
    Transparent = True
  end
  object LbLowE: TGuiLabel
    Left = 127
    Top = 60
    Width = 18
    Height = 25
    AntiAlias = gaaLinear4x
    Caption = 'E'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbNoteClick
  end
  object LbA: TGuiLabel
    Left = 151
    Top = 60
    Width = 18
    Height = 25
    AntiAlias = gaaLinear3x
    Caption = 'A'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlack
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbNoteClick
  end
  object LbD: TGuiLabel
    Left = 175
    Top = 60
    Width = 18
    Height = 25
    AntiAlias = gaaLinear3x
    Caption = 'D'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbNoteClick
  end
  object LbG: TGuiLabel
    Left = 199
    Top = 60
    Width = 18
    Height = 25
    AntiAlias = gaaLinear3x
    Caption = 'G'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbNoteClick
  end
  object LbH: TGuiLabel
    Left = 223
    Top = 60
    Width = 18
    Height = 25
    AntiAlias = gaaLinear3x
    Caption = 'H'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbNoteClick
  end
  object LbE: TGuiLabel
    Left = 247
    Top = 60
    Width = 18
    Height = 25
    AntiAlias = gaaLinear3x
    Caption = 'E'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = 5197647
    Font.Height = -21
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    Transparent = True
    OnClick = LbNoteClick
  end
  object Timer: TTimer
    Interval = 33
    OnTimer = TimerTimer
    Left = 160
    Top = 8
  end
end
