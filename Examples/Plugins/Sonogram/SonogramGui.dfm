object FmSonogram: TFmSonogram
  Left = 351
  Top = 167
  BorderStyle = bsNone
  Caption = 'Sonogram'
  ClientHeight = 287
  ClientWidth = 272
  Color = 7373965
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  OnPaint = FormPaint
  PixelsPerInch = 96
  TextHeight = 13
  object LbFftOrder: TLabel
    Left = 8
    Top = 269
    Width = 62
    Height = 13
    Caption = 'FFT Order: 8'
    PopupMenu = PuFftOrder
    Transparent = True
  end
  object Timer: TTimer
    Interval = 30
    OnTimer = TimerTimer
    Left = 16
    Top = 16
  end
  object PuFftOrder: TPopupMenu
    Left = 48
    Top = 16
    object MiOrder6: TMenuItem
      Tag = 6
      Caption = '6 (= 64 Samples)'
      OnClick = MiOrderClick
    end
    object MiOrder7: TMenuItem
      Tag = 7
      Caption = '7 (= 128 Samples)'
      OnClick = MiOrderClick
    end
    object MiOrder8: TMenuItem
      Tag = 8
      Caption = '8 (= 256 Samples)'
      OnClick = MiOrderClick
    end
    object MiOrder9: TMenuItem
      Tag = 9
      Caption = '9 (= 512 Samples)'
      OnClick = MiOrderClick
    end
    object MiOrder10: TMenuItem
      Tag = 10
      Caption = '10 (= 1024 Samples)'
      OnClick = MiOrderClick
    end
    object MiOrder11: TMenuItem
      Tag = 11
      Caption = '11 (= 2048 Samples)'
      OnClick = MiOrderClick
    end
  end
end
