object FmSonogram: TFmSonogram
  Left = 693
  Top = 124
  BorderStyle = bsNone
  Caption = 'Sonogram'
  ClientHeight = 272
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
  object Timer: TTimer
    Interval = 30
    OnTimer = TimerTimer
    Left = 16
    Top = 16
  end
end
