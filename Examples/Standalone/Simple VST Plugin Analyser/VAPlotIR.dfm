object FmPlotIR: TFmPlotIR
  Left = 554
  Top = 554
  Caption = 'Impulse Resonse Plot'
  ClientHeight = 147
  ClientWidth = 387
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object Waveform: TWaveform
    Left = 0
    Top = 0
    Width = 387
    Height = 147
    Align = alClient
    PopupMenu = PUDisplay
    LineColor = clBlack
    ExplicitLeft = 56
    ExplicitTop = 32
    ExplicitWidth = 100
    ExplicitHeight = 41
  end
  object PUDisplay: TPopupMenu
    Left = 192
    Top = 24
    object MIWaveform: TMenuItem
      Caption = 'Waveform'
      Checked = True
      RadioItem = True
    end
  end
end
